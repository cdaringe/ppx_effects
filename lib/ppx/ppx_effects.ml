(* https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem *)
open Ppxlib
open Ast_builder.Default

let effect_keyword_extension =
  Extension.declare "effect" Ppxlib.Extension.Context.Structure_item
    Ast_pattern.(
      pstr
        (pstr_eval
           (pexp_tuple
              (pexp_construct __ none ^:: pexp_ident __ ^:: pexp_ident __
             ^:: nil))
           nil
        ^:: nil))
    (fun ~loc ~path:_ effect_construct input_type output_type ->
      let eff_name =
        match effect_construct with
        | Lident z -> z
        | _ -> failwith "you shouldn't be using this"
      in
      let effsym = ptyp_constr ~loc (Loc.make ~loc (Lident "eff")) in
      pstr_typext ~loc
      @@ type_extension ~loc
           ~path:(Loc.make ~loc @@ Lident "eff") (* ~params:(ptyp_any ~loc) *)
           ~params:[ (ptyp_any ~loc, (NoVariance, Injective)) ]
           ~constructors:
             [
               extension_constructor ~loc ~name:(Loc.make ~loc eff_name)
                 ~kind:
                   (Pext_decl
                      (* super wrong ... i need to recurse on input & output types of the Pext_decl and generate correct ... stuff *)
                      ( Pcstr_tuple
                          [ ptyp_constr ~loc (Loc.make ~loc input_type) [] ],
                        Some
                          (effsym
                             [ ptyp_constr ~loc (Loc.make ~loc output_type) [] ])
                      ));
             ]
           ~private_:Public)

let loca ~loc v = Loc.make ~loc v

let try_with_ident ~loc =
  loca ~loc @@ Ppxlib.Longident.parse "Obj.Effect_handlers.Deep.try_with"

let continue_ident ~loc =
  loca ~loc @@ Ppxlib.Longident.parse "Obj.Effect_handlers.Deep.continue"

let continuation_ident ~loc =
  loca ~loc @@ Ppxlib.Longident.parse "Obj.Effect_handlers.Deep.continuation"

let invoke_try_with_effects ~loc ~comp ~arg ~handler_record =
  let try_with = pexp_ident ~loc (try_with_ident ~loc) in
  pexp_apply ~loc try_with
    [ (Nolabel, comp); (Nolabel, arg); (Nolabel, handler_record) ]

let some ~loc expr = pexp_construct ~loc (loca ~loc (lident "Some")) expr

let none ~loc = pexp_construct ~loc (loca ~loc (lident "None")) None

let lident_l_of_str ~loc v = loca ~loc (Longident.Lident v)

(*
Convert the ppx syntax: 
  (E1 handle_e1) 

Into:
  E1 x -> Some (fun (k : (a, _) continuation) -> handle_e1 x continue k) 
  *)
let to_effect_handler_match_case ~loc (handler : longident * label) =
  let eff_constructor_lident, handler_name = handler in
  let lhs_ident = loca ~loc eff_constructor_lident in
  let lhs =
    ppat_construct ~loc lhs_ident (Some (ppat_var ~loc (loca ~loc "x")))
  in
  (* (k : (a, _) *)
  let a_constr = ptyp_constr ~loc (lident_l_of_str ~loc "a") [] in
  let rhs_constraint =
    ppat_constraint ~loc
      (ppat_var ~loc (loca ~loc "k"))
      (ptyp_constr ~loc (continuation_ident ~loc) [ a_constr; ptyp_any ~loc ])
  in
  let rhs_apply_fn =
    pexp_apply ~loc
      (pexp_ident ~loc @@ lident_l_of_str ~loc handler_name)
      [
        (Nolabel, pexp_ident ~loc (lident_l_of_str ~loc "x"));
        (Nolabel, pexp_ident ~loc (continue_ident ~loc));
        (Nolabel, pexp_ident ~loc (lident_l_of_str ~loc "k"));
      ]
  in
  let rhs_lambda = pexp_fun ~loc Nolabel None rhs_constraint rhs_apply_fn in
  let rhs = some ~loc @@ Some rhs_lambda in
  case ~lhs ~guard:None ~rhs

let case_match_none ~loc =
  case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(none ~loc)

let simple_record = None

type handler_list = (longident * label) list

let to_effc_handler_record ~loc (handlers : handler_list) =
  let match_effect_cases =
    List.map (to_effect_handler_match_case ~loc) handlers
    @ [ case_match_none ~loc ]
  in
  let match_arg = pexp_ident ~loc @@ lident_l_of_str ~loc "e" in
  let match_effects = pexp_match ~loc match_arg match_effect_cases in
  let effc_fn =
    pexp_newtype ~loc (loca ~loc "a")
    @@ pexp_fun ~loc Nolabel None
         (ppat_constraint ~loc
            (ppat_var ~loc (loca ~loc "e"))
            (ptyp_constr ~loc
               (lident_l_of_str ~loc "eff")
               [ ptyp_constr ~loc (lident_l_of_str ~loc "a") [] ]))
         match_effects
  in
  let effc_entry = (lident_l_of_str ~loc "effc", effc_fn) in
  pexp_record ~loc [ effc_entry ] simple_record

let extract_eff_and_handler = function
  | {
      pexp_desc =
        Parsetree.Pexp_construct
          ( { txt = effect_name; _ },
            Some { pexp_desc = Pexp_ident { txt = Lident handler_name; _ }; _ }
          );
      _;
    } ->
      (effect_name, handler_name)
  | _ -> failwith "invalid hanler entry"

let with_effects_keyword_extension =
  Extension.declare "with_effects" Ppxlib.Extension.Context.Expression
    Ast_pattern.(pstr (pstr_eval (pexp_apply __ __) nil ^:: nil))
    (fun ~loc ~path:_ comp params ->
      let arg, handlers =
        match params with
        | [
         (Nolabel, arg);
         (Nolabel, { pexp_desc = Pexp_array handler_pexp_constructs; _ });
        ] ->
            (arg, List.map extract_eff_and_handler handler_pexp_constructs)
        | _ ->
          Location.raise_errorf ~loc "Unsupported expression %a"
            (Pprintast.expression)
            (Ppxlib.Ast_builder.Default.pexp_apply ~loc comp params)
      in
      invoke_try_with_effects ~loc ~comp ~arg
        ~handler_record:(to_effc_handler_record ~loc handlers))

let list_split_last h tl =
  let rec loop last acc = function
    | [] -> last, List.rev acc
    | h :: t -> loop h (last :: acc) t in
  loop h [] tl

(*
   converts
     | E (..., k) -> exp
   into
     | (E (...) : a eff) -> Some (fun (k : (a, _) continuation) -> exp)
*)
let convert_case_to_effect_handler ~loc = function
  | { pc_lhs={
    ppat_desc=Ppat_construct (name, Some({ppat_desc=Ppat_tuple (p_h :: p_tl);_} as args));
    _ } as pc_lhs;
    pc_guard; pc_rhs } ->
    let a_constr = ptyp_constr ~loc (lident_l_of_str ~loc "a") [] in
    let kont_pat, pats = list_split_last p_h p_tl in
    let kont_pat =
      Ast_builder.Default.ppat_constraint ~loc kont_pat
      (ptyp_constr ~loc (continuation_ident ~loc) [ a_constr; ptyp_any ~loc ])
    in
    let args = match pats with
      | [pat] -> pat
      | _ -> {args with ppat_desc = Ppat_tuple pats} in
    let pc_lhs = [%pat? ([%p {pc_lhs with ppat_desc = Ppat_construct (name, Some args)}] : a eff)] in
    let pc_rhs = [%expr Some (fun [%p kont_pat] -> [%e pc_rhs])] in
    {pc_lhs; pc_guard; pc_rhs}
  | v ->
    Location.raise_errorf ~loc "Use of unsupported pattern %a" Pprintast.pattern v.pc_lhs

let effect_try_extension =
  Extension.declare "effect" Ppxlib.Extension.Context.Expression
    Ast_pattern.(pstr (pstr_eval (pexp_try __ __) nil ^:: nil)) begin
    fun ~loc ~path:_ comp cases ->
      let updated_cases = List.map (convert_case_to_effect_handler ~loc) cases in
      let default_case = Ast_builder.Default.(case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr None]) in
      let cases = updated_cases @ [default_case] in
      let comp = [%expr fun () -> [%e comp]] in
      let arg = [%expr ()] in

      let effect_handler =
        let a_ident = ({txt="a";loc})  in
        Ast_builder.Default.(pexp_newtype ~loc a_ident)
        (Ast_builder.Default.pexp_function ~loc cases)
      in
      invoke_try_with_effects
        ~loc ~comp ~arg ~handler_record:([%expr { effc = [%e effect_handler] } ])
  end

let effect_rule = Context_free.Rule.extension effect_keyword_extension

let with_effects_rules =
  [Context_free.Rule.extension with_effects_keyword_extension;
   Context_free.Rule.extension effect_try_extension ]

let () =
  Driver.register_transformation
    ~rules:( effect_rule :: with_effects_rules )
    "effects"
