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
        | _ ->
            failwith
              "effect extension is WIP. send a patch :) only simple types \
               supported"
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
  loca ~loc @@ Ppxlib.Longident.parse "Effect_handlers.Deep.try_with"

let continue_ident ~loc =
  loca ~loc @@ Ppxlib.Longident.parse "Effect_handlers.Deep.continue"

let continuation_ident ~loc =
  loca ~loc @@ Ppxlib.Longident.parse "Effect_handlers.Deep.continuation"

let invoke_try_with_effects ~loc ~comp ~arg ~handler_record =
  let try_with = pexp_ident ~loc (try_with_ident ~loc) in
  pexp_apply ~loc try_with
    [ (Nolabel, comp); (Nolabel, arg); (Nolabel, handler_record) ]

let lident_l_of_str ~loc v = loca ~loc (Longident.Lident v)

let list_split_last h tl =
  let rec loop last acc = function
    | [] -> (last, List.rev acc)
    | h :: t -> loop h (last :: acc) t
  in
  loop h [] tl

(*
   converts
     | E (..., k) -> exp
   into
     | (E (...) : a eff) -> Some (fun (k : (a, _) continuation) -> exp)
*)
let convert_case_to_effect_handler ~loc = function
  | {
      pc_lhs =
        {
          ppat_desc =
            Ppat_construct
              (name, Some ({ ppat_desc = Ppat_tuple (p_h :: p_tl); _ } as args));
          _;
        } as pc_lhs;
      pc_guard;
      pc_rhs;
    } ->
      let a_constr = ptyp_constr ~loc (lident_l_of_str ~loc "a") [] in
      let kont_pat, pats = list_split_last p_h p_tl in
      let kont_pat =
        Ast_builder.Default.ppat_constraint ~loc kont_pat
          (ptyp_constr ~loc (continuation_ident ~loc)
             [ a_constr; ptyp_any ~loc ])
      in
      let args =
        match pats with
        | [ pat ] -> pat
        | _ -> { args with ppat_desc = Ppat_tuple pats }
      in
      let pc_lhs =
        [%pat?
          ([%p { pc_lhs with ppat_desc = Ppat_construct (name, Some args) }] :
            a eff)]
      in
      let pc_rhs = [%expr Some (fun [%p kont_pat] -> [%e pc_rhs])] in
      { pc_lhs; pc_guard; pc_rhs }
  | v ->
      Location.raise_errorf ~loc "Use of unsupported pattern %a"
        Pprintast.pattern v.pc_lhs

let effect_try_extension =
  Extension.declare "effect" Ppxlib.Extension.Context.Expression
    Ast_pattern.(pstr (pstr_eval (pexp_try __ __) nil ^:: nil))
    (fun ~loc ~path:_ comp cases ->
      let updated_cases =
        List.map (convert_case_to_effect_handler ~loc) cases
      in
      let default_case =
        Ast_builder.Default.(
          case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr None])
      in
      let cases = updated_cases @ [ default_case ] in
      let comp = [%expr fun () -> [%e comp]] in
      let arg = [%expr ()] in

      let effect_handler =
        let a_ident = { txt = "a"; loc } in
        Ast_builder.Default.(pexp_newtype ~loc a_ident)
          (Ast_builder.Default.pexp_function ~loc cases)
      in
      invoke_try_with_effects ~loc ~comp ~arg
        ~handler_record:[%expr { effc = [%e effect_handler] }])

let effect_rule = Context_free.Rule.extension effect_keyword_extension

let try_effects_rule = Context_free.Rule.extension effect_try_extension

let () =
  Driver.register_transformation
    ~rules:[ effect_rule; try_effects_rule ]
    "effects"
