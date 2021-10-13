(* https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem *)
open Ppxlib
open Ast_builder.Default

(* type z = Parsetree.t
   type l = Longident.t *)

(* let expr_to_core = function
   | Ptyp_any -> ptyp_any
   | Ptyp_var v -> ptyp_var v
   | Ptyp_arrow (a,b,c) -> ptyp_arrow a b c
   | Ptyp_tuple ctl -> ptyp_tuple ctl
   | Ptyp_constr (a,b) -> ptyp_constr a b
   | Ptyp_object (object_field_list, closed_flag) -> ptyp_object object_field_list closed_flag
   | Ptyp_class (lloc, ctl) -> ptyp_class lloc ctl
   | Ptyp_alias (ct, s) -> ptyp_alias ct s
   | Ptyp_variant (a,b,c) -> ptyp_variant a b c
   | Ptyp_poly (sll, ct) -> ptyp_poly sll ct
   | Ptyp_package pt -> ptyp_package pt
   | Ptyp_extension extension -> ptyp_extension extension *)

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
        match effect_construct with Lident z -> z | _ -> failwith "uhoh"
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

let var_try_with_private ~loc =
  ppat_var ~loc (loca ~loc "__ppx_effects_try_with")

let var_continue_private ~loc =
  ppat_var ~loc (loca ~loc "__ppx_effects_continue")

let assign_continue ~loc expr =
  pexp_let ~loc Nonrecursive
    [
      value_binding ~loc
        ~pat:(var_continue_private ~loc)
        ~expr:(pexp_ident ~loc (continue_ident ~loc));
    ]
    expr

let assign_try_with ~loc expr =
  pexp_let ~loc Nonrecursive
    [
      value_binding ~loc
        ~pat:(var_try_with_private ~loc)
        ~expr:(pexp_ident ~loc (try_with_ident ~loc));
    ]
    expr

let with_effects_refs ~loc expr =
  expr |> assign_continue ~loc |> assign_try_with ~loc

let invoke_try_with_effects ~loc ~(comp) ~arg ~handler_record =
  let try_with = pexp_ident ~loc (try_with_ident ~loc) in
  with_effects_refs ~loc
  @@ pexp_apply ~loc try_with
       [ (Nolabel, comp); (Nolabel, arg); (Nolabel, handler_record) ]

let to_handler_record_kv ~loc handler = 1
let non_with_record = None
let to_effc_handler_record ~loc handlers = pexp_record ~loc (List.map (to_handler_record_kv ~loc) handlers) non_with_record

let extract_eff_and_handler = function
  | {
      pexp_desc = Parsetree.Pexp_construct (
        {txt = effect_name; _; },
        Some {
          pexp_desc = Pexp_ident {txt = Lident handler; _ };
          _; 
        }
      );
      _;
   } -> effect_name, handler
  | _ -> failwith "invalid hanler entry"
let with_effects_keyword_extension =
  Extension.declare "with_effects" Ppxlib.Extension.Context.Expression
    Ast_pattern.(pstr (pstr_eval (pexp_apply (pexp_ident __) __) nil ^:: nil))
    (fun ~loc ~path:_ comp params ->
      let (arg, handlers) = match params with
        | [(Nolabel, {
          pexp_desc = Pexp_construct (arg, None);
          _
          });(Nolabel, {pexp_desc =
          Pexp_array handler_pexp_constructs; _ })] -> (arg, List.map extract_eff_and_handler handler_pexp_constructs)
        | _ -> failwith "supported args: <how to pretty print AST programatically?>" (* Ppxlib_ast.Pprintast.expression *)   in
      let open_in_effects_handlers_deep expr =
        let ed_mod_ident =
          pmod_ident ~loc (loca ~loc @@ lident "Obj.Effect_handlers.Deep")
        in
        pexp_open ~loc (open_infos ~loc ~expr:ed_mod_ident ~override:Fresh) expr
      in
      invoke_try_with_effects ~loc ~comp ~arg ~handler_record:(to_effc_handler_record ~loc handlers)

let effect_rule = Context_free.Rule.extension effect_keyword_extension

let with_effects_rules =
  Context_free.Rule.extension with_effects_keyword_extension

let () =
  Driver.register_transformation
    ~rules:[ effect_rule; with_effects_rules ]
    "effect"
