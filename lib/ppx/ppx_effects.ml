(* https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem *)
open Ppxlib
open Ast_builder.Default

let extension =
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

let rule = Context_free.Rule.extension extension

let () = Driver.register_transformation ~rules:[ rule ] "effect"
