(* https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem *)
open Ppxlib

let extension =
  Extension.declare_inline
    "effect"
    Ppxlib.Extension.Context.Structure_item
    Ast_pattern.(single_expr_payload (pexp_construct __ __))
    (* The type for Ast_pattern is defined as ('a, 'b, 'c) t where 'a is the type of AST nodes that are matched, 'b is the type of the values you're extracting from the node as a function type and 'c is the return type of that last function. *)
    (fun ~loc ~path l ->
      (* Ast_builder.Default.psig *)
      (* Parsetree.PStr (Pty)
        structure_item_desc = Some;
        pstr_loc = loc;
      }) *)
      (* let builder = Ast_builder.make loc in
       *)
    )

let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation ~rules:[rule] "my_transformation"