open Obj.Effect_handlers

[%%effect A, string, unit]

[%%effect B, unit, unit]

[%%effect C, string, string]

let comp on_complete =
  perform @@ B ();
  let c_out = perform @@ C "c_input" in
  perform @@ A c_out;
  on_complete ()

let () =
  let handle_a s continue k =
    print_endline @@ "handling a with: " ^ s;
    continue k ()
  in
  let handle_b _ continue k =
    print_endline "handling b";
    continue k ()
  in
  let handle_c s continue k =
    print_endline @@ "handling c with: " ^ s;
    continue k "c_output"
  in
  let on_complete () = print_endline "all_done!" in
  [%with_effects comp on_complete [| A handle_a; B handle_b; C handle_c |]]
  
let () =
  let handle_a s k =
    print_endline @@ "handling a with: " ^ s;
    Obj.Effect_handlers.Deep.continue k ()
  in
  let handle_b _ k =
    print_endline "handling b";
    Obj.Effect_handlers.Deep.continue k ()
  in
  let handle_c s k =
    print_endline @@ "handling c with: " ^ s;
    Obj.Effect_handlers.Deep.continue k "c_output"
  in
  let on_complete () = print_endline "all_done!" in
  try%effect comp on_complete with
  | A (s, k) -> handle_a s k
  | B (s, k) -> handle_b s k
  | C (s, k) -> handle_b s k
