open Obj.Effect_handlers

[%%effect A, string, unit]

[%%effect B, unit, unit]

[%%effect C, string, string]

let comp () =
  perform @@ B ();
  let c_out = perform @@ C "c_input" in
  perform @@ A c_out;
  ()

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
  [%with_effects comp () [| A handle_a; B handle_b; C handle_c |]]
