open Obj.Effect_handlers

type !_ eff += A : string -> unit eff

type !_ eff += B : unit -> unit eff

type !_ eff += C : string -> string eff

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
  Obj.Effect_handlers.Deep.try_with comp on_complete
    {
      effc =
        (fun (type a) (e : a eff) ->
          match e with
          | A x ->
              Some
                (fun (k : (a, _) Obj.Effect_handlers.Deep.continuation) ->
                  handle_a x Obj.Effect_handlers.Deep.continue k)
          | B x ->
              Some
                (fun (k : (a, _) Obj.Effect_handlers.Deep.continuation) ->
                  handle_b x Obj.Effect_handlers.Deep.continue k)
          | C x ->
              Some
                (fun (k : (a, _) Obj.Effect_handlers.Deep.continuation) ->
                  handle_c x Obj.Effect_handlers.Deep.continue k)
          | _ -> None);
    }
