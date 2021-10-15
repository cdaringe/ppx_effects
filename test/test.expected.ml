open EffectHandlers

type !_ eff += A : string -> unit eff

type !_ eff += B : unit -> unit eff

type !_ eff += C : string -> string eff

let comp on_complete =
  perform @@ B ();
  let c_out = perform @@ C "c_input" in
  perform @@ A c_out;
  on_complete ()

let () =
  let handle_a s k =
    print_endline @@ "handling a with: " ^ s;
    EffectHandlers.Deep.continue k ()
  in
  let handle_b _ k =
    print_endline "handling b";
    EffectHandlers.Deep.continue k ()
  in
  let handle_c s k =
    print_endline @@ "handling c with: " ^ s;
    EffectHandlers.Deep.continue k "c_output"
  in
  let on_complete () = print_endline "all_done!" in
  EffectHandlers.Deep.try_with
    (fun () -> comp on_complete)
    ()
    {
      effc =
        (fun (type a) -> function
          | (A s : a eff) ->
              Some
                (fun (k : (a, _) EffectHandlers.Deep.continuation) ->
                  handle_a s k)
          | (B s : a eff) ->
              Some
                (fun (k : (a, _) EffectHandlers.Deep.continuation) ->
                  handle_b s k)
          | (C s : a eff) ->
              Some
                (fun (k : (a, _) EffectHandlers.Deep.continuation) ->
                  handle_c s k)
          | _ -> None);
    }
