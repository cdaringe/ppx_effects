open Obj.Effect_handlers

type !_ eff += E1 : string -> unit eff

type !_ eff += E2 : unit -> unit eff

type !_ eff += E3 : string -> string eff

let comp () = ()

let fn () =
  let handle_e1 _unused_string continue k = continue k () in
  let handle_e2 _ continue k = continue k () in
  let handle_e3 s continue k = continue k s in
  Obj.Effect_handlers.Deep.(
    try_with comp ()
      {
        effc =
          (fun (type a) (e : a eff) ->
            match e with
            | E1 x ->
                Some (fun (k : (a, _) continuation) -> handle_e1 x continue k)
            | E2 x ->
                Some (fun (k : (a, _) continuation) -> handle_e2 x continue k)
            | E3 x ->
                Some (fun (k : (a, _) continuation) -> handle_e3 x continue k)
            | _ -> None);
      });
  ()
