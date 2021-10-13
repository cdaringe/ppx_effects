open Obj.Effect_handlers

[%%effect E1, string, unit]

[%%effect E2, unit, unit]

[%%effect E3, string, string]

let comp () = ()

let fn () =
  let handle_e1 _unused_string continue k = continue k () in
  let handle_e2 _ continue k = continue k () in
  let handle_e3 s continue k = continue k s in
  [%with_effects comp () [| E1 handle_e1; E2 handle_e2; E3 handle_e3 |]]
