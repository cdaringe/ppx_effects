# ppx_effects

Allows one to write effects via concise syntax via ppx:

```ml
let comp = (* perform (A input) *)
let () =
  let handle_a s continue k = continue k () in
  let handle_b _ continue k = continue k ()
  [%with_effects comp () [| A handle_a; B handle_b; |]]
```

## install

🚨 not yet published to opam

`opam install ppx_effects`

## usage

- add `ppx_effects` to your preprocessors, a la

```dune
(executable
 (name my_bin)
 ;...snip...;
 (preprocess
  (pps ppx_effects)))
```

- use `[%with_effects <array-syntax>]` where each entry is of form `Effect_constructor handler_name`

## warning

this is ppx is WIP. use with caution
