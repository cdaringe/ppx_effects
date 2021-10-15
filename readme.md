# ppx_effects

Allows one to write effects via concise syntax via ppx:

```ml
try%effect comp on_complete with
  | A (input_a, k) -> handle_a input_a k
  | B (input_b, k) -> handle_b input_b k
  | C (input_c, k) -> handle_c input_c k
```

See a full example in [test/test.ml](./test/test.ml).

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
