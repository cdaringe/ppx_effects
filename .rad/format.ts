import type { Task } from "./common.ts";

export const format: Task = {
  async fn({ sh }) {
    const cmds = [
      `deno fmt rad.ts ui/src .rad`,
      `opam exec -- dune build @fmt --auto-promote`,
    ];
    await Promise.all(cmds.map((cmd) => sh(cmd)));
  },
};

export const formatCheck: Task = {
  async fn({ sh }) {
    const cmds = [
      `deno fmt --check rad.ts ui/src .rad`,
      `opam exec -- dune build @fmt`,
    ];
    await Promise.all(cmds.map((cmd) => sh(cmd)));
  },
};
