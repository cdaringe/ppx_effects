import type { Task, Tasks } from "./.rad/common.ts";
import { tasks as dbTasks } from "./.rad/db.ts";
import { tasks as opamTasks } from "./.rad/opam.ts";
import { deploy } from "./.rad/deploy.ts";
import { format } from "./.rad/format.ts";

const build = `dune build`;
const duneExec = "opam exec -- dune exec";

const startAgent: Task = `${duneExec} bin/Agent.exe -- --poll-duration 10`;

const test: Task = {
  fn: async ({ logger, sh }) => {
    logger.info(`generating OCaml via ppx`);
    await sh(
      `dune exec ./test/pp.exe -- --impl ./test/test.ml -o ./test/test.actual.ml`,
    );
    logger.info(`formatting`);
    await sh(`rad format &> /dev/null`).catch(() => {
      /* ocamlformat expected to error */
    });
    await sh(`rad format`); // should pass 2nd time
    logger.info(`running test for diffing`);
    await sh(`dune runtest`);
    await sh(`dune exec test/test.exe`);
  },
};

// run `rad --list` to see all tasks
export const tasks: Tasks = {
  ...{ b: build, build },
  ...{ startAgent, sa: startAgent },
  ...{ format, f: format },
  ...opamTasks,
  ...dbTasks,
  deploy,
  ...{ t: test, test },
};
