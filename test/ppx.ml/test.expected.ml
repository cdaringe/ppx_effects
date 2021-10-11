open Obj.Effect_handlers

type _ eff +=  
  | E1: string -> unit eff