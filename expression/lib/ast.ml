type argument = string * string

type expression =
  | Identifier of string
  | Application of expression * expression
  | Equality of expression * expression
  (* | Hint of string *)
  | Let of string * argument list * expression
