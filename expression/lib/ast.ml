type argument = string * string


type expression =
  | Identifier of string
  | Application of expression * expression
  | Equality of expression * expression
  | Hint of string
  | Let of string * argument list * expression
  | LetProve of string * argument list * expression
  | LetRec of string * argument list * expression
