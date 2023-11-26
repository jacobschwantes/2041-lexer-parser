type argument = string * string

type typevariant = Variant of (string * string list)
type typedVariable = TypedVariable of (string * string)

type expression =
| Identifier of string
| Application of expression * expression
(* | Equality of expression * expression *)
| Match of expression * (expression * expression) list
| Tuple of expression * expression

type equality = 
| Equality of expression * expression

type hint = Axiom



(* type declaration = 
  | Hint of string
  | Let of string * argument list * expression
  | LetProve of string * argument list * expression
  | LetRec of string * argument list * expression *)
type declaration
  = Declaration of (string * typedVariable list * equality * hint option)

(* type statement = Declaration of declaration | Equality of equality | Expression of expression *)