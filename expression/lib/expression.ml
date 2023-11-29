include Ast
(* include Substitution *)

module Parser = Parser
module Lexer = Lexer

let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast

let string_of_declaration = String_of.string_of_declaration

let empty = []
let singleton x y = [(x, y)]
(* let merge s1 s2 = match s1 with
| [] -> Some s2 (* this case is actually correct, but it shouldn't 
                    be needed once the other one is fixed *)
| _ -> Some s1 (* todo: fix *) *)

let rec merge (s1 : (string * expression) list) (s2 : (string * expression) list) = 
  let rec helper k1 v1 s2 =
    match s2 with
    | (k2, v2) :: tl -> if k1 = k2 && v1 != v2 then None else helper k1 v1 tl
    | [] -> Some (k1, v1)
    (* todo: fail when two different values for a key *)
  in match s1 with
  | (k, v) :: tl -> (
    match helper k v s2 with
    | Some kv -> merge tl (kv :: s2)
    | None -> None)
  | [] -> Some s2

let rec is_variable (a: string) (vars : typedVariable list) : bool =
  match vars with
  | TypedVariable(nm, _) :: tl -> a = nm || is_variable a tl
  | _ -> false
let rec match_expression (vars : typedVariable list) (pattern : expression) (exp : expression) : 'a option =
  match pattern with
    | Identifier x -> if is_variable x vars then (* Todo: fix test for variables: the string "x" is not necessarily the only variable and it might not always be a variable either *)
        (* if x is a variable: *)
        (* let _ = print_endline ("var " ^ x) in *)
        Some (singleton x exp)
        else
        (* if x is a constant: *)
        (* let _ = print_endline ("con " ^ x) in *)
        (if pattern = exp then Some empty else None)
    | Application (p1, p2) -> (* x has to be an application too, otherwise return None *)
        (match exp with
            | Application (e1, e2) ->
                (* We recursively match the sub-expressions.
                    This part is much easier to write if e2 is an expression (and it is for this particular ast),
                    because it's so symmetrical *)
                (match match_expression vars p1 e1, match_expression vars p2 e2 with
                (* | Some s1, Some s2 -> merge s2 s1 *)
                | Some s1, Some s2 -> merge s1 s2
                | _ -> None)
            | _ -> None)
    | _ -> None

let rec find x subs = match subs with
  (* | [] -> failwith "Not found (find was passed an empty list)"
  | [(k,v)] -> if x = k then v else failwith "Not found (find failed but the substitution being passed in really does not contain the variable)"
  | _ -> failwith "Find failed (key not found because this part of the find function is utterly broken)" *)
  | (k, v) :: tl -> if x = k then v else find x tl
  (* | [] -> let _ = print_endline x in failwith "no" *)
  | [] -> Identifier x

let rec substitute variables s e = match e with
    | Identifier v -> find v s
    (* | Identifier "x" -> find "x" s *)
    | Application (e1, e2) -> Application (substitute variables s e1, substitute variables s e2)
    | _ -> e

(* Given a list of variables, an equality to apply, and an expression to
   apply it to, returns the result to applying the equality to the expression
   (if possible) *)
let rec attempt_rewrite (vars : typedVariable list) (lhs : expression) (rhs : expression) (exp: expression) : expression option =
    match match_expression vars lhs exp with
    | Some s -> Some (substitute vars s rhs)
    | None -> (match exp with
        | Application (e1, e2) -> (
            match attempt_rewrite vars lhs rhs e1 with
              (* todo: trying the other side! *)
            | None -> (
              match attempt_rewrite vars lhs rhs e2 with
              | None -> None
              | Some e2' -> Some (Application (e1, e2')))
            | Some e1' -> Some (Application (e1', e2)))
        | _ -> None (* not succesful *)
        )
  

(* Given an expression, tries to apply each equality in the list. Returns the
   name of the rule that was successfully applied and the resulting
   expression 
      returns the pair (eqn, expr') where eqn is the name of the equation that
          applied and expr' is the rewritten expression
      Simply calls the attemptRewrite function for each of the equalities *)
let rec perform_step (exp: expression) (steps: (string * typedVariable list * expression * expression) list) : (string * expression) option =
  match steps with
  | (name, vars, lhs, rhs) :: tl -> (match attempt_rewrite vars lhs rhs exp with
      | Some expr -> Some (name, expr)
      | None -> perform_step exp tl)
  | [] -> None

let rec perform_steps (exp: expression) (steps: (string * typedVariable list * expression * expression) list) : (string * expression) list =
  (match perform_step exp steps with
  | Some (s, exp2) -> (s, exp2) :: perform_steps exp2 steps
  | None -> [])

let rec prove_rhs rules (lhs : expression) (rhs : expression) =
  (match perform_steps rhs rules with
  | (nm, e) :: _ -> prove_rhs rules lhs e @ [" = { " ^ nm ^ " }"; String_of.string_of_expression rhs]
  | [] -> if lhs = rhs then [] else [" = { ??? }"; String_of.string_of_expression rhs])

let rec prove rules (lhs : expression) (rhs : expression) =
  String_of.string_of_expression lhs :: 
  (match perform_steps lhs rules with
    | (nm, e) :: _ -> (" = { " ^ nm ^ " }") :: prove rules e rhs
    | [] -> if lhs = rhs then [] else prove_rhs rules lhs rhs)
     
let rec prover rules declarations =
  match declarations with
  | ProofDeclaration (nm, vars, Equality(lhs, rhs), None) :: rest
    -> (* no hint, so let's prove this *)
        prove rules lhs rhs :: prover ((nm, vars, lhs, rhs)::rules) rest
  | ProofDeclaration (nm, vars, Equality(lhs, rhs), _) :: rest
    -> (* we got a hint so we simply assume the statement *)
        prover ((nm, vars, lhs, rhs)::rules) rest
  | _ :: rest -> prover rules rest
  | [] -> []

let prover_main (declarations : declaration list) =
  prover [] declarations |>
  List.map (String.concat "\n") |>
  String.concat "\n\n" |>
  print_endline