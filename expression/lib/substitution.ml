open Ast

(* Module for finding matchings and dealing with substitutions *)

let empty = []
let singleton x y = [(x,y)]

let rec find x s = match s with
  | [] -> failwith ("Not found (find was passed a list that does not contain "^ x ^" as a key)")
  | (k,v) :: rm
    -> if x = k then v else find x rm

let rec compatible s1 s2 = match s1 with
  | [] -> true
  | (k,v) :: rest ->
      (match List.assoc_opt k s2 with
        | Some v' -> v = v' && compatible rest s2
        | None -> compatible rest s2)
let merge s1 s2 = if compatible s1 s2 then Some (s1 @ s2) else None

let rec match_expression variables pattern expression =
  match pattern with
    | Identifier x -> if List.mem x variables then
        Some (singleton x expression)
        else
        (if pattern = expression then Some empty else None)
    | Application (p1, p2) -> (* x has to be an application too, otherwise return None *)
        (match expression with
            | Application (e1, e2) ->
                (* We recursively match the sub-expressions.
                    This part is much easier to write if e2 is an expression (and it is for this particular ast),
                    because it's so symmetrical *)
                (match match_expression variables p1 e1, match_expression variables p2 e2 with
                | Some s1, Some s2 -> merge s1 s2
                | _ -> None)
            | _ -> None)
    | _ -> None


let rec substitute variables s e = match e with
    | Identifier var -> if List.mem var variables
                        then find var s
                        else e
    | Application (e1, e2) -> Application (substitute variables s e1, substitute variables s e2)
    | _ -> e
