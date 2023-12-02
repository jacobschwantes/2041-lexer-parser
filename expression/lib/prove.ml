(* Module for writing simple proofs *)

let rec attempt_rewrite variables lhs rhs expression =
  match Substitution.match_expression variables lhs expression with
    | Some s -> Some (Substitution.substitute variables s rhs)
    | None -> (match expression with
        | Application (e1, e2) -> (match attempt_rewrite variables lhs rhs e2 with
            | None -> 
              (match attempt_rewrite variables lhs rhs e1 with
                | None -> None
                | Some e1' -> Some (Application (e1', e2)))
            | Some e2' -> Some (Application (e1, e2')))
        | _ -> None (* not succesful *)
        )

let rec perform_step rules expression = match rules with
  | (variables, nm, lhs, rhs) :: rest -> (match attempt_rewrite variables lhs rhs expression with
      | Some e -> Some (nm, e)
      | _ -> perform_step rest expression)
  | [] -> None

let rec perform_steps rules expression
 = match perform_step rules expression with
  | Some (nm, e) -> (nm, e) :: perform_steps rules e
  | None -> []

let print_step e nm = [String_of.string_of_expression e; " = { " ^ nm ^ " }"]
let rec print_steps_n e steps n =
      match (steps, n) with
      | (_, 0) -> []
      | ((nm, e') :: rest, _) ->
        print_step e nm @ print_steps_n e' rest (n-1)
      | _ -> failwith "print_steps_n: n is larger than the number of steps"

let rec lockstep lhs rhs lhs_steps rhs_steps =
  if lhs = rhs then [String_of.string_of_expression lhs]
  else
  match (lhs_steps, rhs_steps) with
  | ((nm1, e1) :: rest1, (nm2, e2) :: rest2) ->
    print_step lhs nm1 @ lockstep e1 e2 rest1 rest2 @ List.rev (print_step rhs nm2)
  | (_ , _) ->
    [String_of.string_of_expression lhs; " = { ??? }"
    ; String_of.string_of_expression rhs]
let rec take_away_steps e steps n =
  match (steps, n) with
  | (_, 0) -> (e, steps)
  | ((_ , e') :: rest, _) ->
    take_away_steps e' rest (n-1)
  | _ -> failwith "take_away_steps: n is larger than the number of steps"

let prove rules lhs rhs
  = let lhs_steps = perform_steps rules lhs in
    let rhs_steps = perform_steps rules rhs in
    let lhs_n = List.length lhs_steps in
    let rhs_n = List.length rhs_steps in
    if lhs_n > rhs_n then
      (let (lhs',lhs_steps') = take_away_steps lhs lhs_steps (lhs_n - rhs_n) in
      print_steps_n lhs lhs_steps (lhs_n - rhs_n) @ 
      lockstep lhs' rhs lhs_steps' rhs_steps
      )
      else
      (let (rhs',rhs_steps') = take_away_steps rhs rhs_steps (rhs_n - lhs_n) in
      lockstep lhs rhs' lhs_steps rhs_steps' @
      List.rev (print_steps_n rhs rhs_steps (rhs_n - lhs_n))
      )