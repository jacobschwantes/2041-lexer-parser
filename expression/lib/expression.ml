include Ast

module Parser = Parser
module Lexer = Lexer

let parse (s : string) : expression =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expression_eof Lexer.token lexbuf in
     ast

let string_of_declaration = String_of.string_of_declaration

let rec mkConstructorApp expr variantvars = match variantvars with
  | [] -> expr
  | [var, _] -> Application (expr, Identifier var)
  | (var, _)::rest ->
    Application (expr, mkConstructorApp (Application (Identifier ",", Identifier var)) rest)

let rec get_variant_namedvars variantnm variantvars i =
   match variantvars with
   | _typenm :: tl -> ("?" ^ (string_of_int i), variantnm) :: get_variant_namedvars variantnm tl (i+1)
   | [] -> []

let rec string_of_vnv vnv =
   match vnv with
   | (s1, s2) :: tl -> "(" ^ s1 ^ ", " ^ s2 ^ "), " ^ string_of_vnv tl
   | [] -> "]"

(* (variables, nm, lhs, rhs) *)
(* make a rule that's the IH *)
(* substitute lhs/rhs with proper type and do thing *)
let rec generate_ihs vars lhs rhs variant_namedvars : (string list * string * expression * expression) list =
   let _ = print_endline ("[" ^ (string_of_vnv variant_namedvars)) in
   match variant_namedvars with
   (* | (nm, typenm) :: tl -> (vars, "", Substitution.substitute vars [] lhs, Substitution.substitute vars [] rhs) :: generate_ihs vars lhs rhs tl *)
   | (_nm, _typenm) :: tl -> generate_ihs vars lhs rhs tl
   | [] -> []
   (* failwith "i'm stupid" *)

let caseproof vars varnm _typenm rules lhs rhs (variantnm, (variantvars: string list)) =
(* let variant_namedvars = [("?1","int");("?2","list")] (* TODO: construct this from variantvars *) in *)
let variant_namedvars = get_variant_namedvars variantnm variantvars 1 in
   let variant_expr = mkConstructorApp (Identifier variantnm) variant_namedvars in
   (* variables??? *)
   let caserule = ([], "case", Identifier varnm, variant_expr) in
(* let ihs = [] (* TODO: generate the inductive hypotheses for all variant_namedvars that are of type typenm *) in *)
(* lhs = rhs, but substitute variables with important variant *)
let ihs = generate_ihs vars lhs rhs variant_namedvars in
   let variantrules = caserule::ihs @ rules in
   (* todo: also print variant members 
      (e.g. "Case Cons (x, y):" instead of "Case Cons:")*)
   ("Case "^ variantnm ^ ":") ::
   List.map (fun (vars, nm, lhs, rhs) -> nm^": "^String.concat "" (List.map (fun x -> "Forall "^x^". ") vars)^String_of.string_of_expression lhs^" = "^String_of.string_of_expression rhs) ihs @
   Prove.prove variantrules lhs rhs @ ["This completes the proof of case "^ variantnm ^ ".";""]

let rec find_var_type varnm vars =
   match vars with
   | TypedVariable(nm, tp) :: tl -> if varnm = nm then tp else find_var_type varnm tl
   | [] -> failwith ("could not find type for " ^ varnm)

let rec get_type_variants typenm types : typevariant list =
   match types with
   | (nm, variants) :: tl -> if typenm = nm then variants else get_type_variants typenm tl
   | [] -> failwith ("could not find variants for " ^ typenm)

let rec variants_as_thing types =
   match types with
   | Variant(nm, variants) :: tl -> (nm, variants) :: variants_as_thing tl
   | [] -> []

let inductionproof varnm (types: (string * typevariant list) list) vars rules lhs rhs =
   (* get type of var to do induction on *)
   let typenm = find_var_type varnm vars in
   let variants = variants_as_thing (get_type_variants typenm types) in
   (* let typenm = "list" in *)
   (* find the type in 'types' and convert it to this *)
   (* let variants = [("Cons",["int";"list"]) ; ("Nil" , [])] in *)
   ("Proof by induction on (" ^ varnm ^ " : "^ typenm^") :") ::
   List.(concat (map (caseproof vars varnm typenm rules lhs rhs) variants)) @
   ["This completes the proof by induction."]

let build_fndef fnm vars =
   List.fold_left (fun acc var -> Application(acc, var)) (Identifier (fnm)) (List.map (fun var -> Identifier (var)) vars)

let rec prover (types : (string * typevariant list) list) rules declarations =
      match declarations with
         (* todo: add proven definitions as lemmas instead of rules *)
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), None) :: rest
            -> (* no hint, so let's prove this *)
               Prove.prove rules lhs rhs :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), Some Axiom) :: rest
            -> (* we got an axiom hint so we simply assume the statement *)
               prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), Some (Induction varnm)) :: rest
            -> (* we got an induction hint so we do that for the statement *)
               inductionproof varnm types vars rules lhs rhs :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | TypeDeclaration (nm, variants) :: rest
            -> (* add the type to the list of types and keep going *)
            prover ((nm, variants)::types) rules rest
         | FunctionDeclaration (fnm, vars, fnbody) :: rest -> (* add function definition as a rule *)
            let TypedVariable(nm, _) = fnm in
            let varnames = List.map typedVariableVariable vars in
            let fndef = build_fndef nm varnames in
            (* todo: match a case instead of getting stuck with the definition *)
            let _ = print_endline ("fndef: " ^ (String_of.string_of_expression fndef) ^ " = " ^ (String_of.string_of_expression fnbody)) in
            prover types ((varnames, nm ^ " definition", fndef, fnbody) :: rules) rest
            (* prover types (rules) rest *)
         | [] -> []


let prover_main decls =
      prover [] [] decls |>
      List.map (String.concat "\n") |>
      String.concat "\n\n" |>
      print_endline

