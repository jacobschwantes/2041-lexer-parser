let (*prove*) rev_rev (x : list) = (reverse (reverse (x)) = x)
(*hint: axiom *)
let (*prove*) rev4 (x : list) = (reverse (reverse (reverse (reverse (x))))
= x)
let rec append_assoc (l1 : list) (l2 : list) (l3 : list) = (append
(append (l1) (l2)) (l3) = append (l1) (append (l2) (l3)))
(*hint: induction l1 *)
let (*prove*) rev_append (l1 : list) (l2 : list) = (reverse (append (l1)
(l2)) = append (reverse (l2)) (reverse (l1)))
(*hint: induction l1 *)
