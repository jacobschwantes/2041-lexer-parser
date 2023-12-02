let (*prove*) bee (h : int)
 = (dong h (dong h) = dong h)
 (*hint: axiom *)

let (*prove*) bee2 (h : int)
 = (dong h (dong h) = dong h)

let (*prove*) baz (h : int)
 = (dong h (dong h (dong h)) = dong h)

 let (*prove*) foo (h : int)
  = (gra x x = x)
  (*hint: axiom *)

 let (*prove*) bau (h : int)
  = (gra x (gra x x) = x)

 let (*prove*) bau (x : int)
  = (gra x x = x)

 let (*prove*) bae (x : int)
  = (gra x y = x)