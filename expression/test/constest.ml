let (*prove*) bee (h : int) (i : int)
 = (dong h h = jee h)
 (*hint: axiom *)

let (*prove*) zee (h : int)
 = (dong 0 h = h)

let (*prove*) shouldbreak (h : int)
 = (dong h h = jee h)

let (*prove*) shouldbreak (h : int)
 = (dong h i = jee h)

let (*prove*) shouldbreak (h : int)
 = (dong 0 (dong h h) = jee h)

let (*prove*) shouldbread (h : int)
 = (dong (dong h h) (dong h h) = dong (jee h) (jee h))
