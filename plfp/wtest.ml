type _ hlist =
| HNil  : unit hlist
| HCons : ('a * 'b hlist) -> ('a * 'b) hlist

let list = [%hlist ]
type l1 = [%wl ]

let rec nth_0 vl =
match vl with
| HCons(a,_) -> a
let rec nth_1 vl =
match vl with
| HCons(_,HCons(a,_)) -> a
let rec nth_2 vl =
match vl with
| HCons(_,HCons(_,HCons(a,_))) -> a

let rec nth vl n =
match vl with 
| HNil -> if 