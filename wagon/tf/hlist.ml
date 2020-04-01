module HList = struct
  type _ hlist =
  | HNil  : unit hlist
  | HCons : ('a * 'b hlist) -> ('a * 'b) hlist
  let empty = HNil
  let cons = fun a b -> HCons(a,b)
  let hlend = empty
  let (@,) = cons
end
module HListIndex = struct
  open HList
  let _0 = fun (HCons(a, _)) -> a
  let _1 = fun (HCons(_, HCons(a, _))) -> a
  let _2 = fun (HCons(_, HCons(_, HCons(a, _)))) -> a
  let _3 = fun (HCons(_, HCons(_, HCons(_, HCons(a, _))))) -> a
  let _4 = fun (HCons(_, HCons(_, HCons(_, HCons(_, HCons(a, _)))))) -> a
  let _5 = fun (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(a, _))))))) -> a
  let _6 = fun (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(a, _)))))))) -> a
  let _7 = fun (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(a, _))))))))) -> a
  let _8 = fun (HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(_, HCons(a, _)))))))))) -> a
end

module WHList = struct
  type _ whlist =
  | WHNil  : unit whlist
  | WHCons : (('a * string) * ('b whlist)) -> (('a * string) * 'b) whlist
  let empty = WHNil
  let cons = fun a b -> WHCons(a, b)
end
let rec iter_all =
fun x ->
match x with
| WHNil(_) -> ""
| WHCons((f,s), b) -> (snd f) ^ (iter_all b)