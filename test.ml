
open Hashcons

(* a quick demo of Hashcons using lambda-terms *)

type node =
  | Var of string
  | App of term * term
  | Lam of string * term
and term = node hash_consed

(* the key here is to make a O(1) equal and hash functions, making use of
   the fact that sub-terms are already hash-consed and thus we can
   1. use == on sub-terms to implement equal
   2. use .tag from sub-terms to implement hash *)
module X = struct
  type t = node
  let equal t1 t2 = match t1, t2 with
    | Var s1, Var s2 -> s1 = s2
    | App (t11, t12), App (t21, t22) -> t11 == t21 && t12 == t22
    | Lam (s1, t1), Lam (s2, t2) -> s1 = s2 && t1 == t2
    | _ -> false
  let hash = function
    | Var s -> Hashtbl.hash s
    | App (t1, t2) -> t1.tag * 19 + t2.tag
    | Lam (s, t) -> Hashtbl.hash s * 19 + t.tag
end
module H = Make(X)

let ht = H.create 17
let var s = H.hashcons ht (Var s)
let app t1 t2 = H.hashcons ht (App (t1,t2))
let lam s t = H.hashcons ht (Lam (s,t))

let x = var "x"
let delta = lam "x" (app x x)
let omega = app delta delta

let () = assert (var "x" == x)
let () = assert (app x x == app x x)

(* y = \f. (\x. f (\y. x x y)) (\x. f (\y. x x y)) *)
let y =
  let d = lam "x" (app (var "f") (lam "y" (app (app x x) (var "y")))) in
  lam "f" (app d d)

let s = Hset.add y (Hset.add delta (Hset.add omega Hset.empty))
let () = assert (Hset.mem delta s)
let () = assert (not (Hset.mem x s))
let () = assert (Hset.equal s s)
let s = Hset.add (var "x") s
let () = assert (Hset.mem x s)

let m = Hmap.add y 0 (Hmap.add delta 1 (Hmap.add omega 2 Hmap.empty))
let () = assert (Hmap.find delta m = 1)
let () = assert (Hmap.find omega m = 2)
let () = assert (Hmap.find (app delta delta) m = 2)
let () = assert (Hmap.equal (==) m m)


