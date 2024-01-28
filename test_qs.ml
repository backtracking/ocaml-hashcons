
(** Another example involving λ-terms. This one is from

      Constructive Computation Theory
      Gérard Huet, Inria, 2011
      https://gallium.inria.fr/~huet/PUBLIC/CCT.pdf

   section 2.2 (λ-calculus as a general programming language).

   Below we run a quicksort written in λ-calculus on lists of
   Church-encoded natural numbers. The quicksort term is contained in
   the marshaled file "quicksort.term".
*)

open Hashcons

type term = term_node hash_consed
and term_node =
  | Ref of int           (* variables as reference depth *)
  | Abs of term          (* abstraction [x]t             *)
  | App of term * term   (* application (t u)            *)

module Term = Hashcons.Make(
  struct
    type t = term_node
    let equal t1 t2 = match t1, t2 with
      | Ref i, Ref j -> i == j
      | Abs u, Abs v -> u == v
      | App (u1,v1), App (u2,v2) -> u1 == u2 && v1 == v2
      | _ -> false
    let hash = function
      | Ref i -> i
      | Abs t -> (19 * t.hkey + 1)
      | App (u,v) -> (19 * (19 * u.hkey + v.hkey) + 2)
  end)
let ht = Term.create 10007
let ref i = Term.hashcons ht (Ref i)
let abs t = Term.hashcons ht (Abs t)
let app (u,v) = Term.hashcons ht (App (u,v))

let memo f =
  let h = Hashtbl.create 251 in
  fun x ->
    try Hashtbl.find h x.tag
    with Not_found -> let y = f x in Hashtbl.add h x.tag y; y
let memo2_int_term f =
  let h = Hashtbl.create 251 in
  fun x y ->
    try Hashtbl.find h (x, y.tag)
    with Not_found -> let z = f x y in Hashtbl.add h (x, y.tag) z; z
let memo2_term_term f =
  let h = Hashtbl.create 251 in
  fun x y ->
    try Hashtbl.find h (x.tag, y.tag)
    with Not_found -> let z = f x y in Hashtbl.add h (x.tag, y.tag) z; z

let lift n =
  let rec lift_rec k =
    let rec lift_k t = match t.node with
    | Ref i ->
	if i<k then t    (* bound variables are invariant     *)
	else ref(n+i)    (* free variables are relocated by n *)
    | Abs t   -> abs (lift_rec (k+1) t)
    | App (t, u) -> app (lift_k t, lift_k u)
    in
    lift_k
  in
  lift_rec 0

let lift = memo2_int_term lift

let subst_count = Stdlib.ref 0

let subst w =
  incr subst_count;
  let rec subst_w n t = match t.node with
    | Ref k ->
	if k=n then lift n w  (* substituted variable *)
	else if k<n then t    (* bound variables *)
        else ref (k-1)        (* free variables *)
    | Abs t   -> abs (subst_w (n+1) t)
    | App (t, u) -> app (subst_w n t, subst_w n u)
  in
  subst_w 0

let subst = memo2_term_term subst

let rec hnf t = match t.node with
  | Ref n -> t
  | Abs t   -> abs (hnf t)
  | App (t, u) -> match hnf t with
      | {node=Abs w} -> hnf (subst u w)
      | h     -> app (h, u)

let nhf = memo hnf

let rec nf t = match t.node with
  | Ref n -> t
  | Abs t   -> abs (nf t)
  | App (t, u) -> match hnf t with
      | {node=Abs w}  -> nf (subst u w)
      | h      -> app (nf h, nf u)

let nf = memo nf

type expr = Ref2 of int | Abs2 of expr | App2 of expr * expr

let rec term_of_expr = function
  | Ref2 i -> ref i
  | Abs2 t -> abs (term_of_expr t)
  | App2 (u,v) -> app (term_of_expr u, term_of_expr v)

let quicksort =
  let c = open_in "quicksort.term" in
  let e = (input_value c : expr) in
  close_in c;
  term_of_expr e

let nil = (*[c,n]n*) abs (abs (ref 0))
let cons = (*[x,l][c,n](c x (l c n))*)
  abs(abs(abs(abs(app(app (ref 1,
			   ref 3),
		      app (app (ref 2,
				ref 1),
			   ref 0))))))

let zero = (*[s,z]z*) abs (abs (ref 0))
let succ = (*[n][s,z](s (n s z))*)
  abs(abs(abs(app (ref 1,
		   app (app (ref 2, ref 1), ref 0)))))

let rec iter f n x = if n=0 then x else iter f (n-1) (f x)

(* Church *)
let church n = iter (fun c -> nf (app (succ, c))) n zero

(* list : int list -> term *)
let rec list = function
  | x :: l ->
      let cx = church x and ll = list l in
      (*[c,n](c ^Cx (^Ll c n))*)
      abs(abs(app (app (ref 1, cx),
		   app (app (ll, ref 1), ref 0))))
  | []   -> nil

(* and back *)

let eval_nat iter init = function
  | {node=Abs {node=Abs t}} (* [s,z]t *) ->
      let rec eval_rec = function
        | (* z *) {node=Ref 0} -> init
        | (* (s u) *) {node=App ({node=Ref 1}, u)} -> iter (eval_rec u)
        | _ -> failwith "Not a normal church natural"
        in
	eval_rec t
  | _ -> failwith "Not a normal church natural"

let compute_nat = eval_nat (fun n->n+1) 0

let normal_nat n = compute_nat (nf n)

let eval_list_of_nats = function
  | {node=Abs {node=Abs t}} (* [c,n]t *) ->
      let rec lrec = function
        | (* n *)       {node=Ref 0}                   -> []
        | (* (c x l) *) {node=App ({node=App ({node=Ref 1}, x)}, l)} ->
	    (compute_nat x) :: (lrec l)
        | _ -> failwith "Not a normal List"
      in
      lrec t
  | _ -> failwith "Not a normal List"

let normal_list l = eval_list_of_nats (nf l)

open Format

let () =
  let l = list [0;3;5;2;4;1] in
  assert (normal_list (app (quicksort, l)) = [0;1;2;3;4;5]);
  printf "subst count: %d@." !subst_count;
  let stat = Gc.stat () in
  printf "top heap words: %d (%d kb)@." stat.Gc.top_heap_words
    (stat.Gc.top_heap_words / 256);
  let l,n,s,b1,b2,b3 = Term.stats ht in
  printf "table length: %d / nb. entries: %d / sum of bucket length: %d@."
    l n s;
  printf "smallest bucket: %d / median bucket: %d / biggest bucket: %d@."
    b1 b2 b3
