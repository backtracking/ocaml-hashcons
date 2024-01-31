(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Rule 110
    See https://en.wikipedia.org/wiki/Rule_110

    This code adapts Gosper's Hashlife algorithm for one dimension.
    (I proposed this as problem B at SWERC 2020-21; see https://swerc.eu/2020
    Unfortunately, no team solved it.)
*)

open Format
open Hashcons

type cell = node hash_consed
and node =
  | Null
  | Node of {
      size: int;
      bits: int; (* = 2 bits if size = 0, or count otherwise *)
      left: cell; right: cell;
    }
(*
  Note: we'd prefer to build `null` as

    let rec null = { uid = -1; size = 0; bits = 0; left = null; right = null; }

  but we can't do that because type `hash_consed` is private.
  So we use a sum type with two constructors (which somewhat messes
  the code below). *)

module Node = struct
  type t = node
  let hash = function
    | Null -> 0
    | Node { bits; left; right; _ } -> bits + 19 * (left.tag + 31 * right.tag)
  let equal x y = match x,y with
    | Null, Null -> true
    | Node x, Node y ->
        x.size = y.size && x.bits = y.bits &&
        x.left == y.left && x.right == y.right
    | _ -> false
end
module H = Hashcons.Make(Node)

let cells = H.create 1_000_007
let null = H.hashcons cells Null
let level0 = Array.init 4 (fun bits ->
  H.hashcons cells (Node { size = 0; bits; left = null; right = null; }))

let pop = [| 0; 1; 1; 2 |]
let count c = match c.node with
  | Null -> assert false
  | Node c -> if c.size = 0 then pop.(c.bits) else c.bits

let size c = match c.node with Null -> assert false | Node c -> c.size
let bits c = match c.node with Null -> assert false | Node c -> c.bits
let getleft  c = match c.node with Null -> assert false | Node c -> c.left
let getright c = match c.node with Null -> assert false | Node c -> c.right

let make left right = match left.node, right.node with
  | Node l, Node r ->
    let n = l.size in
    assert (n = r.size);
    let bits = count left + count right in
    let c = Node { size = n+1; left; right; bits } in
    H.hashcons cells c
  | _ -> assert false

module Cell1 = struct
  type t = cell
  let hash c = c.hkey
  let equal = (==)
end
module H1 = Hashtbl.Make(Cell1)

let results : cell H1.t = H1.create 5003

(*
current pattern                 111 110 101 100 011 010 001 000
new state for center cell 	 0   0   0   1   1   1   1   0
*)
let bit r i = (r lsr i) land 1
let rule r = Array.init 8 (bit r)
let rule = rule 110

let (++) l r = level0.((l lsl 1) lor r)

(* advance 2^(c.size - 1) steps in the future *)
let rec result c =
  try H1.find results c
  with Not_found -> let r = compute_result c.node in H1.add results c r; r

and compute_result = function
  | Node {size=n; left; right; _} ->
  assert (n >= 1);
  if n = 1 then
    let b1 = rule.((bits left lsl 1) lor ((bits right lsr 1))) in
    let b0 = rule.(((bits left land 1) lsl 2) lor bits right) in
    b1 ++ b0
  else
    let l = result left in
    let r = result right in
    let mid = result (make (getright left) (getleft right)) in
    make (result (make l mid)) (result (make mid r))
  | Null -> assert false

let () = at_exit (fun () ->
  let l,n,s,b1,b2,b3 = H.stats cells in
  printf "table length: %d / nb. entries: %d / sum of bucket length: %d@."
    l n s;
  printf "smallest bucket: %d / median bucket: %d / biggest bucket: %d@."
    b1 b2 b3;
  printf "%d results@." (H1.length results);
)

let futures = Hashtbl.create 17

let lof c = assert (size c = 0); bits c lsr 1
let rof c = assert (size c = 0); bits c land 1

(* advance 2^s steps in the future, with 0 <= s <= c.size - 1 *)
let rec future s c =
  let h =
    try Hashtbl.find futures s
    with Not_found -> let h = H1.create 5003 in Hashtbl.add futures s h; h
  in
  try H1.find h c
  with Not_found -> let r = compute_future s c in H1.add h c r; r

and compute_future s c = match c.node with
  | Node {size=n; left;right; _} ->
  assert (0 <= s && s <= n - 1);
  if s = n - 1 then
    result c
  else if n = 2 then (* then s=0 *)
    let m = rof (getright left) ++ lof (getleft right) in
    make (future s (make (rof (getleft left) ++ lof (getright left)) m))
         (future s (make m (rof (getleft right) ++ lof (getright right))))
  else
    let m = make (getright (getright left)) (getleft (getleft right)) in
    make
      (future s (make (make (getright (getleft left))  (getleft (getright left))) m))
      (future s (make m (make (getright (getleft right)) (getleft (getright right)))))
  | _ -> assert false

let memo ff =
  let h = Hashtbl.create 8192 in
  let rec f x =
    try Hashtbl.find h x
    with Not_found -> let v = ff f x in Hashtbl.add h x v; v
  in
  f

let empty = memo (fun empty n ->
  assert (n >= 0);
  if n = 0 then level0.(0) else let c = empty (n-1) in make c c)

let enlarge c =
  let e = empty (size c - 1) in make (make e (getleft c)) (make (getright c) e)
let rec makeitbig c =
  if size c >= 70 then c else makeitbig (enlarge c)

(* advance x steps in the future, by decomposing x in base 2 *)
let steps x c =
  let rec loop s x c =
    if x = 0 then c
    else loop (s + 1) (x / 2)
      (enlarge (if x mod 2 = 1 then future s c else c)) in
  loop 0 x c

let of_string s =
  let n = String.length s in
  assert ((n land (-n) == n)); (* n is a power of 2 *)
  assert (n >= 2);
  let rec build lo hi =
    if lo = hi - 2 then
      level0.((if s.[lo  ] = '1' then 2 else 0) lor
              (if s.[lo+1] = '1' then 1 else 0))
    else
      let mid = lo + (hi - lo) / 2 in
      make (build lo mid) (build mid hi) in
  makeitbig (build 0 n)

(* a few tests *)

let test ?(size=32) s n b =
  if Sys.word_size >= size then (
    let c = of_string s in
    let c = steps (int_of_string n) c in
    assert (bits c = int_of_string b)
  )
let () = test "0000000000000000" "1" "0"
let () = test "1111111111111111" "1" "3"
let () = test "0010000001010100" "0" "4"
let () = test "0100011101011100" "1000" "595"
let () = test "1010100010111101" "1000000" "591649"
let () = test ~size:64 "1010100010111101" "1152921504606846975" "682111393702695301"
