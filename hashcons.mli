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

(*s Hash tables for hash consing.

    The technique is described in this paper:
      Sylvain Conchon and Jean-Christophe Filliâtre.
      Type-Safe Modular Hash-Consing.
      In ACM SIGPLAN Workshop on ML, Portland, Oregon, September 2006.
      https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf

    Note: a different, more elaborated hash-consing library
          can be found in Why3 sources at http://why3.lri.fr/

    Hash consed values are of the
    following type [hash_consed]. The field [tag] contains a unique
    integer (for values hash consed with the same table). The field
    [hkey] contains the hash key of the value (without modulo) for
    possible use in other hash tables (and internally when hash
    consing tables are resized). The field [node] contains the value
    itself.

    Hash consing tables are using weak pointers, so that values that are no
    more referenced from anywhere else can be erased by the GC. *)

type +'a hash_consed = private {
  hkey: int;
  tag : int;
  node: 'a;
}

(*s Generic part, using ocaml generic equality and hash function. *)

type 'a t

val create : int -> 'a t
  (** [create n] creates an empty table of initial size [n]. The table
      will grow as needed. *)

val clear : 'a t -> unit
  (** Removes all elements from the table. *)

val hashcons : 'a t -> 'a -> 'a hash_consed
  (** [hashcons t n] hash-cons the value [n] using table [t] i.e. returns
      any existing value in [t] equal to [n], if any; otherwise, allocates
      a new one hash-consed value of node [n] and returns it.
      As a consequence the returned value is physically equal to
      any equal value already hash-consed using table [t]. *)

val iter : ('a hash_consed -> unit) -> 'a t -> unit
  (** [iter f t] iterates [f] over all elements of [t]. *)

val stats : 'a t -> int * int * int * int * int * int
  (** Return statistics on the table.  The numbers are, in order:
      table length, number of entries, sum of bucket lengths,
      smallest bucket length, median bucket length, biggest bucket length. *)

(*s Functorial interface. *)

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type key
    type t
    val create : int -> t
    val clear : t -> unit
    val hashcons : t -> key -> key hash_consed
    val iter : (key hash_consed -> unit) -> t -> unit
    val stats : t -> int * int * int * int * int * int
  end

module Make(H : HashedType) : (S with type key = H.t)


module Hmap : sig
  type (+'a, +!'b) t
  type 'a key = 'a hash_consed

  val empty : ('a, 'b) t
  val add : 'a key -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : 'a key -> ('a, 'b) t -> 'b
  val remove : 'a key -> ('a, 'b) t -> ('a, 'b) t
  val mem :  'a key -> ('a, 'b) t -> bool
  val iter : ('a key -> 'b -> unit) -> ('a, 'b) t -> unit
  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val mapi : ('a key -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val fold : ('a key -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
end

module Hset : sig
  type 'a t
  type 'a elt = 'a hash_consed
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a elt -> 'a t -> bool
  val add : 'a elt -> 'a t -> 'a t
  val singleton : 'a elt -> 'a t
  val remove : 'a elt -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val subset : 'a t -> 'a t -> bool
  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val equal : 'a t -> 'a t -> bool
  val compare : 'a t -> 'a t -> int
  val choose : 'a t -> 'a elt
  val choose_opt : 'a t -> 'a elt option
  val cardinal : 'a t -> int
  val for_all : ('a elt -> bool) -> 'a t -> bool
  val exists : ('a elt -> bool) -> 'a t -> bool
  val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t
  val disjoint : 'a t -> 'a t -> bool
  val find : 'a elt -> 'a t -> 'a elt
  val find_opt :  'a elt -> 'a t -> 'a elt option
  val add_seq : 'a elt Seq.t -> 'a t -> 'a t
  val of_seq : 'a elt Seq.t -> 'a t
  val of_list : 'a elt list -> 'a t
  val split : 'a elt -> 'a t -> 'a t * bool * 'a t

  (*s Warning: [iter], [fold], [map], [filter] and [map_filter] do NOT iterate
      over element order. Similarly, [elements] and [to_seq] are not sorted. *)
  val iter : ('a elt -> unit) -> 'a t -> unit
  val fold : ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a elt -> 'b elt) -> 'a t -> 'b t
  val filter : ('a elt -> bool) -> 'a t -> 'a t
  val filter_map : ('a elt -> 'b elt option) -> 'a t -> 'b t
  val elements : 'a t -> 'a elt list
  val to_seq : 'a t -> 'a elt Seq.t

  (*s Warning: [min_elt], [max_elt] and the [_opt] versions are linear w.r.t.
      the size of the set. In other words, [min_elt t] is barely more efficient
      than [fold min t (choose t)]. *)
  val min_elt : 'a t -> 'a elt
  val min_elt_opt : 'a t -> 'a elt option
  val max_elt : 'a t -> 'a elt
  val max_elt_opt : 'a t -> 'a elt option

  (*s [find_first], [find_last] are linear time and can call [f] an arbitrary
      number of times, and not necessarily on elements smaller/larger
      than the witness. *)
  val find_first : ('a elt -> bool) -> 'a t -> 'a elt
  val find_first_opt : ('a elt -> bool) -> 'a t -> 'a elt option
  val find_last : ('a elt -> bool) -> 'a t -> 'a elt
  val find_last_opt : ('a elt -> bool) -> 'a t -> 'a elt option

  (*s Additional functions not appearing in the signature [Set.S] from ocaml
      standard library. *)

  (* [intersect u v] determines if sets [u] and [v] have a non-empty
     intersection. *)
  val intersect : 'a t -> 'a t -> bool

  (* Faster finds when order doesn't matter *)
  val find_any : ('a elt -> bool) -> 'a t -> 'a elt
  val find_any_opt : ('a elt -> bool) -> 'a t -> 'a elt option

  val is_singleton : 'a t -> 'a elt option
  (* Check if the set is a singleton, if so return unique element *)

  val bind : ('a elt -> 'b t) -> 'a t -> 'b t
end

