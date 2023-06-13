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
  val is_empty : ('a, 'b) t -> bool
  val singleton : 'a key -> 'b -> ('a, 'b) t
  val add : 'a key -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : 'a key -> ('a, 'b) t -> 'b
  val find_opt : 'a key -> ('a, 'b) t -> 'b option
  val update : 'a key -> ('b option -> 'b option) -> ('a, 'b) t -> ('a, 'b) t
  val cardinal : ('a, 'b) t -> int
  val remove : 'a key -> ('a, 'b) t -> ('a, 'b) t
  val mem :  'a key -> ('a, 'b) t -> bool
  val add_seq : ('a key * 'b) Seq.t -> ('a, 'b) t -> ('a, 'b) t
  val of_seq : ('a key * 'b) Seq.t -> ('a, 'b) t
  val partition : ('a key -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
  val choose : ('a, 'b) t -> 'a key * 'b
  val choose_opt : ('a, 'b) t -> ('a key * 'b) option
  val split : 'a key -> ('a, 'b) t -> ('a, 'b) t * 'b option * ('a, 'b) t
  val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  val compare : ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  val merge :
    ('a key -> 'b option -> 'c option -> 'd option) ->
    ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  val union :
    ('a key -> 'b -> 'b -> 'b option) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  (*s Warning: iterators do not iterate following key order *)
  val iter : ('a key -> 'b -> unit) -> ('a, 'b) t -> unit
  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val mapi : ('a key -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val fold : ('a key -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  val exists : ('a key -> 'b -> bool) -> ('a, 'b) t -> bool
  val for_all : ('a key -> 'b -> bool) -> ('a, 'b) t -> bool
  val filter : ('a key -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
  val filter_map : ('a key -> 'b -> 'c option) -> ('a, 'b) t -> ('a, 'c) t

  (*s Warning: not sorted *)
  val bindings : ('a, 'b) t -> ('a key * 'b) list
  val to_seq : ('a, 'b) t -> ('a key * 'b) Seq.t

  (*s Warning: these are linear time w.r.t. the size of the map. *)
  val min_binding_opt : ('a, 'b) t -> ('a key * 'b) option
  val max_binding_opt : ('a, 'b) t -> ('a key * 'b) option
  val min_binding : ('a, 'b) t -> 'a key * 'b
  val max_binding : ('a, 'b) t -> 'a key * 'b

  (*s Warning: these are linear time w.r.t. the size of the map and can
      call the function on terms greater/smaller than the witness *)
  val find_first_opt : ('a key -> bool) -> ('a, 'b) t -> ('a key * 'b) option
  val find_last_opt : ('a key -> bool) -> ('a, 'b) t -> ('a key * 'b) option
  val find_first : ('a key -> bool) -> ('a, 'b) t -> 'a key * 'b
  val find_last : ('a key -> bool) -> ('a, 'b) t -> 'a key * 'b

  (*s Extra functions not in [Map.S], a slightly faster find *)
  val find_any : ('a key -> 'b -> bool) -> ('a, 'b) t -> 'a key * 'b
  val find_any_opt : ('a key -> 'b -> bool) -> ('a, 'b) t -> ('a key * 'b) option
  val is_singleton : ('a, 'b) t -> ('a key * 'b) option
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
  val elements : 'a t -> 'a elt list
  val choose : 'a t -> 'a elt
  val cardinal : 'a t -> int
  val iter : ('a elt -> unit) -> 'a t -> unit
  val fold : ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : ('a elt -> bool) -> 'a t -> bool
  val exists : ('a elt -> bool) -> 'a t -> bool
  val filter : ('a elt -> bool) -> 'a t -> 'a t
  val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t

  (*s Warning: [min_elt] and [max_elt] are linear w.r.t. the size of the
      set. In other words, [min_elt t] is barely more efficient than [fold
      min t (choose t)]. *)
  val min_elt : 'a t -> 'a elt
  val max_elt : 'a t -> 'a elt

  (*s Additional functions not appearing in the signature [Set.S] from ocaml
      standard library. *)

  (* [intersect u v] determines if sets [u] and [v] have a non-empty
     intersection. *)
  val intersect : 'a t -> 'a t -> bool
end

