/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/**  Tezos - Persistent structures on top of {!Context} */

open Lwt


/** Keys in (kex x value) database implementations */
type key = string list;

/** Values in (kex x value) database implementations */
type value = MBytes.t;

/** Low level view over a (key x value) database implementation. */
module type STORE = {
  type t;
  let mem: t => key => bool Lwt.t;
  let dir_mem: t => key => bool Lwt.t;
  let get: t => key => value option Lwt.t;
  let set: t => key => value => t Lwt.t;
  let del: t => key => t Lwt.t;
  let list: t => key list => key list Lwt.t;
  let remove_rec: t => key => t Lwt.t;
};

/** Projection of OCaml keys of some abstract type to concrete storage
    keys. For practical reasons, all such keys must fall under a same
    {!prefix} and have the same relative {!length}. Functions
    {!to_path} and {!of_path} only take the relative part into account
    (the prefix is added and removed when needed). */
module type KEY = {
  type t;
  let prefix: key;
  let length: int;
  let to_path: t => key;
  let of_path: key => t;
  let compare: t => t => int;
};

/** A KEY instance for using raw implementation paths as keys */
module RawKey : KEY with type t = key

module type BYTES_STORE = {
  type t;
  type key;
  let mem: t => key => bool Lwt.t;
  let get: t => key => value option Lwt.t;
  let set: t => key => value => t Lwt.t;
  let del: t => key => t Lwt.t;
  let list: t => key list => key list Lwt.t;
  let remove_rec: t => key => t Lwt.t;
};

module MakeBytesStore (S : STORE) (K : KEY) :
  BYTES_STORE with type t = S.t and type key = K.t

/** {2 Typed Store Overlays} *************************************************/

/** Projection of OCaml values of some abstract type to concrete
    storage data. */
module type VALUE = {
  type t;
  let of_bytes: value => t option;
  let to_bytes: t => value;
};

/** A VALUE instance for using the raw bytes values */
module RawValue : VALUE with type t = value

/** Signature of a typed store as returned by {!MakeTypedStore} */
module type TYPED_STORE = {
  type t;
  type key;
  type value;
  let mem: t => key => bool Lwt.t;
  let get: t => key => value option Lwt.t;
  let set: t => key => value => t Lwt.t;
  let del: t => key => t Lwt.t;
};

/** Gives a typed view of a store (values of a given type stored under
    keys of a given type). The view is also restricted to a prefix,
    (which can be empty). For all primitives to work as expected, all
    keys under this prefix must be homogeneously typed. */
module MakeTypedStore (S : STORE) (K : KEY) (C : VALUE) :
  TYPED_STORE with type t = S.t and type key = K.t and type value = C.t


/* {2 Persistent Sets} ******************************************************/

/** Signature of a set as returned by {!MakePersistentSet} */
module type PERSISTENT_SET = {
  type t and key;
  let mem : t => key => bool Lwt.t;
  let set : t => key => t Lwt.t;
  let del : t => key => t Lwt.t;
  let elements : t => key list Lwt.t;
  let clear : t => t Lwt.t;
  let iter : t => f:(key => unit Lwt.t) => unit Lwt.t;
  let fold : t => 'a => f:(key => 'a => 'a Lwt.t) => 'a Lwt.t;
};

/** Signature of a buffered set as returned by {!MakeBufferedPersistentSet} */
module type BUFFERED_PERSISTENT_SET = {
  include PERSISTENT_SET
  module Set : Set.S with type elt = key
  let read : t => Set.t Lwt.t;
  let write : t => Set.t => t Lwt.t;
};

/** Build a set in the (key x value) storage by encoding elements as
    keys and using the association of (any) data to these keys as
    membership. For this to work, the prefix passed must be reserved
    for the set (every key under it is considered a member). */
module MakePersistentSet (S : STORE) (K : KEY)
  : PERSISTENT_SET with type t := S.t and type key := K.t

/* Same as {!MakePersistentSet} but also provides a way to use an
    OCaml set as an explicitly synchronized in-memory buffer. */
module MakeBufferedPersistentSet
    (S : STORE) (K : KEY) (Set : Set.S with type elt = K.t)
  : BUFFERED_PERSISTENT_SET
    with type t := S.t
     and type key := K.t
     and module Set := Set

module type PERSISTENT_MAP = {
  type t and key and value;
  let mem : t => key => bool Lwt.t;
  let get : t => key => value option Lwt.t;
  let set : t => key => value => t Lwt.t;
  let del : t => key => t Lwt.t;
  let bindings : t => (key * value) list Lwt.t;
  let clear : t => t Lwt.t;
  let iter : t => f:(key => value => unit Lwt.t) => unit Lwt.t;
  let fold : t => 'a => f:(key => value => 'a => 'a Lwt.t) => 'a Lwt.t;
};

/** Signature of a buffered map as returned by {!MakeBufferedPersistentMap} */
module type BUFFERED_PERSISTENT_MAP = {
  include PERSISTENT_MAP
  module Map : Map.S with type key = key
  let read : t => value Map.t Lwt.t;
  let write : t => value Map.t => t Lwt.t;
};

/** Build a map in the (key x value) storage. For this to work, the
    prefix passed must be reserved for the map (every key under it is
    considered the key of a binding). */
module MakePersistentMap (S : STORE) (K : KEY) (C : VALUE)
  : PERSISTENT_MAP
    with type t := S.t and type key := K.t and type value := C.t


module MakeBufferedPersistentMap
    (S : STORE) (K : KEY) (C : VALUE) (Map : Map.S with type key = K.t)
 : BUFFERED_PERSISTENT_MAP
   with type t := S.t
    and type key := K.t
    and type value := C.t
    and module Map := Map


/** {2 Predefined Instances} *************************************************/

module MakePersistentBytesMap (S : STORE) (K : KEY)
  : PERSISTENT_MAP
  with type t := S.t and type key := K.t and type value := MBytes.t

module MakeBufferedPersistentBytesMap
    (S : STORE) (K : KEY) (Map : Map.S with type key = K.t)
  : BUFFERED_PERSISTENT_MAP
    with type t := S.t
     and type key := K.t
     and type value := MBytes.t
     and module Map := Map

module type TYPED_VALUE_REPR = {
  type value;
  val encoding: value Data_encoding.t
};

module MakePersistentTypedMap (S : STORE) (K : KEY) (T : TYPED_VALUE_REPR)
  : PERSISTENT_MAP
    with type t := S.t and type key := K.t and type value := T.value

module MakeBufferedPersistentTypedMap
    (S : STORE) (K : KEY) (T : TYPED_VALUE_REPR) (Map : Map.S with type key = K.t)
  : BUFFERED_PERSISTENT_MAP
    with type t := S.t
     and type key := K.t
     and type value := T.value
     and module Map := Map

module MakeHashResolver
    (Store : {
       type t;
       let dir_mem: t -> string list -> bool Lwt.t;
       let list: t -> string list list -> string list list Lwt.t;
       let prefix: string list;
     };)
    (H: HASH) : {
  let resolve : Store.t -> string -> H.t list Lwt.t;
};
