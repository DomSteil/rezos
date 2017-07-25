/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module type NAME = {
  let name : string list;
};

module type VALUE = {
  type t;
  let of_bytes: MBytes.t -> t tzresult;
  let to_bytes: t -> MBytes.t;
};

module type ENCODED_VALUE = {
  type t;
  let encoding: t Data_encoding.t
};

module type INDEX = {
  type t;
  let path_length: int;
  let to_path: t => string list;
  let of_path: string list => t option;
};

module type SINGLE_STORE = {
  type t;
  type value;
  let known: t => bool Lwt.t;
  let read: t => value tzresult Lwt.t;
  let read_opt: t => value option Lwt.t;
  let read_exn: t => value Lwt.t;
  let store: t => value => unit Lwt.t;
  let remove: t => unit Lwt.t;
};

module type STORE = {

  type t;
  type key = string list;
  type value = MBytes.t;

  let known: t => key => bool Lwt.t;
  let read: t => key => value tzresult Lwt.t;
  let read_opt: t => key => value option Lwt.t;
  let read_exn: t => key => value Lwt.t;
  let store: t => key => value => unit Lwt.t;
  let remove: t => key => unit Lwt.t;

  let known_dir: t => key => bool Lwt.t;
  let remove_dir: t => key => unit Lwt.t;

  let fold:
    t => key => init:'a =>
    f:([ `Key of key | `Dir of key ] => 'a => 'a Lwt.t) =>
    'a Lwt.t;

  let keys: t => key => key list Lwt.t;
  let fold_keys: t => key => init:'a => f:(key => 'a => 'a Lwt.t) => 'a Lwt.t;

};

module type SET_STORE = {
  type t;
  type elt;
  let known: t => elt => bool Lwt.t;
  let store: t => elt => unit Lwt.t;
  let remove: t => elt => unit Lwt.t;
  let elements: t => elt list Lwt.t;
  let remove_all: t => unit Lwt.t;
  let iter: t => f:(elt => unit Lwt.t) => unit Lwt.t;
  let fold: t => init:'a => f:(elt => 'a => 'a Lwt.t) => 'a Lwt.t
};

module type BUFFERED_SET_STORE = {
  include SET_STORE
  module Set : Set.S with type elt = elt;
  let read_all: t => Set.t Lwt.t;
  let store_all: t => Set.t => unit Lwt.t;
};

module type MAP_STORE = {
  type t;
  type key;
  type value;
  let known: t => key => bool Lwt.t;
  let read: t => key => value tzresult Lwt.t;
  let read_opt: t => key => value option Lwt.t;
  let read_exn: t => key => value Lwt.t;
  let store: t => key => value => unit Lwt.t;
  let remove: t => key => unit Lwt.t;
  let keys: t => key list Lwt.t;
  let bindings: t => (key * value) list Lwt.t;
  let remove_all: t => unit Lwt.t;
  let iter: t => f:(key => value => unit Lwt.t) => unit Lwt.t;
  let iter_keys: t => f:(key => unit Lwt.t) => unit Lwt.t;
  let fold: t => init:'a => f:(key => value => 'a => 'a Lwt.t) => 'a Lwt.t;
  let fold_keys: t => init:'a => f:(key => 'a => 'a Lwt.t) => 'a Lwt.t;
};

module type BUFFERED_MAP_STORE = {
  include MAP_STORE
  module Map : Map.S with type key = key;
  let read_all: t => value Map.t Lwt.t;
  let store_all: t => value Map.t => unit Lwt.t;
};

module type INDEXED_STORE = {

  type t;
  type key;

  module Store : STORE with type t = t * key

  let remove_all: t => key => unit Lwt.t;

  let fold_indexes: t => init:'a => f:(key => 'a => 'a Lwt.t) => 'a Lwt.t;
  let indexes: t => key list Lwt.t;

  let resolve_index: t => string list => key list Lwt.t;

  module Make_set (N : NAME)
    : SET_STORE with type t = t
                 and type elt = key;

  module Make_buffered_set (N : NAME) (Set : Set.S with type elt = key)
    : BUFFERED_SET_STORE with type t = t
                          and type elt = key
                          and module Set = Set;

  module Make_map (N : NAME) (V : VALUE)
    : MAP_STORE with type t = t
                 and type key = key
                 and type value = V.t;

  module Make_buffered_map
      (N : NAME) (V : VALUE) (Map : Map.S with type key = key)
    : BUFFERED_MAP_STORE with type t = t
                          and type key = key
                          and type value = V.t
                          and module Map = Map;

};
