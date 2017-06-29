/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Error_monad

(** Tezos - Manipulation and creation of hashes *)


(** {2 Hash Types} ************************************************************)

(** The signature of an abstract hash type, as produced by functor
    {!Make_Blake2B}. The {!t} type is abstracted for separating the
    various kinds of hashes in the system at typing time. Each type is
    equipped with functions to use it as is of as keys in the database
    or in memory sets and maps. *)

module type MINIMAL_HASH = {

  type t;

  let name: string;
  let title: string;

  let hash_bytes: MBytes.t list -> t;
  let hash_string: string list -> t;
  let size: int (* in bytes *);
  let compare: t -> t -> int;
  let equal: t -> t -> bool;

  let to_hex: t -> string;
  let of_hex: string -> t option;
  let of_hex_exn: string -> t;

  let to_string: t -> string;
  let of_string: string -> t option;
  let of_string_exn: string -> t;

  let to_bytes: t -> MBytes.t;
  let of_bytes: MBytes.t -> t option;
  let of_bytes_exn: MBytes.t -> t;

  let read: MBytes.t -> int -> t;
  let write: MBytes.t -> int -> t -> unit;

  let to_path: t -> string list;
  let of_path: string list -> t option;
  let of_path_exn: string list -> t;

  let prefix_path: string -> string list;
  let path_length: int;

};

module type INTERNAL_MINIMAL_HASH = {
  include MINIMAL_HASH
  module Table : Hashtbl.S with type key = t
};

module type HASH = {

  include MINIMAL_HASH

  let of_b58check_exn: string -> t;
  let of_b58check_opt: string -> t option;
  let to_b58check: t -> string;
  let to_short_b58check: t -> string;
  let encoding: t Data_encoding.t;
  let pp: Format.formatter -> t -> unit;
  let pp_short: Format.formatter -> t -> unit;
  type Base58.data += Hash of t;
  let b58check_encoding: t Base58.encoding;

  module Set : {
    include Set.S with type elt = t
    let encoding: t Data_encoding.t;
  };

  module Map : {
    include Map.S with type key = t
    let encoding: 'a Data_encoding.t -> 'a t Data_encoding.t;
  };

};

module type INTERNAL_HASH = {
  include HASH
  let of_b58check: string -> t tzresult;
  let param:
    ?name:string ->
    ?desc:string ->
    ('a, 'arg, 'ret) Cli_entries.params ->
    (t -> 'a, 'arg, 'ret) Cli_entries.params
  module Table : Hashtbl.S with type key = t
};

module type INTERNAL_MERKLE_TREE = {
  type elt;
  include INTERNAL_HASH
  let compute: elt list -> t;
  let empty: t;
  type path =
    | Left of path * t
    | Right of t * path
    | Op
  let compute_path: elt list -> int -> path;
  let check_path: path -> elt -> t * int;
  let path_encoding: path Data_encoding.t;
};

module type MERKLE_TREE = {
  type elt;
  include HASH
  let compute: elt list -> t;
  let empty: t;
  type path =
    | Left of path * t
    | Right of t * path
    | Op
  let compute_path: elt list -> int -> path;
  let check_path: path -> elt -> t * int;
  let path_encoding: path Data_encoding.t;
};

/** {2 Building Hashes} *******************************************************/

/** The parameters for creating a new Hash type using
    {!Make_Blake2B}. Both {!name} and {!title} are only informative,
    used in error messages and serializers. */

module type Name = {
  let name : string;
  let title : string;
  let size : int option;
};

module type PrefixedName = {
  include Name
  let b58check_prefix : string;
};

/** Builds a new Hash type using Sha256. */
module Make_minimal_Blake2B (Name : Name) : INTERNAL_MINIMAL_HASH
module Make_Blake2B
    (Register : {
       let register_encoding:
         prefix: string ->
         length: int ->
         to_raw: ('a -> string) ->
         of_raw: (string -> 'a option) ->
         wrap: ('a -> Base58.data) ->
         'a Base58.encoding
     })
    (Name : PrefixedName) : INTERNAL_HASH

(** {2 Predefined Hashes } ****************************************************)

(** Blocks hashes / IDs. *)
module Block_hash : INTERNAL_HASH

(** Operations hashes / IDs. *)
module Operation_hash : INTERNAL_HASH

(** List of operations hashes / IDs. *)
module Operation_list_hash :
  INTERNAL_MERKLE_TREE with type elt = Operation_hash.t

module Operation_list_list_hash :
  INTERNAL_MERKLE_TREE with type elt = Operation_list_hash.t

(** Protocol versions / source hashes. *)
module Protocol_hash : INTERNAL_HASH

module Net_id : {
  include INTERNAL_HASH
  let of_block_hash: Block_hash.t -> t
};

module Generic_hash : INTERNAL_MINIMAL_HASH

(**/**)

module Generic_Merkle_tree (H : sig
    type t
    type elt
    let encoding : t Data_encoding.t
    let empty : t
    let leaf : elt -> t
    let node : t -> t -> t
  end) : sig
  let compute : H.elt list -> H.t
  type path =
    | Left of path * H.t
    | Right of H.t * path
    | Op
  let compute_path: H.elt list -> int -> path
  let check_path: path -> H.elt -> H.t * int
end
