/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module Canceler = Lwt_utils.Canceler

/** Protocol version */

module Version : {
  type t = {
    name : string ,
    major : int ,
    minor : int
  };
  /** Type of a protocol version. */

  let pp : Format.formatter => t => unit
  let encoding : t Data_encoding.t
  let common: t list => t list => t option
};


/** Peer_id, i.e. persistent peer identifier */

module Peer_id : {
  type t = Crypto_box.Public_key_hash.t;
  /** Type of a peer_id, a public key hash. */

  let compare : t => t => int;
  let equal : t => t => bool;
  let pp : Format.formatter => t => unit;
  let pp_short : Format.formatter => t => unit;
  let encoding : t Data_encoding.t;
  let of_string_exn : string => t;
  module Map : Map.S with type key = t;
  module Set : Set.S with type elt = t;
  module Table : Hashtbl.S with type key = t;
};

type addr = Ipaddr.V6.t;
type port = int;


/** Point, i.e. socket address */

module Point : {
  type t = addr , port;
  let compare : t => t => int;
  let pp : Format.formatter => t => unit;
  let pp_opt : Format.formatter => t option => unit;
  let of_string_exn : string => t;
  let of_string : string => (t, string) result;
  let to_string : t => string;
  let encoding : t Data_encoding.t;
  let is_local : t => bool;
  let is_global : t => bool;
  module Map : Map.S with type key = t;
  module Set : Set.S with type elt = t;
  module Table : Hashtbl.S with type key = t;

};

/** Point representing a reachable socket address */

module Id_point : {
  type t = addr * port option;
  let compare : t => t => int;
  let equal : t => t => bool;
  let pp : Format.formatter => t => unit;
  let pp_opt : Format.formatter => t option => unit;
  let to_string : t => string;
  let encoding : t Data_encoding.t;
  let is_local : t => bool;
  let is_global : t => bool;
  let of_point : Point.t => t;
  let to_point : t => Point.t option;
  let to_point_exn : t => Point.t;
  module Map : Map.S with type key = t;
  module Set : Set.S with type elt = t;
  module Table : Hashtbl.S with type key = t;
};


/** Identity */

module Identity : {
  type t = {
    peer_id : Peer_id.t ,
    public_key : Crypto_box.public_key ,
    secret_key : Crypto_box.secret_key ,
    proof_of_work_stamp : Crypto_box.nonce
  };
  /** Type of an identity, comprising a peer_id, a crypto keypair, and a
      proof of work stamp with enough difficulty so that the network
      accept this identity as genuine. */

  let encoding : t Data_encoding.t;

  let generate : Crypto_box.target => t;
  /** [generate target] is a freshly minted identity whose proof of
      work stamp difficulty is at least equal to [target]. */

  let generate_with_animation :
    Format.formatter => Crypto_box.target => t;
  /** [generate_with_animation ppf target] is a freshly minted identity
      whose proof of work stamp difficulty is at least equal to [target]. */

};


/** Bandwidth usage statistics */

module Stat : {

  type t = {
    total_sent : int64 ,
    total_recv : int64 ,
    current_inflow : int ,
    current_outflow : int
  };

  let empty : t;
  let pp: Format.formatter => t => unit;
  let encoding : t Data_encoding.t;
};

/** Information about a connection */

module Connection_info : {

  type t = {
    incoming : bool,
    peer_id : Peer_id.t,
    id_point : Id_point.t,
    remote_socket_port : port,
    versions : Version.t list
  }

  let pp: Format.formatter => t => unit;
  let encoding : t Data_encoding.t;

};
