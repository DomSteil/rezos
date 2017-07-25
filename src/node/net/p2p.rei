/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/** A peer connection address */
type addr = Ipaddr.V6.t;

/** A peer connection port */
type port = int;

/** A p2p protocol version */
module Version = P2p_types.Version;

/** A global identifier for a peer, a.k.a. an identity */

module Peer_id = P2p_types.Peer_id;

module Identity = P2p_types.Identity;

module Point = P2p_types.Point;

module Id_point = P2p_types.Id_point;

module Connection_info = P2p_types.Connection_info;

module Stat = P2p_types.Stat;

type 'meta meta_config = {
  encoding : 'meta Data_encoding.t;
  initial : 'meta;
  score : 'meta => float
}

type 'msg app_message_encoding = Encoding : {
    tag: int ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a => 'msg ;
    unwrap: 'msg => 'a option ;
    max_length: int option ;
  } => 'msg app_message_encoding

type 'msg message_config = {
  encoding : 'msg app_message_encoding list ;
  versions : Version.t list;
}

/** Network configuration */

type config = {

  listening_port : port option,
  /** Tells if incoming connections accepted, precising the TCP port
      on which the peer can be reached */

  listening_addr : addr option,
  /** When incoming connections are accepted, precising on which
      IP adddress the node listen (default: [[::]]). */

  trusted_points : Point.t list ,
  /** List of hard-coded known peers to bootstrap the network from. */

  peers_file : string ,
  /** The path to the JSON file where the metadata associated to
      peer_ids are loaded / stored. */

  closed_network : bool ,
  /** If [true], the only accepted connections are from peers whose
      addresses are in [trusted_peers]. */

  identity : Identity.t ,
  /** Cryptographic identity of the peer. */

  proof_of_work_target : Crypto_box.target
  /** Expected level of proof of work of peers' identity. */

}

/** Network capacities */

type limits = {

  authentification_timeout : float ,
  /** Delay granted to a peer to perform authentication, in seconds. */

  min_connections : int ,
  /** Strict minimum number of connections (triggers an urgent maintenance) */

  expected_connections : int ,
  /** Targeted number of connections to reach when bootstraping / maintaining */

  max_connections : int ,
  /** Maximum number of connections (exceeding peers are disconnected) */

  backlog : int ,
  /** Argument of [Lwt_unix.accept].*/

  max_incoming_connections : int ,
  /** Maximum not-yet-authenticated incoming connections. */

  max_download_speed : int option ,
  /** Hard-limit in the number of bytes received per second. */

  max_upload_speed : int option ,
  /** Hard-limit in the number of bytes sent per second. */

  read_buffer_size : int ,
  /** Size in bytes of the buffer passed to [Lwt_unix.read]. */

  read_queue_size : int option ,
  write_queue_size : int option ,
  incoming_app_message_queue_size : int option ,
  incoming_message_queue_size : int option ,
  outgoing_message_queue_size : int option ,
  /** Various bounds for internal queues. */

  known_peer_ids_history_size : int ,
  known_points_history_size : int ,
  /** Size of circular log buffers, in number of events recorded. */

  max_known_peer_ids : (int * int) option ,
  max_known_points : (int * int) option ,
  /** Optional limitation of internal hashtables (max, target) */

  swap_linger : float ,
  /** Peer swapping does not occur more than once during a timespan of
      [swap_linger] seconds. */

};

type ('msg, 'meta) t;
type ('msg, 'meta) net = ('msg, 'meta) t;

/** A faked p2p layer, which do not initiate any connection
    nor open any listening socket */
let faked_network : 'meta meta_config => ('msg, 'meta) net;

/** Main network initialisation function */
let create :
  config:config => limits:limits =>
  'meta meta_config => 'msg message_config =>  ('msg, 'meta) net tzresult Lwt.t;

/** Return one's peer_id */
let peer_id : ('msg, 'meta) net => Peer_id.t;

/** A maintenance operation : try and reach the ideal number of peers */
let maintain : ('msg, 'meta) net => unit Lwt.t;

/** Voluntarily drop some peers and replace them by new buddies */
let roll : ('msg, 'meta) net => unit Lwt.t;

/** Close all connections properly */
let shutdown : ('msg, 'meta) net => unit Lwt.t;

/** A connection to a peer */
type ('msg, 'meta) connection;

/** Access the domain of active peers */
let connections : ('msg, 'meta) net => ('msg, 'meta) connection list;

/** Return the active peer with identity [peer_id] */
let find_connection : ('msg, 'meta) net => Peer_id.t => ('msg, 'meta) connection option;

/** Access the info of an active peer, if available */
let connection_info :
  ('msg, 'meta) net => ('msg, 'meta) connection => Connection_info.t;
let connection_stat :
  ('msg, 'meta) net => ('msg, 'meta) connection => Stat.t;
let global_stat : ('msg, 'meta) net => Stat.t;

/** Accessors for meta information about a global identifier */
let get_metadata : ('msg, 'meta) net => Peer_id.t => 'meta;
let set_metadata : ('msg, 'meta) net => Peer_id.t => 'meta => unit;

/** Wait for a message from a given connection. */
let recv :
  ('msg, 'meta) net => ('msg, 'meta) connection => 'msg tzresult Lwt.t;

/** Wait for a message from any active connections. */
let recv_any :
  ('msg, 'meta) net => (('msg, 'meta) connection * 'msg) Lwt.t;

/** [send net peer msg] is a thread that returns when [msg] has been
    successfully enqueued in the send queue. */
let send :
  ('msg, 'meta) net => ('msg, 'meta) connection => 'msg => unit tzresult Lwt.t;

/** [try_send net peer msg] is [true] if [msg] has been added to the
    send queue for [peer], [false] otherwise */
let try_send :
  ('msg, 'meta) net => ('msg, 'meta) connection => 'msg => bool;

/** Send a message to all peers */
let broadcast : ('msg, 'meta) net => 'msg => unit;

module RPC : {

  let stat : ('msg, 'meta) net => Stat.t;

  module Event = P2p_connection_pool.Log_event;

  let watch : ('msg, 'meta) net => Event.t Lwt_stream.t * Watcher.stopper;
  let connect : ('msg, 'meta) net => Point.t => float => unit tzresult Lwt.t;

  module Connection : {
    let info : ('msg, 'meta) net => Peer_id.t => Connection_info.t option;
    let kick : ('msg, 'meta) net => Peer_id.t => bool => unit Lwt.t;
    let list : ('msg, 'meta) net => Connection_info.t list;
    let count : ('msg, 'meta) net => int;
  };

  module Point : {
    include module type of Point;

    type state =
      | Requested
      | Accepted of Peer_id.t
      | Running of Peer_id.t
      | Disconnected

    let pp_state_digram : Format.formatter => state => unit;
    let state_encoding : state Data_encoding.t;

    type info = {
      trusted : bool ,
      greylisted_until : Time.t ,
      state : state ,
      last_failed_connection : Time.t option ,
      last_rejected_connection : (Peer_id.t * Time.t) option ,
      last_established_connection : (Peer_id.t * Time.t) option ,
      last_disconnection : (Peer_id.t * Time.t) option ,
      last_seen : (Peer_id.t * Time.t) option ,
      last_miss : Time.t option 
    }

    let info_encoding : info Data_encoding.t

    module Event = P2p_connection_pool_types.Point_info.Event

    let info :
      ('msg, 'meta) net => Point.t => info option
    let list :
      ?restrict:state list => ('msg, 'meta) net => (Point.t * info) list
    let events :
      ?max:int => ?rev:bool => ('msg, 'meta) net => Point.t => Event.t list
    let watch :
      ('msg, 'meta) net => Point.t => Event.t Lwt_stream.t * Watcher.stopper
  end

  module Peer_id : sig
    include module type of Peer_id

    type state =
      | Accepted
      | Running
      | Disconnected

    let pp_state_digram : Format.formatter => state => unit
    let state_encoding : state Data_encoding.t

    type info = {
      score : float ;
      trusted : bool ;
      state : state ;
      id_point : Id_point.t option ;
      stat : Stat.t ;
      last_failed_connection : (Id_point.t * Time.t) option ;
      last_rejected_connection : (Id_point.t * Time.t) option ;
      last_established_connection : (Id_point.t * Time.t) option ;
      last_disconnection : (Id_point.t * Time.t) option ;
      last_seen : (Id_point.t * Time.t) option ;
      last_miss : (Id_point.t * Time.t) option ;
    }
    let info_encoding : info Data_encoding.t

    module Event = P2p_connection_pool_types.Peer_info.Event

    let info :
      ('msg, 'meta) net => Peer_id.t => info option
    let list :
      ?restrict:state list => ('msg, 'meta) net => (Peer_id.t * info) list
    let events :
      ?max:int => ?rev:bool => ('msg, 'meta) net => Peer_id.t => Event.t list
    let watch :
      ('msg, 'meta) net => Peer_id.t => Event.t Lwt_stream.t * Watcher.stopper

  end

end

let fold_connections :
  ('msg, 'meta) net =>
  init:'a => f:(Peer_id.t => ('msg, 'meta) connection => 'a => 'a) => 'a

let iter_connections :
  ('msg, 'meta) net =>
  (Peer_id.t => ('msg, 'meta) connection => unit) => unit

let on_new_connection :
  ('msg, 'meta) net =>
  (Peer_id.t => ('msg, 'meta) connection => unit) => unit

(**/**)
module Raw : sig
  type 'a t =
    | Bootstrap
    | Advertise of P2p_types.Point.t list
    | Swap_request of Point.t * Peer_id.t
    | Swap_ack of Point.t * Peer_id.t
    | Message of 'a
    | Disconnect
  let encoding: 'msg app_message_encoding list => 'msg t Data_encoding.t
end
