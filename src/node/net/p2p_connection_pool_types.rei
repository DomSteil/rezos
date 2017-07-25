/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open P2p_types

module Point_info : {

  type 'conn t;
  type 'conn point_info = 'conn t;
  /** Type of info associated to a point. */

  let compare : 'conn point_info => 'conn point_info => int;

  type greylisting_config = {
    factor: float ,
    initial_delay: int ,
    disconnection_delay: int
  }

  let create :
    ?trusted:bool =>
    ?greylisting_config:greylisting_config =>
    addr => port => 'conn point_info;
  /** [create ~trusted addr port] is a freshly minted point_info. If
      [trusted] is true, this point is considered trusted and will
      be treated as such. */

  let trusted : 'conn point_info => bool;
  /** [trusted pi] is [true] iff [pi] has is trusted,
      i.e. "whitelisted". */

  let set_trusted : 'conn point_info => unit;
  let unset_trusted : 'conn point_info => unit;

  let last_failed_connection :
    'conn point_info => Time.t option;
  let last_rejected_connection :
    'conn point_info => (Peer_id.t * Time.t) option;
  let last_established_connection :
    'conn point_info => (Peer_id.t * Time.t) option;
  let last_disconnection :
    'conn point_info => (Peer_id.t * Time.t) option;

  let last_seen :
    'conn point_info => (Peer_id.t * Time.t) option;
  /** [last_seen pi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  */

  let last_miss :
    'conn point_info => Time.t option;
  /** [last_miss pi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  */

  let greylisted :
    ?now:Time.t => 'conn point_info => bool;

  let greylisted_until : 'conn point_info => Time.t;

  let point : 'conn point_info => Point.t;

  module State : {

    type 'conn t =
      | Requested of { cancel: Canceler.t }
        /** We initiated a connection. */
      | Accepted of { current_peer_id: Peer_id.t ;
                      cancel: Canceler.t }
        /** We accepted a incoming connection. */
      | Running of { data: 'conn ;
                     current_peer_id: Peer_id.t }
        /** Successfully authentificated connection, normal business. */
      | Disconnected
        /** No connection established currently. */
    type 'conn state = 'conn t;

    let pp : Format.formatter => 'conn t => unit;

    let get : 'conn point_info => 'conn state;

    let is_disconnected : 'conn point_info => bool;

    let set_requested :
      ?timestamp:Time.t =>
      'conn point_info => Canceler.t => unit;

    let set_accepted :
      ?timestamp:Time.t =>
      'conn point_info => Peer_id.t => Canceler.t => unit;

    let set_running :
      ?timestamp:Time.t => 'conn point_info => Peer_id.t => 'conn => unit;

    let set_disconnected :
      ?timestamp:Time.t => ?requested:bool => 'conn point_info => unit;


  module Event : {

    type kind =
      | Outgoing_request
        /** We initiated a connection. */
      | Accepting_request of Peer_id.t
        /** We accepted a connection after authentifying the remote peer. */
      | Rejecting_request of Peer_id.t
        /** We rejected a connection after authentifying the remote peer. */
      | Request_rejected of Peer_id.t option
        /** The remote peer rejected our connection. */
      | Connection_established of Peer_id.t
        /** We succesfully established a authentified connection. */
      | Disconnection of Peer_id.t
        /** We decided to close the connection. */
      | External_disconnection of Peer_id.t
        /** The connection was closed for external reason. */

    type t = {
      kind : kind ,
      timestamp : Time.t
    }

    let encoding : t Data_encoding.t;


  let fold_events :
    'conn point_info => init:'a => f:('a => Event.t => 'a) => 'a;

  let watch :
    'conn point_info => Event.t Lwt_stream.t * Watcher.stopper;

  let log_incoming_rejection :
    ?timestamp:Time.t => 'conn point_info => Peer_id.t => unit;


/** Peer_id info: current and historical information about a peer_id */

module Peer_info : {

  type ('conn, 'meta) t;
  type ('conn, 'meta) peer_info = ('conn, 'meta) t;

  let compare : ('conn, 'meta) t => ('conn, 'meta) t => int;

  let create :
    ?created:Time.t =>
    ?trusted:bool =>
    metadata:'meta =>
    Peer_id.t => ('conn, 'meta) peer_info
  /** [create ~trusted ~meta peer_id] is a freshly minted peer_id info for
      [peer_id]. */

  let peer_id : ('conn, 'meta) peer_info => Peer_id.t;

  let created : ('conn, 'meta) peer_info => Time.t;
  let metadata : ('conn, 'meta) peer_info => 'meta;
  let set_metadata : ('conn, 'meta) peer_info => 'meta => unit;

  let trusted : ('conn, 'meta) peer_info => bool;
  let set_trusted : ('conn, 'meta) peer_info => unit;
  let unset_trusted : ('conn, 'meta) peer_info => unit;

  let last_failed_connection :
    ('conn, 'meta) peer_info => (Id_point.t * Time.t) option;
  let last_rejected_connection :
    ('conn, 'meta) peer_info => (Id_point.t * Time.t) option;
  let last_established_connection :
    ('conn, 'meta) peer_info => (Id_point.t * Time.t) option;
  let last_disconnection :
    ('conn, 'meta) peer_info => (Id_point.t * Time.t) option;

  let last_seen :
    ('conn, 'meta) peer_info => (Id_point.t * Time.t) option;
  /** [last_seen gi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  */

  let last_miss :
    ('conn, 'meta) peer_info => (Id_point.t * Time.t) option;
  /** [last_miss gi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  */

  module State : {

    type 'conn t =
      | Accepted of { current_point: Id_point.t ;
                      cancel: Canceler.t }
        /** We accepted a incoming connection, we greeted back and
            we are waiting for an acknowledgement. */
      | Running of { data: 'conn ;
                     current_point: Id_point.t }
        /** Successfully authentificated connection, normal business. */
      | Disconnected
        /** No connection established currently. */
    type 'conn state = 'conn t;

    let pp : Format.formatter => 'conn t => unit;

    let get : ('conn, 'meta) peer_info => 'conn state;

    let is_disconnected : ('conn, 'meta) peer_info => bool;

    let set_accepted :
      ?timestamp:Time.t =>
      ('conn, 'meta) peer_info => Id_point.t => Canceler.t => unit;

    let set_running :
      ?timestamp:Time.t =>
      ('conn, 'meta) peer_info => Id_point.t => 'conn => unit;

    let set_disconnected :
      ?timestamp:Time.t =>
      ?requested:bool =>
      ('conn, 'meta) peer_info => unit;


  module Event : {

    type kind =
      | Accepting_request
        /** We accepted a connection after authentifying the remote peer. */
      | Rejecting_request
        /** We rejected a connection after authentifying the remote peer. */
      | Request_rejected
        /** The remote peer rejected our connection. */
      | Connection_established
        /** We succesfully established a authentified connection. */
      | Disconnection
        /** We decided to close the connection. */
      | External_disconnection
        /** The connection was closed for external reason. */

    type t = {
      kind : kind ,
      timestamp : Time.t ,
      point : Id_point.t
    }

    let encoding : t Data_encoding.t;


  let fold_events :
    ('conn, 'meta) peer_info => init:'a => f:('a => Event.t => 'a) => 'a;

  let watch :
    ('conn, 'meta) peer_info => Event.t Lwt_stream.t * Watcher.stopper;

  let log_incoming_rejection :
    ?timestamp:Time.t =>
    ('conn, 'meta) peer_info => Id_point.t => unit;

  module File : {
    let load :
      string => 'meta Data_encoding.t =>
      ('conn, 'meta) peer_info list tzresult Lwt.t;
    let save :
      string => 'meta Data_encoding.t =>
      ('conn, 'meta) peer_info list => unit tzresult Lwt.t;
