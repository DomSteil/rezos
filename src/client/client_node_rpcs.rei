/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Client_rpcs

let errors:
  config => Json_schema.schema tzresult Lwt.t;

let forge_block:
  config =>
  ?net_id:Net_id.t =>
  ?level:Int32.t =>
  ?proto_level:int =>
  ?predecessor:Block_hash.t =>
  ?timestamp:Time.t =>
  Fitness.fitness =>
  Operation_list_list_hash.t =>
  MBytes.t =>
  MBytes.t tzresult Lwt.t;
/* [forge_block cctxt ?net ?predecessor ?timestamp fitness ops
    proto_hdr] returns the serialization of a block header with
    [proto_hdr] as protocol-specific part. The arguments [?net] and
    [?predecessor] are infered from the current head of main network,
    and [?timestamp] defaults to [Time.now ()]. */

let validate_block:
  config =>
  Net_id.t => Block_hash.t =>
  unit tzresult Lwt.t;

let inject_block:
  config =>
  ?async:bool => ?force:bool =>
  MBytes.t => Operation_hash.t list list =>
  Block_hash.t tzresult Lwt.t;
/* [inject_block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. */

let inject_operation:
  config =>
  ?async:bool => ?force:bool =>
  MBytes.t =>
  Operation_hash.t tzresult Lwt.t;

let inject_protocol:
  config =>
  ?async:bool => ?force:bool =>
  Tezos_compiler.Protocol.t =>
  Protocol_hash.t tzresult Lwt.t;

module Blocks : {

  type block = [
    | `Genesis
    | `Head of int | `Prevalidation
    | `Test_head of int | `Test_prevalidation
    | `Hash of Block_hash.t
    ]

  let net:
    config =>
    block => Net_id.t tzresult Lwt.t;
  let level:
    config =>
    block => Int32.t tzresult Lwt.t;
  let predecessor:
    config =>
    block => Block_hash.t tzresult Lwt.t;
  let predecessors:
    config =>
    block => int => Block_hash.t list tzresult Lwt.t;
  let hash:
    config =>
    block => Block_hash.t tzresult Lwt.t;
  let timestamp:
    config =>
    block => Time.t tzresult Lwt.t;
  let fitness:
    config =>
    block => MBytes.t list tzresult Lwt.t;
  let operations:
    config =>
    block => Operation_hash.t list list tzresult Lwt.t;
  let protocol:
    config =>
    block => Protocol_hash.t tzresult Lwt.t;
  let test_network:
    config =>
    block => Context.test_network tzresult Lwt.t;

  let pending_operations:
    config =>
    block =>
    (error Prevalidation.preapply_result * Operation_hash.Set.t) tzresult Lwt.t;

  type block_info = {
    hash: Block_hash.t,
    net_id: Net_id.t,
    level: Int32.t,
    proto_level: int, /* uint8 */
    predecessor: Block_hash.t,
    timestamp: Time.t,
    operations_hash: Operation_list_list_hash.t,
    fitness: MBytes.t list,
    data: MBytes.t,
    operations: Operation_hash.t list list option,
    protocol: Protocol_hash.t,
    test_network: Context.test_network,
  };

  let info:
    config =>
    ?include_ops:bool => block => block_info tzresult Lwt.t;

  let list:
    config =>
    ?include_ops:bool => ?length:int => ?heads:Block_hash.t list =>
    ?delay:int => ?min_date:Time.t => ?min_heads:int =>
    unit => block_info list list tzresult Lwt.t;

  let monitor:
    config =>
    ?include_ops:bool => ?length:int => ?heads:Block_hash.t list =>
    ?delay:int => ?min_date:Time.t => ?min_heads:int =>
    unit => block_info list list tzresult Lwt_stream.t tzresult Lwt.t;

  type preapply_result = {
    operations: error Prevalidation.preapply_result,
    fitness: MBytes.t list,
    timestamp: Time.t,
  };

  let preapply:
    config =>
    block =>
    ?timestamp:Time.t =>
    ?sort:bool =>
    Hash.Operation_hash.t list => preapply_result tzresult Lwt.t;

};

module Operations : {

  let contents:
    config =>
    Operation_hash.t list => Store.Operation.t list tzresult Lwt.t;

  let monitor:
    config =>
    ?contents:bool => unit =>
    (Operation_hash.t * Store.Operation.t option) list list tzresult
      Lwt_stream.t tzresult Lwt.t;

};

module Protocols : {

  let contents:
    config =>
    Protocol_hash.t => Store.Protocol.t tzresult Lwt.t;

  let list:
    config =>
    ?contents:bool => unit =>
    (Protocol_hash.t * Store.Protocol.t option) list tzresult Lwt.t;

};

let bootstrapped:
  config => (Block_hash.t * Time.t) tzresult Lwt_stream.t tzresult Lwt.t

module Network : {

  let stat:
    config => P2p_types.Stat.t tzresult Lwt.t;

  let connections:
    config => P2p_types.Connection_info.t list tzresult Lwt.t;

  let peers:
    config => (P2p.Peer_id.t * P2p.RPC.Peer_id.info) list tzresult Lwt.t;

  let points:
    config => (P2p.Point.t * P2p.RPC.Point.info) list tzresult Lwt.t;

};

let complete:
  config =>
  ?block:Blocks.block => string => string list tzresult Lwt.t;

let describe:
  config =>
  ?recurse:bool => string list =>
  RPC.Description.directory_descr tzresult Lwt.t;
