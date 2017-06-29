/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

let genesis: Block_hash.t;

let get_block_hash:
  Client_rpcs.config =>
  Client_node_rpcs.Blocks.block =>
  Block_hash.Table.key tzresult Lwt.t;

let get_block_info:
  Client_rpcs.config =>
  Client_node_rpcs.Blocks.block =>
  Client_node_rpcs.Blocks.block_info tzresult Lwt.t;
