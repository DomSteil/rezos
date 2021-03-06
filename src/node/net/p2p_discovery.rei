/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type t;
let create : ('msg, 'meta) P2p_connection_pool.pool => t;
let restart : t => unit;
let shutdown : t => unit Lwt.t;
