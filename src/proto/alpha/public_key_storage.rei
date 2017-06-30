/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Ed25519

type error += Inconsistent_hash of Public_key.t * Public_key_hash.t * Public_key_hash.t;

let get:
  Storage.t => Public_key_hash.t => Public_key.t tzresult Lwt.t;
let get_option:
  Storage.t => Public_key_hash.t => Public_key.t option tzresult Lwt.t;
let reveal:
  Storage.t => Public_key_hash.t => Public_key.t => Storage.t tzresult Lwt.t;
let remove:
  Storage.t => Public_key_hash.t => Storage.t Lwt.t;

let list:
  Storage.t => (Public_key_hash.t * Public_key.t) list tzresult Lwt.t;
