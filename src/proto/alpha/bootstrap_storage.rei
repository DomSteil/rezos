/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type account = {
  public_key_hash : Ed25519.Public_key_hash.t ;
  public_key : Ed25519.Public_key.t ;
}

let account_encoding: account Data_encoding.t;

let accounts: Storage.t => account list;

let init: Storage.t => Storage.t tzresult Lwt.t;

let refill: Storage.t => Storage.t tzresult Lwt.t;
