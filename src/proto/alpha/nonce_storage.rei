/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Tezos_hash

type error +=
  | Too_late_revelation
  | Too_early_revelation
  | Previously_revealed_nonce
  | Unexpected_nonce

type t = Seed_repr.nonce
type nonce = t
let encoding: nonce Data_encoding.t;

let record_hash:
  Storage.t =>
  Ed25519.Public_key_hash.t => Tez_repr.t =>
  Nonce_hash.t => Storage.t tzresult Lwt.t;

let reveal:
  Storage.t => Level_repr.t => nonce =>
  (Storage.t * Ed25519.Public_key_hash.t * Tez_repr.t) tzresult Lwt.t;

type status =
  | Unrevealed of {
      nonce_hash: Tezos_hash.Nonce_hash.t ;
      delegate_to_reward: Ed25519.Public_key_hash.t ;
      reward_amount: Tez_repr.t ;
    }
  | Revealed of nonce

let get: Storage.t => Level_repr.t => status tzresult Lwt.t;

let of_bytes: MBytes.t => nonce tzresult;
let hash: nonce => Nonce_hash.t;
let check_hash: nonce => Nonce_hash.t => bool;
