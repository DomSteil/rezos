/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type nonce

let random_nonce : unit => nonce
let increment_nonce : ?step:int => nonce => nonce
let nonce_encoding : nonce Data_encoding.t

type target
let default_target : target
let make_target : float => target

type secret_key
type public_key
module Public_key_hash : Hash.INTERNAL_HASH
type channel_key

let public_key_encoding : public_key Data_encoding.t
let secret_key_encoding : secret_key Data_encoding.t

let hash : public_key => Public_key_hash.t
let random_keypair : unit => secret_key * public_key * Public_key_hash.t

let box : secret_key => public_key => MBytes.t => nonce => MBytes.t
let box_open : secret_key => public_key => MBytes.t => nonce => MBytes.t option

let precompute : secret_key => public_key => channel_key
let fast_box        : channel_key => MBytes.t => nonce => MBytes.t
let fast_box_open   : channel_key => MBytes.t => nonce => MBytes.t option

let check_proof_of_work : public_key => nonce => target => bool
let generate_proof_of_work : ?max:int => public_key => target => nonce
