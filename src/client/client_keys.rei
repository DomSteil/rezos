/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module Ed25519 = Environment.Ed25519;

module Public_key_hash :
  Client_aliases.Alias with type t = Ed25519.Public_key_hash.t;
module Public_key : Client_aliases.Alias with type t = Ed25519.Public_key.t;
module Secret_key : Client_aliases.Alias with type t = Ed25519.Secret_key.t;

module Seed : {
  let to_hex : Sodium.Sign.seed => string;
  let of_hex : string => Sodium.Sign.seed;
  let generate : unit => Sodium.Sign.seed;
  let extract : Secret_key.t => Sodium.Sign.seed;
};

let get_key:
  Client_commands.context =>
  Public_key_hash.t =>
  ( string * Public_key.t * Secret_key.t ) tzresult Lwt.t;

let get_keys:
  Client_commands.context =>
  ( string * Public_key_hash.t * Public_key.t * Secret_key.t ) list tzresult Lwt.t;

let list_keys:
  Client_commands.context =>
  (string * Public_key_hash.t * bool * bool) list tzresult Lwt.t;

let gen_keys:
  ?seed: Sodium.Sign.seed =>
  Client_commands.context =>
  string =>
  unit tzresult Lwt.t;

let commands: unit => Client_commands.command list;
