/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/* {1 Prefixed Base58Check encodings} */

module Prefix : {

  let block_hash: string;
  let operation_hash: string;
  let operation_list_hash: string;
  let operation_list_list_hash: string;
  let protocol_hash: string;
  let ed25519_public_key_hash: string;
  let cryptobox_public_key_hash: string;
  let ed25519_public_key: string;
  let ed25519_secret_key: string;
  let ed25519_signature: string;
  let net_id: string;

};

/* An extensible sum-type for decoded data: one case per known
    "prefix". See for instance [Hash.Block_hash.Hash] or
    [Environment.Ed25519.Public_key_hash]. */
type data = ..


type 'a encoding = private {
  prefix: string ;
  length: int ;
  encoded_prefix: string ;
  encoded_length: int ;
  to_raw: 'a => string ;
  of_raw: string => 'a option ;
  wrap: 'a => data ;
}

/** Register a new encoding. The function might raise `Invalid_arg` if
    the provided [prefix] overlap with a previously registred
    prefix. The [to_raw] and [of_raw] are the ad-hoc
    serialisation/deserialisation for the data. The [wrap] should wrap
    the deserialised value into the extensible sum-type [data] (see
    the generic function [decode]). */
let register_encoding:
  prefix: string =>
  length: int =>
  to_raw: ('a => string) =>
  of_raw: (string => 'a option) =>
  wrap: ('a => data) =>
  'a encoding

let check_encoded_prefix: 'a encoding => string => int => unit;

module Alphabet : {
  type t;
  let bitcoin: t;
  let ripple: t;
  let flickr: t;
  let make: string => t;
};

/** Encoder for a given kind of data. */
let simple_encode: ?alphabet:Alphabet.t => 'a encoding => 'a => string;

/** Decoder for a given kind of data. It returns [None] when
    the decoded data does not start with the expected prefix. */
let simple_decode: ?alphabet:Alphabet.t => 'a encoding => string => 'a option;

/** Generic decoder. It returns [None] when the decoded data does
    not start with a registred prefix. */
let decode: ?alphabet:Alphabet.t => string => data option;

/** {2 Completion of partial Base58Check value} */

/** Register a (global) resolver for a previsously
    registred kind af data. */
let register_resolver: 'a encoding => (string => 'a list Lwt.t) => unit;

/** Try to complete a prefix of a Base58Check encoded data, by using
    the previously registered resolver associated to this kind of
    data. Note that a prefix of [n] characters of a Base58-encoded
    value provides at least [n/2] bytes of a prefix of the original value. */
let complete: ?alphabet:Alphabet.t => string => string list Lwt.t;

/** {1 Low-level: distinct registering function for economic protocol} */

/** See [src/proto/environment/base48.mli]} for an inlined
    documentation. */
module Make(C: { type context }) : {

  let register_encoding:
    prefix: string =>
    length: int =>
    to_raw: ('a => string) =>
    of_raw: (string => 'a option) =>
    wrap: ('a => data) =>
    'a encoding;

  let decode: ?alphabet:Alphabet.t => string => data option;

  let register_resolver:
    'a encoding => (C.context => string => 'a list Lwt.t) => unit;

  let complete:
    ?alphabet:Alphabet.t => C.context => string => string list Lwt.t;

};

/** {2 Low-level Base58Check encodings} */

/** Base58Check-encoding/decoding functions (with error detections). */
let safe_encode: ?alphabet:Alphabet.t => string => string;
let safe_decode: ?alphabet:Alphabet.t => string => string;

/** Base58-encoding/decoding functions (without error detections). */
let raw_encode: ?alphabet:Alphabet.t => string => string;
let raw_decode: ?alphabet:Alphabet.t => string => string;



let partial_decode: ?alphabet:Alphabet.t => string => int => string;
let make_encoded_prefix: string => int => string * int;
