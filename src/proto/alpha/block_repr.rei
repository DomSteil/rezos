/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Tezos_hash

/** Exported type */
type header = {
  shell: Updater.shell_block_header ;
  proto: proto_header ;
  signature: Ed25519.Signature.t ;
}

and proto_header = {
  priority: int ;
  seed_nonce_hash: Nonce_hash.t ;
  proof_of_work_nonce: MBytes.t ;
}

/** The maximum size of block headers in bytes */
let max_header_length: int;

/** Parse the protocol-specific part of a block header. */
let parse_header: Updater.raw_block_header => header tzresult;

let proto_header_encoding:
  proto_header Data_encoding.encoding;

let unsigned_header_encoding:
  (Updater.shell_block_header * proto_header) Data_encoding.encoding;

let forge_header:
  Updater.shell_block_header => proto_header => MBytes.t;
/** [forge_header shell_hdr proto_hdr] is the binary serialization
    (using [unsigned_header_encoding]) of a block header,
    comprising both the shell and the protocol part of the header,
    without the signature. */
