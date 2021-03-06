/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/** Tezos Utility library - Hexadecimal encoding */

/** Parses a sequence of hexadecimal characters pairs as bytes */
let hex_of_bytes: MBytes.t => string;

/** Prints a sequence of bytes as hexadecimal characters pairs */
let bytes_of_hex: string => MBytes.t;

/** Interprets a sequence of hexadecimal characters pairs representing
    bytes as the characters codes of an OCaml string. */
let hex_decode: string => string;

/** Formats the codes of the characters of an OCaml string as a
    sequence of hexadecimal character pairs. */
let hex_encode: string => string;
