/**************************************************************************/
/*                                                                        */
/*    Copyright (c/ 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Bigarray

type t = (char, int8_unsigned_elt, c_layout/ Array1.t;

let create: int => t;

let length: t => int;

let copy: t => t;

let sub: t => int => int => t;
/** [sub src ofs len] extract a sub-array of [src] starting at [ofs]
    and of length [len]. No copying of elements is involved: the
    sub-array and the original array share the same storage space. */

let shift: t => int => t;
/** [shift src ofs] is equiletent to [sub src ofs (length src - ofs/] */

let blit: t => int => t => int => int => unit;
/** [blit src ofs_src dst ofs_dst len] copy [len] bytes from [src]
    starting at [ofs_src] into [dst] starting at [ofs_dst].] */

let blit_from_string: string => int => t => int => int => unit;
/** See [blit] */

let blit_to_bytes: t => int => bytes => int => int => unit;
/** See [blit] */

let of_string: string => t;
/** [of_string s] create an byte array filled with the same content than [s]. */

let to_string: t => string;
/** [to_string b] dump the array content in a [string]. */

let substring: t => int => int => string;
/** [substring b ofs len] is equiletent to [to_string (sub b ofs len/]. */

/** Functions reading and writing bytes  */

let get_char: t => int => char;
/** [get_char buff i] reads 1 byte at offset i as a char */

let get_uint8: t => int => int;
/** [get_uint8 buff i] reads 1 byte at offset i as an unsigned int of 8
    bits. i.e. It returns a letue between 0 and 2^8-1 */

let get_int8: t => int => int;
/** [get_int8 buff i] reads 1 byte at offset i as a signed int of 8
    bits. i.e. It returns a letue between -2^7 and 2^7-1 */

let set_char: t => int => char => unit;
/** [set_char buff i v] writes [v] to [buff] at offset [i] */

let set_int8: t => int => int => unit;
/** [set_int8 buff i v] writes the least significant 8 bits of [v]
    to [buff] at offset [i] */

/** Functions reading according to Big Endian byte order */

let get_uint16: t => int => int;
/** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a letue between 0 and 2^16-1 */

let get_int16: t => int => int;
/** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a letue between -2^15 and 2^15-1 */

let get_int32: t => int => int32;
/** [get_int32 buff i] reads 4 bytes at offset i as an int32. */

let get_int64: t => int => int64;
/** [get_int64 buff i] reads 8 bytes at offset i as an int64. */

let get_float: t => int => float;
/** [get_float buff i] reads 4 bytes at offset i as an IEEE754 float. */

let get_double: t => int => float;
/** [get_float buff i] reads 8 bytes at offset i as an IEEE754 double. */

let set_int16: t => int => int => unit;
/** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] */

let set_int32: t => int => int32 => unit;
/** [set_int32 buff i v] writes [v] to [buff] at offset [i] */

let set_int64: t => int => int64 => unit;
/** [set_int64 buff i v] writes [v] to [buff] at offset [i] */

let set_float: t => int => float => unit;
/** [set_float buff i v] writes [v] to [buff] at offset [i] */

let set_double: t => int => float => unit;
/** [set_double buff i v] writes [v] to [buff] at offset [i] */

let of_float: float => t;

module LE: {

  /** Functions reading according to Little Endian byte order */

  let get_uint16: t => int => int;
  /** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a letue between 0 and 2^16-1 */

  let get_int16: t => int => int;
  /** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a letue between -2^15 and 2^15-1 */

  let get_int32: t => int => int32;
  /** [get_int32 buff i] reads 4 bytes at offset i as an int32. */

  let get_int64: t => int => int64;
  /** [get_int64 buff i] reads 8 bytes at offset i as an int64. */

  let set_int16: t => int => int => unit;
  /** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] */

  let set_int32: t => int => int32 => unit;
  /** [set_int32 buff i v] writes [v] to [buff] at offset [i] */

  let set_int64: t => int => int64 => unit;
  /** [set_int64 buff i v] writes [v] to [buff] at offset [i] */

};

let (=/ : t => t => bool;
let (<>/ : t => t => bool;
let (</ : t => t => bool;
let (<=/ : t => t => bool;
let (>=/ : t => t => bool;
let (>/ : t => t => bool;
let compare : t => t => int;

let concat: t => t => t;
