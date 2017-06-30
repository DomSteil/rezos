/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type t;
type cycle = t;
include Compare.S with type t := t;
let encoding: cycle Data_encoding.t;
let arg: cycle RPC.Arg.arg;
let pp: Format.formatter => cycle => unit;

let root: cycle;
let pred: cycle => cycle option;
let succ: cycle => cycle;

let to_int32: cycle => int32;
let of_int32_exn: int32 => cycle;
