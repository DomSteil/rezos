/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type t;

let min_value : t;
let epoch : t;
let max_value : t;

let add : t => int64 => t;
let diff : t => t => int64;

let equal : t => t => bool;
let compare : t => t => int;

let (=) : t => t => bool;
let (<>) : t => t => bool;
let (<) : t => t => bool;
let (<=) : t => t => bool;
let (>=) : t => t => bool;
let (>) : t => t => bool;
let min : t => t => t;
let max : t => t => t;

let of_seconds : int64 => t;
let to_seconds : t => int64;

let of_notation : string => t option;
let of_notation_exn : string => t;
let to_notation : t => string;

let now : unit => t;

let encoding : t Data_encoding.t;
let rfc_encoding : t Data_encoding.t;

let pp_hum : Format.formatter => t => unit;

type 'a timed_data = {
  data: 'a ;
  time: t ;
}

let make_timed : 'a => 'a timed_data;

let timed_encoding : 'a Data_encoding.t => 'a timed_data Data_encoding.t;

module Set : Set.S with type elt = t;
module Map : Map.S with type key = t;
module Table : Hashtbl.S with type key = t;
