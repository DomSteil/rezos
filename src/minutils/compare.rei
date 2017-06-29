/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module type S = {
  type t
  let (=) : t => t => bool;
  let (<>) : t => t => bool;
  let (<) : t => t => bool;
  let (<=) : t => t => bool;
  let (>=) : t => t => bool;
  let (>) : t => t => bool;
  let compare : t => t => int;
  let max : t => t => t;
  let min : t => t => t;
};

module Char : S with type t = char;
module Bool : S with type t = bool;
module Int : S with type t = int;
module Int32 : S with type t = int32;
module Uint32 : S with type t = int32;
module Int64 : S with type t = int64;
module Uint64 : S with type t = int64;
module Float : S with type t = float;
module String : S with type t = string;
module List(P : S) : S with type t = P.t list;
module Option(P : S) : S with type t = P.t option;
