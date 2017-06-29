/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module type S = {
  type t;
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

module Char = {
  type t = char;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module Bool = {
  type t = bool;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module Int = {
  type t = int;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module Int32 = {
  type t = int32;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module Int64 = {
  type t = int64;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module MakeUnsigned(Int : S)(Z : sig val zero : Int.t end) = {
  type t = Int.t;
  let compare va vb =
    Int.(if va >= Z.zero then if vb >= Z.zero then compare va vb else -1
           else if vb >= Z.zero then 1 else compare va vb);
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) a b =
    Int.(if Z.zero <= a then
             (a < b || b < Z.zero)
           else
             (b < Z.zero && a < b));
  let (<=) a b =
    Int.(if Z.zero <= a then
             (a <= b || b < Z.zero)
           else
             (b < Z.zero && a <= b));
  let (>=) a b = (<=) b a;
  let (>) a b = (<) b a;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module Uint32 = MakeUnsigned(Int32)({ let zero = 0l });
module Uint64 = MakeUnsigned(Int64)({ let zero = 0L });

module Float = {
  type t = float;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module String = {
  type t = string;
  let (=) = ((=) : t => t => bool);
  let (<>) = ((<>) : t => t => bool);
  let (<) = ((<) : t => t => bool);
  let (<=) = ((<=) : t => t => bool);
  let (>=) = ((>=) : t => t => bool);
  let (>) = ((>) : t => t => bool);
  let compare = compare;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module List(P : S) = {
  type t = P.t list;
  let rec compare xs ys =
    match xs, ys with
    | [], [] => 0
    | [], _ => -1
    | _, [] => 1
    | x :: xs, y :: ys =>
        let hd = P.compare x y in
        if hd <> 0 then hd else compare xs ys;
  let (=) xs ys = compare xs ys = 0;
  let (<>) xs ys = compare xs ys <> 0;
  let (<) xs ys = compare xs ys < 0;
  let (<=) xs ys = compare xs ys <= 0;
  let (>=) xs ys = compare xs ys >= 0;
  let (>) xs ys = compare xs ys > 0;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};

module Option(P : S) = {
  type t = P.t option;
  let rec compare xs ys =
    match xs, ys with
    | None, None => 0
    | None, _ => -1
    | _, None => 1
    | Some x, Some y => P.compare x y;
  let (=) xs ys = compare xs ys = 0;
  let (<>) xs ys = compare xs ys <> 0;
  let (<) xs ys = compare xs ys < 0;
  let (<=) xs ys = compare xs ys <= 0;
  let (>=) xs ys = compare xs ys >= 0;
  let (>) xs ys = compare xs ys > 0;
  let max x y = if x >= y then x else y;
  let min x y = if x <= y then x else y;
};
