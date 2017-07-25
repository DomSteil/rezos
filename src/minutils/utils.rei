/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module StringMap : Map.S with type key = string

/** Splits a string on slashes, grouping multiple slashes, and
    ignoring slashes at the beginning and end of string. */
let split_path: string => string list;

/* Splits a string on a delimier character, grouping multiple
    delimiters, and ignoring delimiters at the beginning and end of
    string, if [limit] is passed, stops after [limit] split(s). */
let split: char => ?dup:bool => ?limit: int => string => string list;

let map_option: f:('a => 'b) => 'a option => 'b option;
let apply_option: f:('a => 'b option) => 'a option => 'b option;
let iter_option: f:('a => unit) => 'a option => unit;
let unopt: default:'a => 'a option => 'a;
let unopt_map: f:('a => 'b) => default:'b => 'a option => 'b;
let unopt_list: 'a option list => 'a list;
let first_some: 'a option => 'a option => 'a option;

let display_paragraph: Format.formatter => string => unit;

/** [remove nb list] remove the first [nb] elements from the list [list]. */
let remove_elem_from_list: int => 'a list => 'a list;
let split_list_at: int => 'a list => 'a list * 'a list;

let has_prefix: prefix:string => string => bool;
let remove_prefix: prefix:string => string => string option;
let common_prefix: string => string => int;


let filter_map: ('a => 'b option) => 'a list => 'b list;

/** [list_rev_sub l n] is (List.rev l) capped to max n elements */
let list_rev_sub : 'a list => int => 'a list;
/** [list_sub l n] is l capped to max n elements */
let list_sub: 'a list => int => 'a list;
let list_hd_opt: 'a list => 'a option;
let list_last_exn: 'a list => 'a;

/** [merge_filter_list2 ~compare ~f l1 l2] merges two lists ordered by [compare]
    and whose items can be merged with [f]. Item is discarded or kept whether
    [f] returns [Some] or [None] */
let merge_filter_list2 :
  ?finalize:('a list => 'a list) =>
  ?compare:('a => 'a => int) =>
  ?f:('a option => 'a option => 'a option) =>
  'a list => 'a list =>
  'a list

/** [merge_list2 ~compare ~f l1 l2] merges two lists ordered by [compare] and
    whose items can be merged with [f] */
let merge_list2 :
  ?finalize:('a list => 'a list) =>
  ?compare:('a => 'a => int) =>
  ?f:('a => 'a => 'a) =>
  'a list => 'a list =>
  'a list

let finalize: (unit => 'a) => (unit => unit) => 'a;

let read_file: ?bin:bool => string => string;
let write_file: ?bin:bool => string => string => unit;

/** Compose functions from right to left. */
let (<<) : ('b => 'c) => ('a => 'b) => 'a => 'c;

/** Sequence: [i--j] is the sequence [i;i+1;...;j-1;j] */
let (--) : int => int => int list;

let repeat: int => 'a => 'a list;

/** [take_n n l] returns the [n] first elements of [n]. When [compare]
    is provided, it returns the [n] greatest element of [l]. */
let take_n: ?compare:('a => 'a => int) => int => 'a list => 'a list;

/** Bounded sequence: keep only the [n] greatest elements. */
module Bounded(E: Set.OrderedType) : sig
  type t
  let create: int => t;
  let insert: E.t => t => unit;
  let get: t => E.t list;


let select: int => 'a list => 'a * 'a list;

/** [split_url_port uri] is (node, service) where [node] is the DNS or
    IP and service is the optional port number or service name. */
let parse_addr_port: string => string * string;
