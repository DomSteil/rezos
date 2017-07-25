/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/** Typed RPC services: definition, binding and dispatch. */

/** Typed path argument. */
module Arg : sig

  type 'a arg
  let make:
    ?descr:string =>
    name:string =>
    destruct:(string => ('a, string) result) =>
    construct:('a => string) =>
    unit => 'a arg;

  type descr = {
    name: string,
    descr: string option
  };
  let descr: 'a arg => descr

  let int: int arg;
  let int32: int32 arg;
  let int64: int64 arg;
  let float: float arg;


/** Parametrized path to services. */
module Path : sig

  type ('prefix, 'params) path
  type 'prefix context = ('prefix, 'prefix) path

  let root: 'a context/

  let add_suffix:
    ('prefix, 'params) path => string => ('prefix, 'params) path;
  let (/):
    ('prefix, 'params) path => string => ('prefix, 'params) path;

  let add_arg:
    ('prefix, 'params) path => 'a Arg.arg => ('prefix, 'params * 'a) path;
  let (/:):
    ('prefix, 'params) path => 'a Arg.arg => ('prefix, 'params * 'a) path;

  let prefix:
    ('prefix, 'a) path => ('a, 'params) path => ('prefix, 'params) path;

  let map:
    ('a => 'b) => ('b => 'a) => ('prefix, 'a) path => ('prefix, 'b) path;


/** HTTP methods as defined in Cohttp.Code */

type meth = [
  | `GET
  | `POST
  | `HEAD
  | `DELETE
  | `PATCH
  | `PUT
  | `OPTIONS
  | `TRACE
  | `CONNECT
  | `Other of string
]

let string_of_method : meth => string;

/** Services. */
type ('prefix, 'params, 'input, 'output) service

let service:
  ?meth: meth =>
  ?description: string =>
  input: 'input Data_encoding.t =>
  output: 'output Data_encoding.t =>
  ('prefix, 'params) Path.path =>
  ('prefix, 'params, 'input, 'output) service;

let get_service:
  ?description: string =>
  output: 'output Data_encoding.t =>
  ('prefix, 'params) Path.path =>
  ('prefix, 'params, unit, 'output) service

let head_service:
  ?description: string =>
  ('prefix, 'params) Path.path =>
  ('prefix, 'params, unit, unit) service

let post_service:
  ?description: string =>
  input: 'input Data_encoding.t =>
  output: 'output Data_encoding.t =>
  ('prefix, 'params) Path.path =>
  ('prefix, 'params, 'input, 'output) service

let put_service:
  ?description: string =>
  input: 'input Data_encoding.t =>
  output: 'output Data_encoding.t =>
  ('prefix, 'params) Path.path =>
  ('prefix, 'params, 'input, 'output) service

let delete_service:
  ?description: string =>
  input: 'input Data_encoding.t =>
  output: 'output Data_encoding.t =>
  ('prefix, 'params) Path.path =>
  ('prefix, 'params, 'input, 'output) service;

let prefix:
  ('prefix, 'inner_prefix) Path.path =>
  ('inner_prefix, 'params, 'input, 'output) service =>
  ('prefix, 'params, 'input, 'output) service;

let forge_request:
  (unit, 'params, 'input, 'output) service =>
  'params => 'input => meth * string list * Data_encoding.json;

let read_answer:
  (unit, 'params, 'input, 'output) service =>
  Data_encoding.json => ('output, string) result;

/** Service directory description */
module Description : sig

  type service_descr = {
    description: string option ;
    input: Json_schema.schema ;
    output: Json_schema.schema ;
  }

  type directory_descr =
    | Static of static_directory_descr
    | Dynamic of string option

  and static_directory_descr = {
    service: service_descr option ;
    subdirs: static_subdirectories_descr option ;
  }

  and static_subdirectories_descr =
    | Suffixes of directory_descr Map.Make(String).t
    | Arg of Arg.descr * directory_descr

  let service:
    ?meth: meth =>
    ?description:string =>
    ('prefix, 'params) Path.path =>
    ('prefix, 'params, bool option, directory_descr) service

  val pp_print_directory_descr:
    Format.formatter => directory_descr => unit


module Answer : sig

  /** Return type for service handler */
  type 'a answer =
    { code : int ;
      body : 'a output ;
    }

  and 'a output =
    | Empty
    | Single of 'a
    | Stream of 'a stream

  and 'a stream = {
    next: unit => 'a option Lwt.t ;
    shutdown: unit => unit ;
  }

  let ok: 'a => 'a answer;
  let answer: ?code:int => 'a => 'a answer;
  let return: ?code:int => 'a => 'a answer Lwt.t;
  let return_stream: 'a stream => 'a answer Lwt.t;



/** Dispatch tree */
type 'prefix directory


let empty: 'prefix directory;

let map: ('a => 'b) => 'b directory => 'a directory;

let prefix: ('pr, 'p) Path.path => 'p directory => 'pr directory;
let merge: 'a directory => 'a directory => 'a directory;

/** Possible error while registring services. */
type step =
  | Static of string
  | Dynamic of Arg.descr
type conflict =
  | CService | CDir | CBuilder | CCustom
  | CTypes of Arg.descr *
              Arg.descr
  | CType of Arg.descr * string list
exception Conflict of step list * conflict

/** Registring handler in service tree. */
let register:
  'prefix directory =>
  ('prefix, 'params, 'input, 'output) service =>
  ('params => 'input => 'output Answer.answer Lwt.t) =>
  'prefix directory

/** Registring handler in service tree. Curryfied variant.  */
let register0:
  unit directory =>
  (unit, unit, 'i, 'o) service =>
  ('i => 'o Answer.answer Lwt.t) =>
  unit directory;

let register1:
  'prefix directory =>
  ('prefix, unit * 'a, 'i, 'o) service =>
  ('a => 'i => 'o Answer.answer Lwt.t) =>
  'prefix directory;

let register2:
  'prefix directory =>
  ('prefix, (unit * 'a) * 'b, 'i, 'o) service =>
  ('a => 'b => 'i => 'o Answer.answer Lwt.t) =>
  'prefix directory;

let register3:
  'prefix directory =>
  ('prefix, ((unit * 'a) * 'b) * 'c, 'i, 'o) service =>
  ('a => 'b => 'c => 'i => 'o Answer.answer Lwt.t) =>
  'prefix directory;

let register4:
  'prefix directory =>
  ('prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'i, 'o) service =>
  ('a => 'b => 'c => 'd => 'i => 'o Answer.answer Lwt.t) =>
  'prefix directory;

let register5:
  'prefix directory =>
  ('prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'i, 'o) service =>
  ('a => 'b => 'c => 'd => 'e => 'i => 'o Answer.answer Lwt.t) =>
  'prefix directory;

/** Registring dynamic subtree. */
let register_dynamic_directory:
  ?meths:meth list =>
  ?descr:string =>
  'prefix directory =>
  ('prefix, 'a) Path.path =>
  ('a => 'a directory Lwt.t) =>
  'prefix directory;

/** Registring dynamic subtree. (Curryfied variant) */
let register_dynamic_directory1:
  ?descr:string =>
  'prefix directory =>
  ('prefix, unit * 'a) Path.path =>
  ('a => (unit * 'a) directory Lwt.t) =>
  'prefix directory;

let register_dynamic_directory2:
  ?descr:string =>
  'prefix directory =>
  ('prefix, (unit * 'a) * 'b) Path.path =>
  ('a => 'b => ((unit * 'a) * 'b) directory Lwt.t) =>
  'prefix directory;

let register_dynamic_directory3:
  ?descr:string =>
  'prefix directory =>
  ('prefix, ((unit * 'a) * 'b) * 'c) Path.path =>
  ('a => 'b => 'c => (((unit * 'a) * 'b) * 'c) directory Lwt.t) =>
  'prefix directory;

/** Registring custom directory lookup. */
type custom_lookup = RestoDirectory.custom_lookup
  (* | CustomService of Description.service_descr * *)
  (*                    ( Data_encoding.json option => *)
  (*                      Data_encoding.json Answer.answer Lwt.t ) *)
  (* | CustomDirectory of Description.directory_descr *)

let register_custom_lookup:
  ?meth:meth =>
  ?descr:string =>
  'prefix directory =>
  ('prefix, 'params) Path.path =>
  ('params => string list => custom_lookup Lwt.t) =>
  'prefix directory;

let register_custom_lookup1:
  ?meth:meth =>
  ?descr:string =>
  'prefix directory =>
  ('prefix, unit * 'a) Path.path =>
  ('a => string list => custom_lookup Lwt.t) =>
  'prefix directory;

let register_custom_lookup2:
  ?meth:meth =>
  ?descr:string =>
  'prefix directory =>
  ('prefix, (unit * 'a) * 'b) Path.path =>
  ('a => 'b => string list => custom_lookup Lwt.t) =>
  'prefix directory;

let register_custom_lookup3:
  ?meth:meth =>
  ?descr:string =>
  'prefix directory =>
  ('prefix, ((unit * 'a) * 'b) * 'c) Path.path =>
  ('a => 'b => 'c => string list => custom_lookup Lwt.t) =>
  'prefix directory;

/** Registring a description service. */
let register_describe_directory_service:
  'prefix directory =>
  ('prefix, 'prefix, bool option, Description.directory_descr) service =>
  'prefix directory;

exception Cannot_parse of Arg.descr * string * string list

/** Resolve a service. */
val lookup:
  'prefix directory => ?meth:meth => 'prefix => string list =>
  (Data_encoding.json option => Data_encoding.json Answer.answer Lwt.t) Lwt.t
