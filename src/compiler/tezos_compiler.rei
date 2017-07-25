/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Hash

/** Low-level part of the [Updater]. */

module Meta : {
  let to_file: Lwt_io.file_name => ?hash:Protocol_hash.t => string list => unit;
  let of_file: Lwt_io.file_name => Protocol_hash.t option * string list;
};

module Protocol : {

  type t = component list;

  type component = {
    name: string,
    interface: string option,
    implementation: string
  };

  type protocol = t;

  let compare: protocol => protocol => int;
  let equal: protocol => protocol => bool;

  let hash: protocol => Protocol_hash.t;
  let encoding: protocol Data_encoding.encoding;

  let of_dir: Lwt_io.file_name => protocol;

};

let main: unit => unit;
