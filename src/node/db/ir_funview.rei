/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module type S = {
  include Irmin.RO
  let dir_mem: t => key => bool Lwt.t;
  let update: t => key => value => t Lwt.t;
  let remove: t => key => t Lwt.t;
  let list: t => key => key list Lwt.t;
  let remove_rec: t => key => t Lwt.t;
  let empty: t;
  type db;
  let of_path: db => key => t Lwt.t;
  let update_path: db => key => t => unit Lwt.t;
};

module Make (S: Irmin.S):
  S with type db = S.t;
     and type key = S.key;
     and type value = S.value;
