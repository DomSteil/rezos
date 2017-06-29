/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type t;

let create: init:int => alpha:float => t;
let destroy: t => unit;

let add: t => int => unit;

let on_update: (unit => unit) => unit;
let updated: unit Lwt_condition.t;

type stat = {
  total: int64 ;
  average: int ;
}
let stat: t => stat;
