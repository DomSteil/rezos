/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/* Imperative Ring Buffer */

type 'a t;
let create : int => 'a t;
let add : 'a t => 'a => unit;
let add_list : 'a t => 'a list => unit;
let last : 'a t => 'a option;
exception Empty
let last_exn : 'a t => 'a;
let fold : 'a t => init:'b => f:('b => 'a => 'b) => 'b;
let elements : 'a t => 'a list;
