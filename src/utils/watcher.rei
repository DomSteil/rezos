/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/* {1 Notification callbacks} */

type 'a input
type stopper

let create_input : unit => 'a input;
let notify : 'a input => 'a => unit;
let create_stream : 'a input => 'a Lwt_stream.t * stopper;
let create_fake_stream : unit => 'a Lwt_stream.t * stopper;
let shutdown : stopper => unit;
