/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

let default_name: string;

type error += No_identity_file of string;
type error += Insufficient_proof_of_work of { expected: float };

let read:
  ?expected_pow:float =>
  string => P2p.Identity.t tzresult Lwt.t;

type error += Existent_identity_file of string;

let write: string -> P2p.Identity.t -> unit tzresult Lwt.t;
