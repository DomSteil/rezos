/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type t = private {
  level: Raw_level_repr.t ;
  level_position: int32 ;
  cycle: Cycle_repr.t ;
  cycle_position: int32 ;
  voting_period: Voting_period_repr.t ;
  voting_period_position: int32 ;
}

type level = t;
let encoding: level Data_encoding.t;
let pp: Format.formatter => level => unit;
let pp_full: Format.formatter => level => unit;
include Compare.S with type t := level;

let root: Raw_level_repr.t => level;

let from_raw:
  first_level:Raw_level_repr.t =>
  cycle_length:int32 =>
  voting_period_length:int32 =>
  Raw_level_repr.t => level;

let diff: level => level => int32;
