/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

let current: Storage.t => Level_repr.t;
let previous: Storage.t => Level_repr.t;

let root: Storage.t => Level_repr.t;

let from_raw: Storage.t => ?offset:int32 => Raw_level_repr.t => Level_repr.t;
let pred: Storage.t => Level_repr.t => Level_repr.t option;
let succ: Storage.t => Level_repr.t => Level_repr.t;

let last_level_in_cycle: Storage.t => Cycle_repr.t => Level_repr.t;
let levels_in_cycle: Storage.t => Cycle_repr.t => Level_repr.t list;
