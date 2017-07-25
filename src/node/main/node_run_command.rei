/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

let cmd: unit Cmdliner.Term.t , Cmdliner.Term.info;

module Manpage : {
  let command_description: string;
  let examples: Cmdliner.Manpage.block list;
};
