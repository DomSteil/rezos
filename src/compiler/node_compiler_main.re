/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

let compiler_name = "tezos-protocol-compiler";

let () =
  if Filename.basename Sys.argv.(0) = compiler_name {
    try
      Tezos_compiler.main ();
      Pervasives.exit 0
    with exn =>
      Format.eprintf "%a\n%!" Opterrors.report_error exn;
      Pervasives.exit 1;
    };
