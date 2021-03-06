/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open P2p_types

type t = {
  data_dir: string option,
  config_file: string ,
  min_connections: int option ,
  expected_connections: int option ,
  max_connections: int option ,
  max_download_speed: int option ,
  max_upload_speed: int option ,
  peer_table_size: int option ,
  expected_pow: float option ,
  peers: string list ,
  no_bootstrap_peers: bool ,
  listen_addr: string option ,
  rpc_listen_addr: string option ,
  closed: bool ,
  cors_origins: string list ,
  cors_headers: string list ,
  rpc_tls: Node_config_file.tls option ,
  log_output: Logging.Output.t option 
};

module Term : {
  let args: t Cmdliner.Term.t;
  let data_dir: string option Cmdliner.Term.t;
  let config_file: string option Cmdliner.Term.t;
};

let read_and_patch_config_file: t -> Node_config_file.t tzresult Lwt.t;

module Manpage : {
  let misc_section: string;
  let args: Cmdliner.Manpage.block list;
  let bugs: Cmdliner.Manpage.block list;
};
