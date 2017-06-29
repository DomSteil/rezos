/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/


module type Entity = {
  type t;
  let encoding : t Data_encoding.t;
  let of_source :
    Client_commands.context =>
    string => t tzresult Lwt.t;
  let to_source :
    Client_commands.context =>
    t => string tzresult Lwt.t;
  let name : string;
};

module type Alias = {
  type t;
  let load :
    Client_commands.context =>
    (string * t) list tzresult Lwt.t;
  let find :
    Client_commands.context =>
    string => t tzresult Lwt.t;
  let find_opt :
    Client_commands.context =>
    string => t option tzresult Lwt.t;
  let rev_find :
    Client_commands.context =>
    t => string option tzresult Lwt.t;
  let name :
    Client_commands.context =>
    t => string tzresult Lwt.t;
  let mem :
    Client_commands.context =>
    string => bool tzresult Lwt.t;
  let add :
    Client_commands.context =>
    string => t => unit tzresult Lwt.t;
  let del :
    Client_commands.context =>
    string => unit tzresult Lwt.t;
  let update :
    Client_commands.context =>
    string => t => unit tzresult Lwt.t;
  let save :
    Client_commands.context =>
    (string * t) list => unit tzresult Lwt.t;
  let of_source  :
    Client_commands.context =>
    string => t tzresult Lwt.t;
  let to_source  :
    Client_commands.context =>
    t => string tzresult Lwt.t;
  let alias_param :
    ?name:string =>
    ?desc:string =>
    ('a, Client_commands.context, 'ret) Cli_entries.params =>
    (string * t => 'a, Client_commands.context, 'ret) Cli_entries.params;
  let fresh_alias_param :
    ?name:string =>
    ?desc:string =>
    ('a, Client_commands.context, 'ret) Cli_entries.params =>
    (string => 'a, Client_commands.context, 'ret) Cli_entries.params;
  let source_param :
    ?name:string =>
    ?desc:string =>
    ('a, Client_commands.context, 'ret) Cli_entries.params =>
    (t => 'a, Client_commands.context, 'ret) Cli_entries.params;
};
module Alias (Entity : Entity) : Alias with type t = Entity.t;
