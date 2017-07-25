/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module Tag : {

  type t = string list;

  let add: t => string => t;
  let remove: t => string => t;
  let encoding: t Data_encoding.t;

};

module type Entity = {
  let name : string;
};

module Tags (Entity : Entity) : {

  include Client_aliases.Alias with type t = Tag.t;

  let tag_param:
    ?name:string =>
    ?desc:string =>
    ('a, Client_commands.context, 'ret) Cli_entries.params =>
    (Tag.t => 'a, Client_commands.context, 'ret) Cli_entries.params;

  let rev_find_by_tag:
    Client_commands.context =>
    string =>
    string option tzresult Lwt.t;

  let filter:
    Client_commands.context =>
    (string , t => bool) =>
    (string , t) list tzresult Lwt.t;

  let filter_by_tag:
    Client_commands.context =>
    string =>
    (string , t) list tzresult Lwt.t;

};
