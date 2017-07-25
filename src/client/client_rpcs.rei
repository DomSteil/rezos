/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

type config = {
  host : string,
  port : int,
  tls : bool,
  logger : logger,
};

and logger =
    Logger : {
      log_request : Uri.t => Data_encoding.json => 'a Lwt.t ;
      log_success :
        'a => Cohttp.Code.status_code => Data_encoding.json  => unit Lwt.t ;
      log_error :
        'a => Cohttp.Code.status_code => string  => unit Lwt.t ;
    } => logger

let default_config: config;
let null_logger: logger;
let timings_logger: Format.formatter => logger;
let full_logger: Format.formatter => logger;

let get_json:
  config =>
  RPC.meth => string list => Data_encoding.json =>
  Data_encoding.json tzresult Lwt.t;

let call_service0:
  config =>
  (unit, unit, 'i, 'o) RPC.service =>
  'i => 'o tzresult Lwt.t;

let call_service1:
  config =>
  (unit, unit * 'a, 'i, 'o) RPC.service =>
  'a => 'i => 'o tzresult Lwt.t;

let call_service2:
  config =>
  (unit, (unit * 'a) * 'b, 'i, 'o) RPC.service =>
  'a => 'b => 'i => 'o tzresult Lwt.t;

let call_streamed_service0:
  config =>
  (unit, unit, 'a, 'b) RPC.service =>
  'a => ('b, error list) result Lwt_stream.t tzresult Lwt.t;

let call_err_service0:
  config =>
  (unit, unit, 'i, 'o tzresult) RPC.service =>
  'i => 'o tzresult Lwt.t;

let call_err_service1:
  config =>
  (unit, unit * 'a, 'i, 'o tzresult) RPC.service =>
  'a => 'i => 'o tzresult Lwt.t;

let call_err_service2:
  config =>
  (unit, (unit * 'a) * 'b, 'i, 'o tzresult) RPC.service =>
  'a => 'b => 'i => 'o tzresult Lwt.t;

let call_describe0:
  config =>
  (unit, unit, 'a, 'b) RPC.service =>
  string list => 'a => 'b tzresult Lwt.t;
