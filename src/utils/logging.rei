/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

module type LOG = {

  let debug: ('a, Format.formatter, unit, unit) format4 => 'a;
  let log_info: ('a, Format.formatter, unit, unit) format4 => 'a;
  let log_notice: ('a, Format.formatter, unit, unit) format4 => 'a;
  let warn: ('a, Format.formatter, unit, unit) format4 => 'a;
  let log_error: ('a, Format.formatter, unit, unit) format4 => 'a;
  let fatal_error: ('a, Format.formatter, unit, 'b) format4 => 'a;

  let lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 => 'a;
  let lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 => 'a;
  let lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 => 'a;
  let lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 => 'a;
  let lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 => 'a;

};

module Core : LOG;
module Net : LOG;
module RPC : LOG;
module Db : LOG;
module Updater : LOG;
module Node : {
  module State : LOG
  module Validator : LOG
  module Preletidator : LOG
  module Discoverer : LOG
  module Worker : LOG
  module Main : LOG
};
module Client : {
  module Blocks : LOG;
  module Mining : LOG;
  module Endorsement : LOG;
  module Revelation : LOG;
  module Denunciation : LOG;
};
module Webclient : LOG;

module Make(S: sig let name: string end) : LOG;

type level = Lwt_log_core.level =
  | Debug
      (** Debugging message. They can be automatically removed by the
          syntax extension. *)
  | Info
      (** Informational message. Suitable to be displayed when the
          program is in verbose mode. *)
  | Notice
      (** Same as {!Info}, but is displayed by default. *)
  | Warning
      (** Something strange happend *)
  | Error
      (** An error message, which should not means the end of the
          program. *)
  | Fatal

type template = Lwt_log.template;
let default_template : template;

let level_encoding : level Data_encoding.t;

module Output : {
  type t =
    | Null
    | Stdout
    | Stderr
    | File of string
    | Syslog of Lwt_log.syslog_facility

  let encoding : t Data_encoding.t;
  let of_string : string => t option;
  let to_string : t => string;
  let pp : Format.formatter => t => unit;
};


let init: ?template:template => Output.t => unit Lwt.t;

let sections : string list ref;
