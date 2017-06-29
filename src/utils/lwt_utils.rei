/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/
let may: f:('a => unit Lwt.t) => 'a option => unit Lwt.t;

let never_ending: 'a Lwt.t;

let canceler : unit =>
  (unit => unit Lwt.t) *
  (unit => unit Lwt.t) *
  ((unit => unit Lwt.t) => unit);

module Canceler : {

  type t;
  let create : unit => t;
  let cancel : t => unit Lwt.t;
  let cancelation : t => unit Lwt.t;
  let on_cancel : t => (unit => unit Lwt.t) => unit;
  let canceled : t => bool;

};

module Idle_waiter : {

  type t;
  /** A lightweight scheduler to run tasks concurrently as well as
      special callbacks that must be run in mutual exclusion with the
      tasks (and each other). */

  let create : unit => t;
  /** Creates a new task / idle callback scheduler */

  let task : t => (unit => 'a Lwt.t) => 'a Lwt.t;
  /** Schedule a task to be run as soon as no idle callbacks is
      running, or as soon as the next idle callback has been run if it
      was scheduled by {!force_idle}. */

  let when_idle : t => (unit => 'a Lwt.t) => 'a Lwt.t;
  /** Runs a callback as soon as no task is running. Does not prevent
      new tasks from being scheduled, the calling code should ensure
      that some idle time will eventually come. Calling this function
      from inside the callback will result in a dead lock. */

  let force_idle : t => (unit => 'a Lwt.t) => 'a Lwt.t;
  /** Runs a callback as soon as possible. Lets all current tasks
      finish, but postpones all new tasks until the end of the
      callback. Calling this function from inside the callback will
      result in a dead lock. */

};

let worker:
  string =>
  run:(unit => unit Lwt.t) =>
  cancel:(unit => unit Lwt.t) =>
  unit Lwt.t;

let trigger: unit => (unit => unit) * (unit => unit Lwt.t);
let queue: unit => ('a => unit) * (unit => 'a list Lwt.t);
let sort: ('a => 'a => int Lwt.t) => 'a list => 'a list Lwt.t;

let read_bytes:
  ?pos:int => ?len:int => Lwt_unix.file_descr => bytes => unit Lwt.t;

let read_mbytes:
  ?pos:int => ?len:int => Lwt_unix.file_descr => MBytes.t => unit Lwt.t;

let write_bytes:
  ?pos:int => ?len:int => Lwt_unix.file_descr => bytes => unit Lwt.t;
let write_mbytes:
  ?pos:int => ?len:int => Lwt_unix.file_descr => MBytes.t => unit Lwt.t;

let remove_dir: string => unit Lwt.t;
let create_dir: ?perm:int => string => unit Lwt.t;
let create_file: ?perm:int => string => string => unit Lwt.t;

let safe_close: Lwt_unix.file_descr => unit Lwt.t;

open Error_monad;

type error += Canceled;
let protect :
  ?on_error:(error list => 'a tzresult Lwt.t) =>
  ?canceler:Canceler.t =>
  (unit => 'a tzresult Lwt.t) => 'a tzresult Lwt.t;

type error += Timeout;
let with_timeout:
  ?canceler:Canceler.t =>
  float => (Canceler.t => 'a tzresult Lwt.t) => 'a tzresult Lwt.t;

let unless: bool => (unit => unit Lwt.t) => unit Lwt.t;

module Lock_file : {
  let create :
    ?close_on_exec:bool =>
    ?unlink_on_exit:bool =>
    string => unit tzresult Lwt.t;

  let blocking_create :
    ?timeout:float =>
    ?close_on_exec:bool =>
    ?unlink_on_exit:bool =>
    string => unit tzresult Lwt.t;

  let is_locked : string => bool tzresult Lwt.t;
  let get_pid : string => int tzresult Lwt.t;
};

let getaddrinfo:
  passive:bool =>
  node:string => service:string =>
  (Ipaddr.V6.t * int) list Lwt.t;
