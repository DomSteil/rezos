/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

let mli = open_out Sys.argv.(1)

let dump_file oc file =
  let ic = open_in file;
  let buf = Bytes.create 256;
  let rec loop () =
    let len = input ic buf 0 (Bytes.length buf);
    if len <> 0 then (output oc buf 0 len; loop ())
  loop ();
  close_in ic

let included = ["Pervasives"];

let () =
  Printf.fprintf mli
    "module Make(Param : sig val name: string end)() : sig\n"


let () =
  for i = 2 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i);
    let unit =
      String.capitalize_ascii
        (Filename.chop_extension (Filename.basename file)) in
    if List.mem unit included then begin
      Printf.fprintf mli "# 1 %S\n" file ;
      dump_file mli file
    ;
    Printf.fprintf mli "module %s : sig\n" unit;
    Printf.fprintf mli "# 1 %S\n" file ;
    dump_file mli file;
    Printf.fprintf mli "end\n";
    if unit = "Result" then begin
      Printf.fprintf mli
        "type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b\n";
    ;
  done


let () =
  Printf.fprintf mli {|
module type PACKED_PROTOCOL = sig
  let hash : Hash.Protocol_hash.t
  include Updater.PROTOCOL
  let error_encoding : error Data_encoding.t
  let classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
  let pp : Format.formatter -> error -> unit
  let complete_b58prefix : Context.t -> string -> string list Lwt.t
val __cast: (module PACKED_PROTOCOL) -> (module Protocol.PACKED_PROTOCOL)
|}

let () =
  Printf.fprintf mli "end\n" ;
  close_out mli
