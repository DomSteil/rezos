/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Store_sigs

type t;
type global_store = t;

/** Open or initialize a store at a given path. */
let init: string => t tzresult Lwt.t;
let close : t => unit;


/** {2 Net store} ************************************************************/

module Net : {

  let list: global_store -> Net_id.t list Lwt.t;
  let destroy: global_store -> Net_id.t -> unit Lwt.t;

  type store;
  let get: global_store -> Net_id.t -> store;

  module Genesis_hash : SINGLE_STORE
    with type t := store
     and type value := Block_hash.t;

  module Genesis_time : SINGLE_STORE
    with type t := store
     and type value := Time.t;

  module Genesis_protocol : SINGLE_STORE
    with type t := store
     and type value := Protocol_hash.t;

  module Genesis_test_protocol : SINGLE_STORE
    with type t := store
     and type value := Protocol_hash.t;

  module Expiration : SINGLE_STORE
    with type t := store
     and type value := Time.t;

  module Allow_forked_network : SET_STORE
    with type t := t
     and type elt := Net_id.t;

};


/** {2 Chain data} ***********************************************************/

module Chain : {

  type store;
  let get: Net.store => store;

  module Current_head : SINGLE_STORE
    with type t := store
     and type value := Block_hash.t;

  module Known_heads : BUFFERED_SET_STORE
    with type t := store;
     and type elt := Block_hash.t;
     and module Set := Block_hash.Set;

  module Valid_successors : BUFFERED_SET_STORE
    with type t = store * Block_hash.t;
     and type elt := Block_hash.t;
     and module Set := Block_hash.Set;

  module Invalid_successors : BUFFERED_SET_STORE
    with type t = store * Block_hash.t;
     and type elt := Block_hash.t;
     and module Set := Block_hash.Set;

  module Successor_in_chain : SINGLE_STORE
    with type t = store * Block_hash.t;
     and type value := Block_hash.t;;

  module In_chain_insertion_time : SINGLE_STORE
    with type t = store * Block_hash.t;
     and type value := Time.t;

};


/** {2 Generic signature} *****************************************************/

/** Generic signature for Operations, Block_header, and Protocol "tracked"
    contents (i.e. with 'discovery_time', 'validtity', ...) */
module type DATA_STORE = {

  type store;
  type key;
  type key_set;
  type value;

  let encoding: value Data_encoding.t;

  let compare: value => value => int;
  let equal: value => value => bool;

  let hash: value => key;
  let hash_raw: MBytes.t => key;

  module Discovery_time : MAP_STORE
    with type t := store;
     and type key := key;
     and type value := Time.t;

  module Contents : SINGLE_STORE
    with type t = store * key;
     and type value := value;

  module RawContents : SINGLE_STORE
    with type t = store * key;
     and type value := MBytes.t;

  module Validation_time : SINGLE_STORE
    with type t = store * key;
     and type value := Time.t;

  module Errors : MAP_STORE
    with type t := store;
     and type key := key;
     and type value = error list;

  module Pending : BUFFERED_SET_STORE
    with type t = store;
     and type elt := key;
     and type Set.t = key_set;

};


/** {2 Operation store} *****************************************************/

module Operation : {

  type shell_header = {
    net_id: Net_id.t ;
  }
  let shell_header_encoding: shell_header Data_encoding.t;

  type t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  type store
  let get: Net.store -> store;

  include DATA_STORE
    with type store := store;
     and type key = Operation_hash.t;
     and type value = t;
     and type key_set = Operation_hash.Set.t;

};


/** {2 Block header store} **************************************************/

module Block_header : {

  type shell_header = {
    net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }
  let shell_header_encoding: shell_header Data_encoding.t;

  type t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  type store
  let get: Net.store -> store;

  include DATA_STORE
    with type store := store;
     and type key = Block_hash.t;
     and type value = t;
     and type key_set = Block_hash.Set.t;

  module Operation_list_count : SINGLE_STORE
    with type t = store * Block_hash.t;
     and type value = int;

  module Operation_list : MAP_STORE
    with type t = store * Block_hash.t;
     and type key = int;
     and type value = Operation_hash.t list;

  module Operation_list_path : MAP_STORE
    with type t = store * Block_hash.t;
     and type key = int;
     and type value = Operation_list_list_hash.path;

};


/** {2 Protocol store} ******************************************************/

module Protocol : {

  type t = Tezos_compiler.Protocol.t;

  type store;
  let get: global_store -> store;

  include DATA_STORE
    with type store := store;
     and type key = Protocol_hash.t;
     and type value = t;
     and type key_set = Protocol_hash.Set.t;

};
