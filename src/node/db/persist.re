/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

/*  Tezos - Persistent structures on top of {!Store} or {!Context} */

open Lwt.Infix

/*-- Signatures --------------------------------------------------------------*/

type key = string list;
type value = MBytes.t;

module type STORE = {
  type t;
  let mem: t => key => bool Lwt.t;
  let dir_mem: t => key => bool Lwt.t;
  let get: t => key => value option Lwt.t;
  let set: t => key => value => t Lwt.t;
  let del: t => key -> t Lwt.t;
  let list: t => key list => key list Lwt.t;
  let remove_rec: t => key => t Lwt.t;
};

module type BYTES_STORE = {
  type t;
  type key;
  let mem: t => key => bool Lwt.t;
  let get: t => key => value option Lwt.t;
  let set: t => key => value => t Lwt.t;
  let del: t => key => t Lwt.t;
  let list: t => key list => key list Lwt.t;
  let remove_rec: t => key => t Lwt.t;
};

module type TYPED_STORE = {
  type t;
  type key;
  type value;
  let mem: t => key => bool Lwt.t;
  let get: t => key => value option Lwt.t;
  let set: t => key => value => t Lwt.t;
  let del: t => key => t Lwt.t;
};

module type KEY = {
  type t;
  let prefix: key;
  let length: int;
  let to_path: t => key;
  let of_path: key => t;
  let compare: t => t => int;
};

module type VALUE = {
  type t;
  let of_bytes: value => t option;
  let to_bytes: t => value;
};

module type PERSISTENT_SET = {
  type t and key;
  let mem : t => key => bool Lwt.t;
  let set : t => key => t Lwt.t;
  let del : t => key => t Lwt.t;
  let elements : t => key list Lwt.t;
  let clear : t => t Lwt.t;
  let iter : t => f:(key => unit Lwt.t) => unit Lwt.t;
  let fold : t => 'a => f:(key => 'a -> 'a Lwt.t) => 'a Lwt.t;
};

module type BUFFERED_PERSISTENT_SET = {
  include PERSISTENT_SET
  module Set : Set.S with type elt = key
  let read : t => Set.t Lwt.t;
  let write : t => Set.t => t Lwt.t;
};

module type PERSISTENT_MAP = {
  type t and key and value;
  let mem : t => key => bool Lwt.t;
  let get : t => key => value option Lwt.t;
  let set : t => key => value => t Lwt.t;
  let del : t => key => t Lwt.t;
  let bindings : t => (key * value) list Lwt.t;
  let clear : t => t Lwt.t;
  let iter : t => f:(key => value => unit Lwt.t) => unit Lwt.t;
  let fold : t => 'a => f:(key => value => 'a => 'a Lwt.t) => 'a Lwt.t;
};

module type BUFFERED_PERSISTENT_MAP = {
  include PERSISTENT_MAP
  module Map : Map.S with type key = key
  let read : t => value Map.t Lwt.t;
  let write : t => value Map.t -> t Lwt.t;
};

/*-- Utils -------------------------------------------------------------------*/

let prefix prf key =
  prf @ key;

let unprefix prf key =
  let rec eat = function
    | k :: key, p :: prefix =>
        assert (k = p) ;
        eat (key, prefix)
    | key, [] => key
    | _ => assert false in
  eat (key, prf);

/*-- Typed Store Overlays ----------------------------------------------------*/

module MakeBytesStore
    (S : STORE) (K : KEY) = {

  type t = S.t;
  type key = K.t;
  type value = MBytes.t;

  let to_path k =
    let suffix = K.to_path k;
    prefix K.prefix suffix;
  let of_path k = K.of_path (unprefix K.prefix k);

  let mem s k = S.mem s (to_path k);

  let get s k = S.get s (to_path k);

  let set s k v = S.set s (to_path k) v;

  let del s k = S.del s (to_path k);

  let list s l = S.list s (List.map to_path l) >>= fun res =>
    Lwt.return (List.map of_path res);

  let remove_rec s k = S.remove_rec s (to_path k);
};

module MakeTypedStore
    (S : STORE) (K : KEY) (C : VALUE) = {

  type t = S.t;
  type key = K.t;
  type value = C.t;

  module S = MakeBytesStore (S) (K)

  let mem = S.mem;
  let get s k =
    S.get s k >>= function
    | None => Lwt.return None
    | Some v => Lwt.return (C.of_bytes v);
  let set s k v = S.set s k (C.to_bytes v);
  let del = S.del;

  let raw_get = S.get;

};

module RawKey = {
  type t = key;;
  let prefix = [];
  let length = 0;
  let to_path p = p;
  let of_path p = p;
  let compare pa pb = Pervasives.compare pa pb;
};
module RawValue = {
  type t = value;
  let to_bytes b = b;
  let of_bytes b = Some b;
};

/*-- Set Builders ------------------------------------------------------------*/

module MakePersistentSet
    (S : STORE) (K : KEY) = {

  let to_path k =
    let suffix = K.to_path k;
    assert (List.length suffix = K.length) ;
    prefix K.prefix suffix;

  let of_path p = K.of_path (unprefix K.prefix p);

  let empty =
    MBytes.of_string "";

  let inited_key =
    prefix K.prefix [ "inited" ];

  let mem c k =
    S.mem c (to_path k);

  let set c k =
    S.set c inited_key empty >>= fun c ->
    S.set c (to_path k) empty;

  let del c k =
    S.del c (to_path k);

  let clear c =
    S.remove_rec c K.prefix;

  let fold c x ~f =
    let rec dig i root acc =
      if root = inited_key then
        Lwt.return acc
      else if i <= 0 then
        f (of_path root) acc
      else
        S.list c [root] >>= fun roots =>
        Lwt_list.fold_right_s (dig (i - 1)) roots acc in
    S.mem c inited_key >>= function
    | true => dig K.length K.prefix x
    | false => Lwt.return x;

  let iter c ~f = fold c () (fun x () => f x);
  let elements c = fold c [] (fun p xs => Lwt.return (p :: xs));

};

module MakeBufferedPersistentSet
    (S : STORE) (K : KEY) (Set : Set.S with type elt = K.t) = {

  include MakePersistentSet(S)(K)

  let read c =
    fold c Set.empty (fun p set => Lwt.return (Set.add p set));

  let write c set =
    S.set c inited_key empty >>= fun c =>
    read c >>= fun old_set =>
    Lwt_list.fold_left_s
      (fun c h => S.del c (to_path h))
      c Set.(elements (diff old_set set)) >>= fun c =>
    Lwt_list.fold_left_s
      (fun c h => S.set c (to_path h) empty)
      c Set.(elements (diff set old_set));

};

/*-- Map Builders ------------------------------------------------------------*/

module MakePersistentMap
    (S : STORE) (K : KEY) (C : VALUE) = {

  let to_path k =
    let suffix = K.to_path k in
    assert (List.length suffix = K.length) ;
    prefix K.prefix suffix;

  let of_path p = K.of_path (unprefix K.prefix p);

  let empty =
    MBytes.of_string "";

  let inited_key =
    prefix K.prefix [ "inited" ];

  let mem c k =
    S.mem c (to_path k);

  let get c k =
    S.get c (to_path k) >|= function
    | None => None
    | Some b => C.of_bytes b;

  let set c k b =
    S.set c inited_key empty >>= fun c =>
    S.set c (to_path k) (C.to_bytes b);

  let del c k =
    S.del c (to_path k);

  let clear c =
    S.remove_rec c K.prefix;

  let fold c x ~f =
    let rec dig i root acc =
      if root = inited_key then
        Lwt.return acc
      else if i <= 0 then
        S.get c root >>= function
        | None => Lwt.return acc
        | Some b =>
            match C.of_bytes b with
            | None => Lwt.return acc
            | Some v => f (of_path root) v acc
      else
        S.list c [root] >>= fun roots =>
        Lwt_list.fold_right_s (dig (i - 1)) roots acc in
    S.mem c inited_key >>= function
    | true => dig K.length K.prefix x
    | false => Lwt.return x

  let iter c ~f = fold c () (fun k v () => f k v)
  let bindings c = fold c [] (fun k v acc => Lwt.return ((k, v) :: acc))

};

module MakeBufferedPersistentMap
    (S : STORE) (K : KEY) (C : VALUE) (Map : Map.S with type key = K.t) = {

  include MakePersistentMap(S)(K)(C)

  let read c = fold c Map.empty (fun k v m -> Lwt.return (Map.add k v m));

  let write c m =
    clear c >>= fun c =>
    S.set c inited_key empty >>= fun c =>
    Lwt_list.fold_left_s
      (fun c (k, b) => S.set c (to_path k) (C.to_bytes b))
      c (Map.bindings m)

};

/*-- Imperative overlays ----------------------------------------------------*/

type 'a shared_ref =
  { mutable contents : 'a ;
    lock : Lwt_mutex.t };
let share contents =
  { contents ;
    lock = Lwt_mutex.create () };
let update r f =
  Lwt_mutex.with_lock r.lock
    (fun () => f r.contents >>= function
       | None => Lwt.return false
       | Some new_contents =>
           r.contents <- new_contents ;
           Lwt.return true);
let update_with_res r f =
  Lwt_mutex.with_lock r.lock
    (fun () => f r.contents >>= function
       | (None, x) => Lwt.return (false, x)
       | (Some new_contents, x) =>
           r.contents <- new_contents ;
           Lwt.return (true, x));
let use r f =
  Lwt_mutex.with_lock r.lock
    (fun () => f r.contents);

module type IMPERATIVE_PROXY = {
  module Store : TYPED_STORE {

  type t;
  type rdata;
  type state;
  let create: state => Store.t shared_ref => t;
  let known: t => Store.key => bool Lwt.t;
  let read: t => Store.key => Store.value option Lwt.t;
  let store: t => Store.key => Store.value => bool Lwt.t;
  let update: t => Store.key => Store.value => bool Lwt.t;
  let remove: t => Store.key => bool Lwt.t;
  let prefetch: t => rdata => Store.key => unit;
  let fetch: t => rdata => Store.key => Store.value Lwt.t;
  let pending: t => Store.key => bool;
  let shutdown: t => unit Lwt.t;
};
};
module type IMPERATIVE_PROXY_SCHEDULER = {
  module Store : TYPED_STORE {
  type state;
  type rdata;
  type data;

  let name : string;
  let init_request :
    state => Store.key => data Lwt.t;
  let request :
    state =>
    get:(rdata => Store.key => Store.value Lwt.t) =>
    set:(Store.key => Store.value => unit Lwt.t) =>
    (Store.key * data * rdata) list => float;
};

module MakeImperativeProxy
    (Store : TYPED_STORE)
    (Table : Hashtbl.S with type key = Store.key)
    (Scheduler : IMPERATIVE_PROXY_SCHEDULER with module Store := Store)
  : IMPERATIVE_PROXY with module Store := Store and type state = Scheduler.state and type rdata = Scheduler.rdata = {

  type rdata = Scheduler.rdata;
  type data =
    { rdata: rdata ;
      state: [ `Inited of Scheduler.data | `Initing of Scheduler.data Lwt.t ] ;
      wakener: Store.value Lwt.u }
  type store = Store.t;
  type state = Scheduler.state;
  type key = Store.key;
  type value = Store.value;

  type t =
    { tbl : data Table.t ;
      store : Store.t shared_ref ;
      cancelation: unit => unit Lwt.t ;
      cancel: unit => unit Lwt.t ;
      on_cancel: (unit => unit Lwt.t) => unit ;
      worker_trigger: unit => unit;
      worker_waiter: unit => unit Lwt.t ;
      worker: unit Lwt.t ;
      gstate : state }

  let pending_requests { tbl } =
    Table.fold
      (fun h data acc =>
         match data.state with
         | `Initing _ => acc
         | `Inited d => (h, d, data.rdata) :: acc)
      tbl [];

  let pending { tbl } hash = Table.mem tbl hash;

  let request { tbl , worker_trigger, gstate } rdata hash =
    assert (not (Table.mem tbl hash));
    let waiter, wakener = Lwt.wait ();
    let data = Scheduler.init_request gstate hash;
    switch Lwt.state data with
    | Lwt.Return data =>
        let state = `Inited data;
        Table.add tbl hash { rdata, state, wakener } ;
        worker_trigger () ;
        waiter
    | _ =>
        let state = `Initing data;
        Table.add tbl hash { rdata , state, wakener } ;
        Lwt.async
          (fun () =>
             data >>= fun data =>
             let state = `Inited data in
             Table.add tbl hash { rdata, state, wakener } ;
             worker_trigger () ;
             Lwt.return_unit) ;
        waiter

  let prefetch ({ store, tbl } as session) rdata hash =
    Lwt.ignore_result
      (use store (fun store => Store.mem store hash) >>= fun exists =>
       if not exists && not (Table.mem tbl hash) then
         request session rdata hash >>= fun _ => Lwt.return_unit
       else
         Lwt.return_unit);

  let known { store } hash =
    use store (fun store => Store.mem store hash);

  let read { store } hash =
    use store (fun store => Store.get store hash);

  let fetch ({ store, tbl } as session) rdata hash =
    try Lwt.waiter_of_wakener (Table.find tbl hash).wakener
    with Not_found =>
      use store (fun store => Store.get store hash) >>= function
      | Some op => Lwt.return op
      | None =>
          try Lwt.waiter_of_wakener (Table.find tbl hash).wakener
          with Not_found => request session rdata hash;

  let store { store ; tbl } hash data =
    update store (fun store =>
        Store.mem store hash >>= fun exists =>
        if exists then Lwt.return_none
        else ( Store.set store hash data >>= fun store =>
               Lwt.return (Some store) ) ) >>= fun changed =>
    try
      let wakener = (Table.find tbl hash).wakener in
      Table.remove tbl hash;
      Lwt.wakeup wakener data;
      Lwt.return changed
    with Not_found => Lwt.return changed;

  let remove { store ; _ } hash =
    update store (fun store =>
        Store.mem store hash >>= fun exists =>
        if not exists then Lwt.return_none
        else ( Store.del store hash >>= fun store =>
               Lwt.return (Some store) ) );

  let update { store ; _ } hash data =
    update store (fun store =>
        Store.mem store hash >>= fun exists =>
        if not exists then Lwt.return_none
        else ( Store.set store hash data >>= fun store =>
               Lwt.return (Some store) ) );

  let create gstate st =
    let tbl = Table.create 50;
    let cancelation, cancel, on_cancel = Lwt_utils.canceler ();
    let worker_trigger, worker_waiter = Lwt_utils.trigger ();
    let session =
      { tbl ; gstate ; store = st ; worker = Lwt.return () ;
        cancelation ; cancel ; on_cancel ;
        worker_trigger ; worker_waiter };
    let worker =
      let rec worker_loop () =
        Lwt.pick [(worker_waiter () >|= fun () => `Process);
                  (cancelation () >|= fun () => `Cancel)] >>= function
        | `Cancel => Lwt.return_unit
        | `Process =>
            begin
              match pending_requests session with
              | [] => ()
              | requests =>
                  let get = fetch session
                  and set k v = store session k v >>= fun _ -> Lwt.return_unit in
                  let timeout = Scheduler.request gstate ~get ~set requests in
                  if timeout > 0. then
                    Lwt.ignore_result (Lwt_unix.sleep timeout >|= worker_trigger);
            end;
            worker_loop ()
      in
      Lwt_utils.worker Scheduler.name ~run:worker_loop ~cancel in
    { session with worker }

  let shutdown { cancel ; worker } =
    cancel () >>= fun () => worker;

};

/*-- Predefined Instances ----------------------------------------------------*/

module MBytesValue = {
  type t = MBytes.t;
  let of_bytes x = Some x;
  let to_bytes x = x;
};

module MakePersistentBytesMap
    (S : STORE) (K : KEY) =
  MakePersistentMap(S)(K)(MBytesValue)

module MakeBufferedPersistentBytesMap
    (S : STORE) (K : KEY) (Map : Map.S with type key = K.t) =
  MakeBufferedPersistentMap(S)(K)(MBytesValue)(Map)

module type TYPED_VALUE_REPR = {
  type value;
  val encoding: value Data_encoding.t
};

module TypedValue (T : TYPED_VALUE_REPR) = {
  type t = T.value;
  let of_bytes x = Data_encoding.Binary.of_bytes T.encoding x;
  let to_bytes x = Data_encoding.Binary.to_bytes T.encoding x;
};

module MakePersistentTypedMap
    (S : STORE)    (K : KEY)
    (T : TYPED_VALUE_REPR) =
  MakePersistentMap(S)(K)(TypedValue(T))

module MakeBufferedPersistentTypedMap
    (S : STORE)
    (K : KEY)
    (T : TYPED_VALUE_REPR)
    (Map : Map.S with type key = K.t)
  =
  MakeBufferedPersistentMap(S)(K)(TypedValue(T))(Map)

module MakeHashResolver
    (Store : {
       type t;
       let dir_mem: t => string list => bool Lwt.t;
       let list: t => string list list => string list list Lwt.t;
       let prefix: string list;
     end)
    (H: HASH) = struct
  let plen = List.length Store.prefix;
  let build path =
    H.of_path_exn @@
    Utils.remove_elem_from_list plen path
  let resolve t p =
    let rec loop prefix = function
      | [] ->
          Lwt.return [build prefix]
      | "" :: ds =>
          Store.list t [prefix] >>= fun prefixes =>
          Lwt_list.map_p (fun prefix => loop prefix ds) prefixes
          >|= List.flatten
      | [d] =>
          Store.list t [prefix] >>= fun prefixes =>
          Lwt_list.filter_map_p (fun prefix =>
              match remove_prefix d (List.hd (List.rev prefix)) with
              | None => Lwt.return_none
              | Some _ => Lwt.return (Some (build prefix))
            ) prefixes
      | d :: ds =>
          Store.dir_mem t (prefix @ [d]) >>= function
          | true => loop (prefix @ [d]) ds
          | false => Lwt.return_nil in
    loop Store.prefix (H.prefix_path p)
};
