/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2014 - 2016.                                          */
/*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

open Error_monad

let (//) = Filename.concat
let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

open Error_monad
open Utils

let () =
  let expected_primitive = "blake2b"
  and primitive = Sodium.Generichash.primitive in
  if primitive <> expected_primitive then begin
    Printf.eprintf
      "FATAL ERROR: \
       invalid value for Sodium.Generichash.primitive: %S (expected %S)@."
      primitive expected_primitive ;
    exit 1
  end

/*-- Signatures -------------------------------------------------------------*/

module type MINIMAL_HASH = {

  type t;

  let name: string;
  let title: string;

  let hash_bytes: MBytes.t list => t;
  let hash_string: string list => t;
  let size: int (* in bytes *);
  let compare: t => t => int;
  let equal: t => t => bool;

  let to_hex: t => string;
  let of_hex: string => t option;
  let of_hex_exn: string => t;

  let to_string: t => string;
  let of_string: string => t option;
  let of_string_exn: string => t;

  let to_bytes: t => MBytes.t;
  let of_bytes: MBytes.t => t option;
  let of_bytes_exn: MBytes.t => t;

  let read: MBytes.t => int => t;
  let write: MBytes.t => int => t => unit;

  let to_path: t => string list;
  let of_path: string list => t option;
  let of_path_exn: string list => t;

  let prefix_path: string => string list;
  let path_length: int;

};

module type INTERNAL_MINIMAL_HASH = {
  include MINIMAL_HASH
  module Table : Hashtbl.S with type key = t
};

module type HASH = {

  include MINIMAL_HASH

  let of_b58check_exn: string => t;
  let of_b58check_opt: string => t option;
  let to_b58check: t => string;
  let to_short_b58check: t => string;
  let encoding: t Data_encoding.t;
  let pp: Format.formatter => t => unit;
  let pp_short: Format.formatter => t => unit;
  type Base58.data += Hash of t;
  let b58check_encoding: t Base58.encoding;

  module Set : {
    include Set.S with type elt = t
    let encoding: t Data_encoding.t
  };

  module Map : {
    include Map.S with type key = t
    let encoding: 'a Data_encoding.t => 'a t Data_encoding.t
  };

};

module type INTERNAL_HASH = {
  include HASH
  let of_b58check: string => t tzresult;
  let param:
    ?name:string =>
    ?desc:string =>
    ('a, 'arg, 'ret) Cli_entries.params =>
    (t => 'a, 'arg, 'ret) Cli_entries.params;
  module Table : Hashtbl.S with type key = t;
};

/*Internal Merkle Tree Vriant Data Type*/
module type INTERNAL_MERKLE_TREE = {
  type elt;
  include INTERNAL_HASH
  let compute: elt list => t;
  let empty: t;
  type path =
    | Left of path * t   /*0*/
    | Right of t * path  /*1*/
    | Op
  let compute_path: elt list => int => path;
  let check_path: path => elt => t * int;
  let path_encoding: path Data_encoding.t;
};


/* Merkle Tree Variant Data Type */
module type MERKLE_TREE = {
  type elt;
  include HASH
  let compute: elt list => t;
  let empty: t;
  type path =
    | Left of path * t  /*0*/
    | Right of t * path  /*1*/
    | Op
  let compute_path: elt list => int => path;
  let check_path: path => elt => t * int;
  let path_encoding: path Data_encoding.t;
};

module type Name = {
  let name: string;
  let title:  string;
  let size: int option;
};

module type PrefixedName = {
  include Name
  let b58check_prefix: string;
};

/*-- Type specific Hash builder ---------------------------------------------*/

module Make_minimal_Blake2B (K : Name) = {

  type t = Sodium.Generichash.hash;

  include K

  let size =
    switch K.size with
    | None => 32
    | Some x => x

  let of_string s =
    if String.length s <> size then
      None
    else
      Some (Sodium.Generichash.Bytes.to_hash (Bytes.of_string s))
  let of_string_exn s =
    match of_string s with
    | None =>
        let msg =
          Printf.sprintf "%s.of_string: wrong string size (%d)"
            K.name (String.length s) in
        raise (Invalid_argument msg)
    | Some h => h
  let to_string s = Bytes.to_string (Sodium.Generichash.Bytes.of_hash s)

  let of_hex s = of_string (Hex_encode.hex_decode s)
  let of_hex_exn s = of_string_exn (Hex_encode.hex_decode s)
  let to_hex s = Hex_encode.hex_encode (to_string s)

  let compare = Sodium.Generichash.compare
  let equal x y = compare x y = 0

  let of_bytes b =
    if MBytes.length b <> size then
      None
    else
      Some (Sodium.Generichash.Bigbytes.to_hash b)
  let of_bytes_exn b =
    match of_bytes b with
    | None =>
        let msg =
          Printf.sprintf "%s.of_bytes: wrong string size (%d)"
            K.name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h => h
  let to_bytes = Sodium.Generichash.Bigbytes.of_hash

  let read src off = of_bytes_exn @@ MBytes.sub src off size
  let write dst off h = MBytes.blit (to_bytes h) 0 dst off size

  let hash_bytes l =
    let open Sodium.Generichash in
    let state = init ~size () in
    List.iter (Bigbytes.update state) l ;
    final state

  let hash_string l =
    let open Sodium.Generichash in
    let state = init ~size () in
    List.iter
      (fun s => Bytes.update state (BytesLabels.unsafe_of_string s))
      l ;
    final state

  let fold_read f buf off len init =
    let last = off + len * size in
    if last > MBytes.length buf then
      invalid_arg "Hash.read_set: invalid size.";
    let rec loop acc off =
      if off >= last then
        acc
      else
        let hash = read buf off in
        loop (f hash acc) (off + size)
    in
    loop init off

  let path_length = 6
  let to_path key =
    let key = to_hex key in
    [ String.sub key 0 2 ; String.sub key 2 2 ;
      String.sub key 4 2 ; String.sub key 6 2 ;
      String.sub key 8 2 ; String.sub key 10 (size * 2 - 10) ]
  let of_path path =
    let path = String.concat "" path in
    of_hex path
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn path

  let prefix_path p =
    let p = Hex_encode.hex_encode p in
    let len = String.length p in
    let p1 = if len >= 2 then String.sub p 0 2 else ""
    and p2 = if len >= 4 then String.sub p 2 2 else ""
    and p3 = if len >= 6 then String.sub p 4 2 else ""
    and p4 = if len >= 8 then String.sub p 6 2 else ""
    and p5 = if len >= 10 then String.sub p 8 2 else ""
    and p6 = if len > 10 then String.sub p 10 (len - 10) else "" in
    [ p1 ; p2 ; p3 ; p4 ; p5 ; p6 ]

  module Table = {
    include Hashtbl.Make({
      type nonrec t = t
      let hash s =
        Int64.to_int
          (EndianString.BigEndian.get_int64
             (Bytes.unsafe_to_string (Sodium.Generichash.Bytes.of_hash s))
             0)
      let equal = equal
    })
  };

};

module Make_Blake2B (R : {
    val register_encoding:
      prefix: string =>
      length:int =>
      to_raw: ('a => string) =>
      of_raw: (string => 'a option) =>
      wrap: ('a => Base58.data) =>
      'a Base58.encoding
  end) (K : PrefixedName) = struct

  include Make_minimal_Blake2B(K)

  (* Serializers *)

  type Base58.data += Hash of t

  let b58check_encoding =
    R.register_encoding
      ~prefix: K.b58check_prefix
      ~length:size
      ~wrap: (fun s => Hash s)
      ~of_raw:(fun h => of_string h) ~to_raw:to_string

  let of_b58check_opt s =
    Base58.simple_decode b58check_encoding s
  let of_b58check_exn s =
    switch Base58.simple_decode b58check_encoding s with
    | Some x => x
    | None => Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" K.name
  let of_b58check s =
    switch Base58.simple_decode b58check_encoding s with
    | Some x => Ok x
    | None => generic_error "Unexpected hash (%s)" K.name
  let to_b58check s = Base58.simple_encode b58check_encoding s

  let to_short_b58check s =
    String.sub (to_b58check s) 0 (10 + 2 * String.length K.b58check_prefix)

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (conv to_bytes of_bytes_exn (Fixed.bytes size))
      ~json:
        (describe ~title: (K.title ^ " (Base58Check-encoded Sha256)") @@
         conv to_b58check (Data_encoding.Json.wrap_error of_b58check_exn) string)

  let param ?(name=K.name) ?(desc=K.title) t =
    Cli_entries.param ~name ~desc (fun _ str => Lwt.return (of_b58check str)) t

  let pp ppf t =
    Format.pp_print_string ppf (to_b58check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b58check t)

  module Set = {
    include Set.Make(struct type nonrec t = t let compare = compare end)
    let encoding =
      Data_encoding.conv
        elements
        (fun l => List.fold_left (fun m x => add x m) empty l)
        Data_encoding.(list encoding)
  };

  module Map = {
    include Map.Make(struct type nonrec t = t let compare = compare end)
    let encoding arg_encoding =
      Data_encoding.conv
        bindings
        (fun l => List.fold_left (fun m (k,v) => add k v m) empty l)
        Data_encoding.(list (tup2 encoding arg_encoding))
  };

};

module Generic_Merkle_tree (H : {
    type t;
    type elt;
    let encoding : t Data_encoding.t;
    let empty : t;
    let leaf : elt => t;
    let node : t => t => t;
  }) = {

  let rec step a n =
    let m = (n+1) / 2 in
    for i = 0 to m - 1 do
      a.(i) <- H.node a.(2*i) a.(2*i+1)
    done ;
    a.(m) <- H.node a.(n) a.(n) ;
    if m = 1 then
      a.(0)
    else if m mod 2 = 0 then
      step a m
    else begin
      a.(m+1) <- a.(m) ;
      step a (m+1)
    };

  let empty = H.empty

  let compute xs =
    match xs with
    | [] => H.empty
    | [x] => H.leaf x
    | _ :: _ :: _ =>
        let last = Utils.list_last_exn xs in
        let n = List.length xs in
        let a = Array.make (n+1) (H.leaf last) in
        List.iteri (fun i x => a.(i) <- H.leaf x) xs ;
        step a n

  type path =
    | Left of path * H.t
    | Right of H.t * path
    | Op

  let rec step_path a n p j =
    let m = (n+1) / 2 in
    let p = if j mod 2 = 0 then Left (p, a.(j+1)) else Right (a.(j-1), p) in
    for i = 0 to m - 1 do
      a.(i) <- H.node a.(2*i) a.(2*i+1)
    done ;
    a.(m) <- H.node a.(n) a.(n) ;
    if m = 1 then
      p
    else if m mod 2 = 0 then
      step_path a m p (j/2)
    else begin
      a.(m+1) <- a.(m) ;
      step_path a (m+1) p (j/2)
    end

  let compute_path xs i =
    match xs with
    | [] => invalid_arg "compute_path"
    | [_] => Op
    | _ :: _ :: _ =>
        let last = Utils.list_last_exn xs in
        let n = List.length xs in
        if i < 0 || n <= i then invalid_arg "compute_path" ;
        let a = Array.make (n+1) (H.leaf last) in
        List.iteri (fun i x => a.(i) <- H.leaf x) xs ;
        step_path a n Op i

  let rec check_path p h =
    match p with
    | Op =>
        H.leaf h, 1, 0
    | Left (p, r) =>
        let l, s, pos = check_path p h in
        H.node l r, s * 2, pos
    | Right (l, p) =>
        let r, s, pos = check_path p h in
        H.node l r, s * 2, pos + s

  let check_path p h =
    let h, _, pos = check_path p h in
    h, pos

  let path_encoding =
    let open Data_encoding in
    mu "path"
      (fun path_encoding =>
         union [
           case ~tag:240
             (obj2
                (req "path" path_encoding)
                (req "right" H.encoding))
             (function Left (p, r) => Some (p, r) | _ => None)
             (fun (p, r) => Left (p, r)) ;
           case ~tag:15
             (obj2
                (req "left" H.encoding)
                (req "path" path_encoding))
             (function Right (r, p) => Some (r, p) | _ => None)
             (fun (r, p) => Right (r, p)) ;
           case ~tag:0
             unit
             (function Op => Some () | _ => None)
             (fun () => Op)
         ])

end

module Make_merkle_tree
    (R : {
       let register_encoding:
         prefix: string =>
         length:int =>
         to_raw: ('a => string) =>
         of_raw: (string => 'a option) =>
         wrap: ('a => Base58.data) =>
         'a Base58.encoding
     )
    (K : PrefixedName)
    (Contents: sig
       type t
       val to_bytes: t => MBytes.t
     }) =

  include Make_Blake2B (R) (K)

  type elt = Contents.t

  let empty = hash_bytes []

  include Generic_Merkle_tree(struct
      type nonrec t = t
      type nonrec elt = elt
      let encoding = encoding
      let empty = empty
      let leaf x = hash_bytes [Contents.to_bytes x]
      let node x y = hash_bytes [to_bytes x; to_bytes y]
    end)



/*-- Pre-instanciated hashes ------------------------------------------------*/

module Block_hash =
  Make_Blake2B (Base58) ({
    let name = "Block_hash";
    let title = "A Tezos block ID";
    let b58check_prefix = Base58.Prefix.block_hash;
    let size = None;
  })

module Operation_hash =
  Make_Blake2B (Base58) ({
    let name = "Operation_hash";
    let title = "A Tezos operation ID";
    let b58check_prefix = Base58.Prefix.operation_hash;
    let size = None;
  })

module Operation_list_hash =
  Make_merkle_tree (Base58) ({
    let name = "Operation_list_hash";
    let title = "A list of operations";
    let b58check_prefix = Base58.Prefix.operation_list_hash;
    let size = None;
  }) (Operation_hash)

module Operation_list_list_hash =
  Make_merkle_tree (Base58) ({
    let name = "Operation_list_list_hash";
    let title = "A list of list of operations";
    let b58check_prefix = Base58.Prefix.operation_list_list_hash;
    let size = None;
  }) (Operation_list_hash)

module Protocol_hash =
  Make_Blake2B (Base58) ({
    let name = "Protocol_hash";
    let title = "A Tezos protocol ID";
    let b58check_prefix = Base58.Prefix.protocol_hash;
    let size = None;
  })

module Generic_hash =
  Make_minimal_Blake2B ({
    let name = "Generic_hash";
    let title = "";
    let size = None;
  })

module Net_id = {

  type t = string;
  type net_id = t;

  let name = "Net_id";
  let title = "Network identifier";

  let size = 4;

  let of_block_hash bh =
    MBytes.substring (Block_hash.to_bytes bh) 0 4;

  let hash_bytes l = of_block_hash (Block_hash.hash_bytes l);
  let hash_string l = of_block_hash (Block_hash.hash_string l);

  type Base58.data += Hash of t;

  let of_string s =
    if String.length s <> size then None else Some s;
  let of_string_exn s =
    match of_string s with
    | None =>
        let msg =
          Printf.sprintf "%s.of_string: wrong string size (%d)"
            name (String.length s) in
        raise (Invalid_argument msg)
    | Some h => h
  let to_string s = s;

  let of_hex s = of_string (Hex_encode.hex_decode s);
  let of_hex_exn s = of_string_exn (Hex_encode.hex_decode s);
  let to_hex s = Hex_encode.hex_encode (to_string s);

  let compare = String.compare;
  let equal = String.equal;

  let of_bytes b =
    if MBytes.length b <> size then
      None
    else
      Some (MBytes.to_string b);
  let of_bytes_exn b =
    match of_bytes b with
    | None =>
        let msg =
          Printf.sprintf "%s.of_bytes: wrong string size (%d)"
            name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h => h;
  let to_bytes = MBytes.of_string;

  let read src off = of_bytes_exn @@ MBytes.sub src off size;
  let write dst off h = MBytes.blit (to_bytes h) 0 dst off size;

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.net_id
      ~length: size
      ~wrap: (fun s => Hash s)
      ~of_raw:of_string ~to_raw: (fun h => h);

  let of_b58check_opt s =
    Base58.simple_decode b58check_encoding s;
  let of_b58check_exn s =
    match Base58.simple_decode b58check_encoding s with
    | Some x => x
    | None => Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" name;
  let of_b58check s =
    match Base58.simple_decode b58check_encoding s with
    | Some x => Ok x
    | None => generic_error "Unexpected hash (%s)" name;
  let to_b58check s = Base58.simple_encode b58check_encoding s;
  let to_short_b58check = to_b58check;

  let encoding =
    let open Data_encoding in
    splitted
      ~binary: (Fixed.string size)
      ~json:
        (describe ~title: (title ^ " (Base58Check-encoded Sha256)") @@
         conv to_b58check (Data_encoding.Json.wrap_error of_b58check_exn) string);

  let param ?(name=name) ?(desc=title) t =
    Cli_entries.param ~name ~desc (fun _ str => Lwt.return (of_b58check str)) t;

  let pp ppf t =
    Format.pp_print_string ppf (to_b58check t);

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b58check t);

  module Set = {
    include Set.Make(struct type nonrec t = t let compare = compare end)
    let encoding =
      Data_encoding.conv
        elements
        (fun l => List.fold_left (fun m x => add x m) empty l)
        Data_encoding.(list encoding)
  };

  module Map = {
    include Map.Make(struct type nonrec t = t let compare = compare end)
    let encoding arg_encoding =
      Data_encoding.conv
        bindings
        (fun l => List.fold_left (fun m (k,v) => add k v m) empty l)
        Data_encoding.(list (tup2 encoding arg_encoding))
  };

  let fold_read f buf off len init =
    let last = off + len * size in
    if last > MBytes.length buf then
      invalid_arg "Hash.read_set: invalid size.";
    let rec loop acc off =
      if off >= last then
        acc
      else
        let hash = read buf off in
        loop (f hash acc) (off + size)
    in
    loop init off

  let path_length = 1
  let to_path key = [to_hex key]
  let of_path path =
    let path = String.concat "" path in
    of_hex path
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn path

  let prefix_path p =
    let p = Hex_encode.hex_encode p in
    [ p ]

  module Table = struct
    include Hashtbl.Make(struct
      type nonrec t = t
      let hash = Hashtbl.hash
      let equal = equal
    end)
  end

end

let () =
  Base58.check_encoded_prefix Block_hash.b58check_encoding "B" 51 ;
  Base58.check_encoded_prefix Operation_hash.b58check_encoding "o" 51 ;
  Base58.check_encoded_prefix Operation_list_hash.b58check_encoding "Lo" 52 ;
  Base58.check_encoded_prefix Operation_list_list_hash.b58check_encoding "LLo" 53 ;
  Base58.check_encoded_prefix Protocol_hash.b58check_encoding "P" 51 ;
  Base58.check_encoded_prefix Net_id.b58check_encoding "Net" 15
