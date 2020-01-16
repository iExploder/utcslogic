module WagonType : WagonTypeBasic  = struct
  type 'a x2

  type i8
  type i16 = i8  x2
  type i32 = i16 x2
  type i64 = i32 x2
  type 'i ityp
  type f32
  type f64
  type 'f ftyp

  type 'a vs64  = f32 x2 (* Dummy *)
  type 'a vi128 = i64 x2
  type 'a vs128 = 'a vs64 x2
  type 'a vd128 = f64 x2
  type 'a vi256 = 'a vi128 x2
  type 'a vs256 = 'a vs128 x2
  type 'a vd256 = 'a vd128 x2

  type 'a vec

  type 'a ptr

  type structure
  type 'a arr

  (* abbr, fullname, size(bits),  alignment requirement(bits) *)
  type 'a wtype = string * string * int * int 

  (* Architecture Word Size, 64 bits for x64 arch *)
  let arch_wordsize = fun () -> 64 

  (* Types, <stdint.h> needed *)
  let i8_t          : (i8  ityp) wtype = ("c",    "int8_t",   8,    8)
  let i16_t         : (i16 ityp) wtype = ("w",    "int16_t",  16,   16)  
  let i32_t         : (i32 ityp) wtype = ("i",    "int32_t",  32,   32)
  let i64_t         : (i64 ityp) wtype = ("l",    "int64_t",  64,   64)
  let f32_t         : (f32 ftyp) wtype = ("s",    "float",    32,   32)
  let f64_t         : (f64 ftyp) wtype = ("d",    "double",   64,   64)

  let halfi         : ('a x2 ityp) wtype -> ('a ityp wtype) = 
    fun (abbr, full, siz, align) ->
      match abbr with
      | "w" (* Int16 *) -> i8_t
      | "i" (* Int32 *) -> i16_t
      | "l" (* Int64 *) -> i32_t
      | _ -> failwith "Unexpected arg"
  
  let doublei       : ('a ityp) wtype -> ('b ityp) wtype =
    fun (abbr, full, siz, align) ->
      match abbr with
      | "c" (* Int8  *) -> i16_t
      | "w" (* Int16 *) -> i32_t
      | "i" (* Int32 *) -> i64_t
      | _ -> failwith "Unexpected arg"

  let vs64_t        : (f32 ftyp) wtype -> (f32 vs64)  vec wtype = 
    fun (abbr, full, siz, align) -> ("VHs", "_m64",    64,   64)
  let vi128_t       : ('a  ityp) wtype -> ('a  vi128) vec wtype = 
    fun (abbr, full, siz, align) -> ("VS" ^ abbr, "_m128i",  128,  128)
  let vs128_t       : (f32 ftyp) wtype -> (f32 vs128) vec wtype = 
    fun (abbr, full, siz, align) -> ("VSs", "_m128",   128,  128)
  let vd128_t       : (f64 ftyp) wtype -> (f64 vd128) vec wtype = 
    fun (abbr, full, siz, align) -> ("VSd", "_m128d",  128,  128)
  let vi256_t       : ('a  ityp) wtype -> ('a  vi256) vec wtype = 
    fun (abbr, full, siz, align) -> ("VD" ^ abbr, "_m256i",  256,  256)
  let vs256_t       : (f32 ftyp) wtype -> (f32 vs256) vec wtype = 
    fun (abbr, full, siz, align) -> ("VDs", "_m256s",  256,  256)
  let vd256_t       : (f64 ftyp) wtype -> (f64 vd256) vec wtype = 
    fun (abbr, full, siz, align) -> ("VDd", "_m256d",  256,  256)
  
  let halfv         : ('a vec) wtype -> ('b vec) wtype =
    fun (abbr, full, siz, align) ->
    match abbr with
    | "VDd" (* VD256 *) -> vd128_t f64_t
    | "VDs" (* VS256 *) -> vs128_t f32_t
    | "VDl" (* VI256L*) -> vi128_t i64_t
    | "VDi" (* VI256I*) -> vi128_t i32_t 
    | "VDw" (* VI256W*) -> vi128_t i16_t
    | "VDc" (* VI256C*) -> vi128_t i8_t
    | _ -> failwith "Unexpected arg"
  let doublev       : ('a vec) wtype -> ('b vec) wtype =
    fun (abbr, full, siz, align) ->
    match abbr with
    | "VSd" (* VD128 *) -> vd256_t f64_t
    | "VSs" (* VS128 *) -> vs256_t f32_t
    | "VSl" (* VI128L*) -> vi256_t i64_t
    | "VSi" (* VI128I*) -> vi256_t i32_t 
    | "VSw" (* VI128W*) -> vi256_t i16_t
    | "VSc" (* VI128C*) -> vi256_t i8_t
    | _ -> failwith "Unexpected arg"
  let scalarv       : ('a vec) wtype -> ('b) wtype =
    fun (abbr, full, siz, align) ->
    let scalar_abbr = String.get abbr 2 in
    match scalar_abbr with
    | 'c' -> i8_t
    | 'w' -> i16_t
    | 'i' -> i32_t
    | 'l' -> i64_t
    | 's' -> f32_t
    | 'd' -> f64_t
    | _ -> failwith "Unexpected arg"

  let ptr_type      : 'a wtype -> ('a ptr) wtype = 
    fun (ta, tf, siz, align) -> 
      (Printf.sprintf "P_%s_" ta, 
      tf ^ "*", 
      arch_wordsize (), 
      arch_wordsize ())
  let array_type    : 'a wtype -> int -> ('a arr) wtype =
    fun (ta, tf, siz, align) n -> 
      (Printf.sprintf "A_%s%d_" ta n, 
      Printf.sprintf "%s[%d]" tf n, 
      siz * n, 
      siz)

  let structure     : ('a wtype) list -> structure wtype =
    fun typ_list ->
      let rec struct_name : ('a wtype) list -> string = fun typ_l ->
        match typ_l with
        | [] -> ""
        | (ta, tf, siz, align) :: tl -> ta ^ (struct_name tl)
      in
      let rec struct_size : ('a wtype) list -> int = fun typ_l ->
        match typ_l with
        | [] -> 0
        | (ta, tf, siz, align) :: tl -> siz + (struct_size tl)
      in
      let abbr = Printf.sprintf "S_%s_" @@ struct_name typ_list in
      let fullname = Printf.sprintf "struct %s" abbr in
      let siz = struct_size typ_list in
        (abbr, fullname, siz, arch_wordsize ())
  let typename      : 'a wtype -> string =
    fun (abbr, fullname, siz, align) -> fullname
  let size          : 'a wtype -> int =
    fun (abbr, fullname, siz, align) -> siz
  let special_align : 'a wtype -> bool =
    fun (abbr, fullname, siz, align) -> align > (arch_wordsize ())
end
(** 

*)