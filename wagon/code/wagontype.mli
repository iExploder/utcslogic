module type WagonTypeBasic = sig
    type 'a x2
    type i8
    type i16
    type i32
    type i64
    type 'i ityp
    type f32
    type f64
    type 'f ftyp
    type 'a vs64
    type 'a vi128
    type 'a vs128
    type 'a vd128
    type 'a vi256
    type 'a vs256
    type 'a vd256
    type 'a ptr
    type structure
    type 'a vec
    type 'a arr
    type 'a wtype
    val arch_wordsize : unit -> int
    val i8_t : i8 ityp wtype
    val i16_t : i16 ityp wtype
    val i32_t : i32 ityp wtype
    val i64_t : i64 ityp wtype
    val f32_t : f32 ftyp wtype
    val f64_t : f64 ftyp wtype
    val vs64_t  : f32 ftyp wtype -> f32 vs64 vec wtype
    val vi128_t : 'a ityp wtype -> 'a vi128 vec wtype
    val vs128_t : f32 ftyp wtype -> f32 vs128 vec wtype
    val vd128_t : f64 ftyp wtype -> f64 vd128 vec wtype
    val vi256_t : 'a ityp wtype -> 'a vi256 vec wtype
    val vs256_t : f32 ftyp wtype -> f32 vs256 vec wtype
    val vd256_t : f64 ftyp wtype -> f64 vd256 vec wtype

    val halfi : 'a x2 ityp wtype -> 'a ityp wtype
    val doublei : 'a ityp wtype -> 'b ityp wtype
    val halfv : 'a vec wtype -> 'b vec wtype
    val doublev : 'a vec wtype -> 'b vec wtype
    val scalarv : 'a vec wtype -> 'b wtype

    val ptr_type : 'a wtype -> 'a ptr wtype
    val array_type : 'a wtype -> int -> 'a arr wtype
    val structure : 'a wtype list -> structure wtype
    val typename : 'a wtype -> string
    val size : 'a wtype -> int
    val special_align : 'a wtype -> bool
  end
  