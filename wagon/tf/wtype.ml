module type WagonBasicType = sig
  (* For data type with multiplied relation *)
  type 'a x2
  
  (* Integer Scalar *)
  (* Signed Integers *)
  type bln_ (* Boolean *)
  type i8_
  type i16_ = i8_  x2
  type i32_ = i16_ x2
  type i64_ = i32_ x2

  type 'a ityp
  (* Floating-point decimal Scalar *)
  (* Notice: 
     According to IEEE 754, floats in single and double
     precision have different data layouts, so no x2 is
     defined here. *)
  type f32_
  type 'f ftyp
  type f64_
  type 'd dtyp

  (* Vector Types *)
  type 't v64_
  type 't v128_ = 't v64_ x2
  type 't v256_ = 't v128_ x2
  type 't v512_ = 't v256_ x2

  type 'v vec

  (* Array Type *)
  type 'ifdv arr = 'ifdv * int

  (* Structure *)
  type 'hl wstr = 'hl HList.hlist
  type 'a wtype

  val halfi : 'a x2 ityp -> 'a ityp
  val halff : f64_ dtyp -> f32_ ftyp
  val halfv : 'a x2 vec -> 'a vec

  val doublei : 'a ityp -> 'a x2 ityp
  val doublef : f32_ ftyp -> f64_ dtyp
  val doublev : 'a vec -> 'a x2 vec

  val i8_t  : i8_  ityp wtype
  val i16_t : i16_ ityp wtype
  val i32_t : i32_ ityp wtype
  val i64_t : i64_ ityp wtype

  val f32_t : f32_ ftyp wtype
  val f64_t : f64_ dtyp wtype

  val arr   : 'a wtype -> int -> 'a arr wtype

end

module WagonTypeName = sig
  (* For data type with multiplied relation *)
  type 'a x2
  
  (* Integer Scalar *)
  (* Signed Integers *)
  type bln_ (* Boolean *)
  type i8_
  type i16_ = i8_ x2
  type i32_ = i16_ x2
  type i64_ = i32_ x2
  (* Unsigned Integers *)
  type u8_
  type u16_ = u8_ x2
  type u32_ = u16_ x2
  type u64_ = u32_ x2

  type 'a ityp
  (* Floating-point decimal Scalar *)
  (* Notice: 
     According to IEEE 754, floats in single and double
     precision have different data layouts, so no x2 is
     defined here. *)
  type f32_
  type 'f ftyp
  type f64_
  type 'd dtyp

  (* Vector Types *)
  type 't v64_
  type 't v128_ = 't v64_ x2
  type 't v256_ = 't v128_ x2
  type 't v512_ = 't v256_ x2

  type 'v vec

  type 'a wtype = string

  val halfi : 'a x2 ityp -> 'a ityp
  val halff : f64_ dtyp -> f32_ ftyp
  val halfv : 'a x2 vec -> 'a vec

  val doublei : 'a ityp -> 'a x2 ityp
  val doublef : f32_ ftyp -> f64_ dtyp
  val doublev : 'a vec -> 'a x2 vec
end

let rec iter_all : 'a hlist -> string =
fun x ->
match x with
| HNil -> ""
| HCons((_,s), b) -> s ^ (iter_all b)
