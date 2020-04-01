module WagonType = struct

    type 't x2

    type bln
    type i8
    type i16 = i8 x2
    type i32 = i16 x2
    type i64 = i32 x2
    type f32
    type f64

    type 't pdt = string * string * int * int

    type 't v128 = 't * int
    type 't v256 = 't * int

    type ('hd, 'tl) structure = 'hd * 'tl

    type 't wtype = 't * string

    let bln     : bln pdt = ("bool",    "ib",   8,   8)
    let i8      : i8 pdt  = ("int8_t",  "i8",   8,   8)
    let i16     : i16 pdt = ("int16_t", "i16",  16,  16)
    let i32     : i32 pdt = ("int32_t", "i32",  32,  32)
    let i64     : i64 pdt = ("int64_t", "i64",  64,  64)
    let f32     : f32 pdt = ("float"  , "f32",  32,  32)
    let f64     : f64 pdt = ("double" , "d64",  64,  64)

    let pdt     : 'a pdt -> 'a pdt wtype =
    fun (fullname, abbrname, siz, align) -> ((fullname, abbrname, siz, align), fullname)

    let v128    : 'a pdt -> 'a pdt v128 =
    fun (fullname, abbrname, siz, align) ->
        let elem_num = 128 / siz in
        ((fullname, abbrname, siz, align), elem_num)

    let v256    : 'a pdt -> 'a pdt v128 =
    fun (fullname, abbrname, siz, align) ->
        let elem_num = 256 / siz in
        ((fullname, abbrname, siz, align), elem_num)

    let vtype : 'a -> 'a wtype =
    fun ((fullname, abbrname, siz, align), elem_num) -> 
        ((fullname, abbrname, siz, align), elem_num), 
        Printf.sprintf "__m%d%s" (siz * elem_num) (String.sub abbrname 0 1)
end