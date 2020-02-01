(* GADT-style implementation of Wagon Types *)
module WagonDT = struct
    (* Wagon Primitive Data Types *)
    type wpdt = 
    | Bool
    | Int of int
    | FSingle
    | FDouble
    type wdata =
    | PDT of wpdt
    | Struct of wdata list
    | Array of wdata * int
    | Vec128i of wpdt (* Bool/ Int *)
    | Vec256i of wpdt (* Bool/ Int *)
    | Vec512i of wpdt (* Bool/ Int *)
    | Vec128s of wpdt (* FSingle *)
    | Vec256s of wpdt (* FSingle *)
    | Vec512s of wpdt (* FSingle *)
    | Vec128d of wpdt (* FDouble *)
    | Vec256d of wpdt (* FDouble *)
    | Vec512d of wpdt (* FDouble *)
    type wfunc = wdata list * wdata

    let i8_t    : wdata = PDT(Int(8))
    let i16_t   : wdata = PDT(Int(16))
    let i32_t   : wdata = PDT(Int(32))
    let i64_t   : wdata = PDT(Int(64))

    let f32_t   : wdata = PDT(FSingle)
    let f64_t   : wdata = PDT(FDouble)

    let v128    : wdata -> wdata = 
        fun scalar ->
        match scalar with
        | PDT(dt) -> 
            begin
                match dt with
                | Bool      -> Vec128i(Bool)
                | Int(n)    -> Vec128i(Int(n))
                | FSingle   -> Vec128s(FSingle)
                | FDouble   -> Vec128d(FDouble)
            end
        | _ -> failwith "unexpected scalar type"
    let v256    : wdata -> wdata = 
        fun scalar ->
        match scalar with
        | PDT(dt) -> 
            begin
                match dt with
                | Bool      -> Vec256i(Bool)
                | Int(n)    -> Vec256i(Int(n))
                | FSingle   -> Vec256s(FSingle)
                | FDouble   -> Vec256d(FDouble)
            end
        | _ -> failwith "unexpected scalar type"
    let v512    : wdata -> wdata = 
        fun scalar ->
        match scalar with
        | PDT(dt) -> 
            begin
                match dt with
                | Bool      -> Vec512i(Bool)
                | Int(n)    -> Vec512i(Int(n))
                | FSingle   -> Vec512s(FSingle)
                | FDouble   -> Vec512d(FDouble)
            end
        | _ -> failwith "unexpected scalar type"
    
    let arr     : wdata -> int -> wdata =
        fun elem n -> Array(elem, n)
    let structure : wdata list -> wdata =
        fun elem_list -> Struct(elem_list)
    let struct_elem : wdata -> int -> wdata =
        fun str n ->
        match str with
        | Struct(elem_list) -> List.nth elem_list n
        | _ -> failwith "unexpected data type"


end