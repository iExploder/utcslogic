(* GADT-style implementation of Wagon Types *)
module WagonDT = struct
    (* Wagon Primitive Data Types *)
    type wpdt = 
    | Bool
    | Int of int
    | FSingle
    | FDouble
    type wtype =
    | PDT of wpdt
    | Struct of wtype list
    | Array of wtype * int
    | Vec128i of wpdt (* Bool/ Int *)
    | Vec256i of wpdt (* Bool/ Int *)
    | Vec512i of wpdt (* Bool/ Int *)
    | Vec128s of wpdt (* FSingle *)
    | Vec256s of wpdt (* FSingle *)
    | Vec512s of wpdt (* FSingle *)
    | Vec128d of wpdt (* FDouble *)
    | Vec256d of wpdt (* FDouble *)
    | Vec512d of wpdt (* FDouble *)
    | Pointer of wtype (* Pointer of any type *)

    let wpdt_bitlen : wpdt -> int =
    fun dt ->
        match dt with
        | Bool      -> 8
        | Int(l)    -> l
        | FSingle   -> 32
        | FDouble   -> 64

    let struct_elem : wtype -> int -> wtype =
    fun str n ->
        match str with
        | Struct(elem_list) -> List.nth elem_list n
        | _ -> failwith "unexpected data type"

    let _struct_decl : string list ref = ref []
    let _struct_decl_append : string -> unit =
    fun decl ->
        if List.exists (fun s -> s = decl) (!_struct_decl) then ()
        else _struct_decl := (!_struct_decl) @ [decl]
    let get_struct_decl : unit -> string =
    fun () -> List.fold_left (^) "" (!_struct_decl)
    
    let pdt_name : wpdt -> string =
    fun pdt -> 
        match pdt with
        | Bool -> "bool"
        | Int(i) -> Printf.sprintf "int%d_t" i
        | FSingle -> "float"
        | FDouble -> "double"

    let struct_name : wtype -> string =
    fun wt ->
        match wt with
        | Struct(s) -> Printf.sprintf "ws_%08x" (Hashtbl.hash wt)
        | _ -> failwith "unexpected data type"
    let rec typename : wtype -> string =
    fun wt ->
        match wt with
        | PDT(pdt) -> pdt_name pdt
        | Struct(wtl) -> struct_name wt
        | Array(it,n) -> Printf.sprintf "%s[%d]" (typename it) n
        | Vec128i(it) -> "__m128i"
        | Vec256i(it) -> "__m256i"
        | Vec512i(it) -> "__m512i"
        | Vec128s(it) -> "__m128"
        | Vec256s(it) -> "__m256"
        | Vec512s(it) -> "__m512"
        | Vec128d(it) -> "__m128d"
        | Vec256d(it) -> "__m256d"
        | Vec512d(it) -> "__m512d"
        | Pointer(it) -> Printf.sprintf "%s*" (typename it)
    let typename_decl : wtype -> string -> string =
    fun wt nam ->
        match wt with
        | PDT(pdt)    -> Printf.sprintf "%s %s;" (pdt_name pdt) nam
        | Struct(wtl) -> Printf.sprintf "struct %s %s;" (struct_name wt) nam
        | Array(it,n) -> Printf.sprintf "%s %s[%d];" (typename it) nam n
        | Vec128i(it) -> Printf.sprintf "%s %s;" "__m128i" nam
        | Vec256i(it) -> Printf.sprintf "%s %s;" "__m256i" nam
        | Vec512i(it) -> Printf.sprintf "%s %s;" "__m512i" nam
        | Vec128s(it) -> Printf.sprintf "%s %s;" "__m128" nam
        | Vec256s(it) -> Printf.sprintf "%s %s;" "__m256" nam
        | Vec512s(it) -> Printf.sprintf "%s %s;" "__m512" nam
        | Vec128d(it) -> Printf.sprintf "%s %s;" "__m128d" nam
        | Vec256d(it) -> Printf.sprintf "%s %s;" "__m256d" nam
        | Vec512d(it) -> Printf.sprintf "%s %s;" "__m512d" nam
        | Pointer(it) -> Printf.sprintf "%s* %s;" (typename it) nam

    let struct_decl : wtype -> string =
    fun pdt ->
        let rec gen_struct_list ?(id=0) l =
            match l with 
            | [] -> ""
            | hd :: tl -> (Printf.sprintf "\t%s v%d;\n" (typename hd) id) ^ (gen_struct_list tl ~id:(id+1) )
        in
        match pdt with
        | Struct(s) -> Printf.sprintf "struct %s{\n%s};\n" (struct_name pdt) (gen_struct_list s)
        | _ -> failwith "unexpected data type"

    let i8_t    : wtype = PDT(Int(8))
    let i16_t   : wtype = PDT(Int(16))
    let i32_t   : wtype = PDT(Int(32))
    let i64_t   : wtype = PDT(Int(64))

    let f32_t   : wtype = PDT(FSingle)
    let f64_t   : wtype = PDT(FDouble)

    let v128    : wtype -> wtype = 
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
    let v256    : wtype -> wtype = 
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
    let v512    : wtype -> wtype = 
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
    
    let arr     : wtype -> int -> wtype =
    fun elem n -> Array(elem, n)
    let structure : wtype list -> wtype =
    fun elem_list -> 
        let str = Struct(elem_list) in
        _struct_decl_append (struct_decl str);
        str

end