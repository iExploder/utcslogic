module WagonBasic = struct
    open WagonDT
    (* Wagon Function Type *)
    type wfxtype = wtype list * wtype
    type wexpr = wtype * string
    type wstmt = string

    (* Invisible Internal Functions for Users *)
    let litmagic    : ('a -> string) -> wtype -> 'a -> wexpr =
    fun serializer typ lit -> (typ, serializer lit)
    let string_of_array : ('a -> string) -> string -> 'a array -> string =
    fun serializer delimiter arr ->
        let fold_func s i = Printf.sprintf "%s%s%s" s delimiter @@ serializer i in
        let untrimmed_str = Array.fold_left fold_func "" arr in
        String.sub untrimmed_str (String.length delimiter) 
            ((String.length untrimmed_str) - (String.length delimiter))

    let string_binop : string -> string -> string -> string =
    fun op lhs rhs -> Printf.sprintf "(%s %s %s)" lhs op rhs

    let _binop        : (wtype -> wtype -> wtype) -> string -> wexpr -> wexpr -> wexpr =
    fun typf op_str (t1, s1) (t2, s2) -> (typf t1 t2, string_binop op_str s1 s2)
    let _typf_iii     : wtype -> wtype -> wtype =
    fun wt1 wt2 ->
    match (wt1, wt2) with
    | (PDT(Int(l1)), PDT(Int(l2))) -> if l1 = l2 then wt1 
                                        else failwith "unexpeccted bitlengths of integer operands"
    | _ -> failwith "unexpected operand type(s)"
    let _typf_sss     : wtype -> wtype -> wtype =
    fun wt1 wt2 ->
    match (wt1, wt2) with
    | (PDT(FSingle), PDT(FSingle)) -> wt1
    | _ -> failwith "unexpected operand type(s)"
    let _typf_ddd     : wtype -> wtype -> wtype =
    fun wt1 wt2 ->
    match (wt1, wt2) with
    | (PDT(FDouble), PDT(FDouble)) -> wt1
    | _ -> failwith "unexpected operand type(s)"


    let intlit      : wtype -> int -> wexpr =
    fun typ lit ->
        match typ with
        | PDT(Int(i)) -> litmagic string_of_int typ lit
        | _ -> failwith "unexpected data type"

    let floatlit    : wtype -> float -> wexpr =
    fun typ lit ->
        match typ with 
        | PDT(t) -> let ftyp =
                        match t with
                        | FSingle -> FSingle
                        | FDouble -> FDouble
                        | _ -> failwith "unexpected data type"
                    in litmagic string_of_float (PDT(ftyp)) lit
        | _ -> failwith "unexpected data type"

    let vilit       : wtype -> int array -> wexpr =
    fun typ arr ->
        let verify_len i a = 
            match i with
            | Vec128i(Int(bitlen)) -> if bitlen * (Array.length a) = 128 then true else false
            | Vec256i(Int(bitlen)) -> if bitlen * (Array.length a) = 256 then true else false
            | Vec512i(Int(bitlen)) -> if bitlen * (Array.length a) = 512 then true else false
            | _ -> failwith "unexpected SIMD vector type"
        in
        if verify_len typ arr then 
            (typ, Printf.sprintf "{%s}" @@ string_of_array string_of_int ", " arr)
        else failwith "unexpected literal array length"

    let vslit       : wtype -> float array -> wexpr =
    fun typ arr ->
        let verify_len i a = 
            match i with
            | Vec128s(FSingle) -> if 32 * (Array.length a) = 128 then true else false
            | Vec256s(FSingle) -> if 32 * (Array.length a) = 256 then true else false
            | Vec512s(FSingle) -> if 32 * (Array.length a) = 512 then true else false
            | _ -> failwith "unexpected SIMD vector type"
        in
        if verify_len typ arr then 
            (typ, Printf.sprintf "{%s}" @@ string_of_array (Printf.sprintf "%ff") ", " arr)
        else failwith "unexpected literal array length"

    let vdlit       : wtype -> float array -> wexpr =
    fun typ arr ->
        let verify_len i a = 
            match i with
            | Vec128d(FDouble) -> if 64 * (Array.length a) = 128 then true else false
            | Vec256d(FDouble) -> if 64 * (Array.length a) = 256 then true else false
            | Vec512d(FDouble) -> if 64 * (Array.length a) = 512 then true else false
            | _ -> failwith "unexpected SIMD vector type"
        in
        if verify_len typ arr then 
            (typ, Printf.sprintf "{%s}" @@ string_of_array (Printf.sprintf "%f") ", " arr)
        else failwith "unexpected literal array length"
    
    let addi        : wexpr -> wexpr -> wexpr = _binop _typf_iii "+"




end