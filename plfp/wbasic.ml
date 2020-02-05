module WagonBasic = struct
    open WagonDT
    (* Wagon Function Type *)
    type wfxtype = wtype list * wtype
    type wexpr = wtype * string
    type wstmt = string
    type 'a wlvalue = 'a

    (* Invisible Internal Functions for Users *)
    let _litmagic    : ('a -> string) -> wtype -> 'a -> wexpr =
    fun serializer typ lit -> (typ, serializer lit)
    let _string_of_array : ('a -> string) -> string -> 'a array -> string =
    fun serializer delimiter arr ->
        let fold_func s i = Printf.sprintf "%s%s%s" s delimiter @@ serializer i in
        let untrimmed_str = Array.fold_left fold_func "" arr in
        String.sub untrimmed_str (String.length delimiter) 
            ((String.length untrimmed_str) - (String.length delimiter))

    let _string_binop : string -> string -> string -> string =
    fun op lhs rhs -> Printf.sprintf "(%s %s %s)" lhs op rhs

    let _binop        : (wtype -> wtype -> wtype) -> string -> wexpr -> wexpr -> wexpr =
    fun typf op_str (t1, s1) (t2, s2) -> (typf t1 t2, _string_binop op_str s1 s2)
    let _typf_iii     : wtype -> wtype -> wtype =
    fun wt1 wt2 ->
    match (wt1, wt2) with
    | (PDT(Int(l1)), PDT(Int(l2))) -> 
        if l1 = l2 then wt1 
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

    let _namecounter  : int ref = ref (-1)
    let _getname      : unit -> string = 
    fun () -> 
        let count = 
            _namecounter := (!_namecounter) + 1; 
            (!_namecounter) 
        in
        Printf.sprintf "wobj_%08x" count

    let _declare_list : string list ref = ref []
    let _declare_append : string -> unit =
    fun str ->
        let newlist = (!_declare_list) @ [str] in
        _declare_list := newlist

    let boollit     : bool -> wexpr =
    fun b -> 
        let bstr = if b then "true" else "false" in 
        (PDT(Bool), bstr)

    let intlit      : wtype -> int -> wexpr =
    fun typ lit ->
        match typ with
        | PDT(Int(i)) -> _litmagic string_of_int typ lit
        | _ -> failwith "unexpected data type"

    let floatlit    : wtype -> float -> wexpr =
    fun typ lit ->
        match typ with 
        | PDT(t) -> let ftyp =
                        match t with
                        | FSingle -> FSingle
                        | FDouble -> FDouble
                        | _ -> failwith "unexpected data type"
                    in _litmagic string_of_float (PDT(ftyp)) lit
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
            (typ, Printf.sprintf "{%s}" @@ _string_of_array string_of_int ", " arr)
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
            (typ, Printf.sprintf "{%s}" @@ _string_of_array (Printf.sprintf "%ff") ", " arr)
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
            (typ, Printf.sprintf "{%s}" @@ _string_of_array (Printf.sprintf "%f") ", " arr)
        else failwith "unexpected literal array length"
    
    let addi        : wexpr -> wexpr -> wexpr = _binop _typf_iii "+"
    let subi        : wexpr -> wexpr -> wexpr = _binop _typf_iii "-"
    let muli        : wexpr -> wexpr -> wexpr = _binop _typf_iii "*"
    let divi        : wexpr -> wexpr -> wexpr = _binop _typf_iii "/"
    let adds        : wexpr -> wexpr -> wexpr = _binop _typf_sss "+"
    let subs        : wexpr -> wexpr -> wexpr = _binop _typf_sss "-"
    let muls        : wexpr -> wexpr -> wexpr = _binop _typf_sss "*"
    let divs        : wexpr -> wexpr -> wexpr = _binop _typf_sss "/"
    let addd        : wexpr -> wexpr -> wexpr = _binop _typf_ddd "+"
    let subd        : wexpr -> wexpr -> wexpr = _binop _typf_ddd "-"
    let muld        : wexpr -> wexpr -> wexpr = _binop _typf_ddd "*"
    let divd        : wexpr -> wexpr -> wexpr = _binop _typf_ddd "/"

    let declare     : wtype -> wexpr wlvalue =
    fun typ ->
        let nam = _getname () in
        let decl = typename_decl typ nam in
        _declare_append decl;
        (typ, nam)
    
    let assign      : wexpr wlvalue -> wexpr -> wstmt =
    fun (ltyp, lexp) (rtyp, rexp) ->
        Printf.sprintf "%s=%s;" lexp rexp
    
    let struct_elem : wexpr wlvalue -> int -> wexpr wlvalue =
    fun (styp, sexp) n ->
        match styp with
        | Struct(sl) -> 
            let typ = List.nth sl n in (typ, Printf.sprintf "%s.v%d" sexp n)
        | _ -> failwith "unexpected data type"
    
    let array_elem : wexpr wlvalue -> wexpr -> wexpr wlvalue =
    fun (styp, sexp) (ntyp, nexp) ->
        match (styp, ntyp) with
        | (Array(aet, n), PDT(Int(il))) -> (aet, Printf.sprintf "%s[%s]" sexp nexp)
        | _ -> failwith "unexpected data type"

    let deref     : 'a wlvalue -> 'a = fun a -> a


end