module type WagonPDT = sig
    type t
    val typename : string
    val abbr : string
end
module WagonInt32 : WagonPDT = struct
    type t
    let typename = "int32_t"
    let abbr = "dw"
end
module WagonInt64 : WagonPDT = struct
    type t
    let typename = "int64_t"
    let abbr = "qw"
end
module WagonFloat32 : WagonPDT = struct
    type t
    let typename = "float"
    let abbr = "sf"
end
module WagonFloat64 : WagonPDT = struct
    type t
    let typename = "double"
    let abbr = "df"
    
end