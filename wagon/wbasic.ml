module WagonBasic = struct
    open WagonType
    type 'a expr = 'a * string
    type 'a stmt = 'a * unit * string
    
end