type ('hd, 'tl) hlist = 'hd * 'tl
type ('hd, 'tl) indexer = {
    hd : ('hd, 'tl) hlist -> 'hd ;
    tl : ('hd, 'tl) hlist -> 'tl ;
    next : ('hl, ('hr, 'tr) hlist) -> ('hr, 'tr) indexer
}
let indexer : ('hd, 'tl) hlist -> ('hd, 'tl) indexer =
fun (h, t) -> {hd = fun (h', t') -> h'; tl = fun (h', t') -> t'}