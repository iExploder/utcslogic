module type WagonGeneralP =
  sig
    type 't wexpr
    type ('a, 'b) wfus = (string -> 'a) -> 'b
    type ('at, 'wfa, 'wfb) wtype = {
      name : string;
      init : ('wfa, 'wfb) wfus;
    }

    type wstmt

    (* L-value in C *)
    type 't lval

    type 't decl  = {v : 't lval;  decl : wstmt}

    type 't wfunc = {f : 't wexpr; decl : wstmt}

    val cons_finit_in :  ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus
    val cons_wtype :
      ('at0, 'wfb, 'wfc) wtype ->
      ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype
    type wbln
    type wi8
    type wi16
    type wi32
    type wi64
    type wf32
    type wf64
    type 't warray
    type 't wstruct
    val wbln : (wbln, 'a, bool  -> 'a) wtype
    val wi8  : (wi8,  'a, int   -> 'a) wtype
    val wi16 : (wi16, 'a, int   -> 'a) wtype
    val wi32 : (wi32, 'a, int   -> 'a) wtype
    val wi64 : (wi64, 'a, int   -> 'a) wtype
    val wf32 : (wf32, 'a, float -> 'a) wtype
    val wf64 : (wf64, 'a, float -> 'a) wtype
    val init : ('a, 'a wexpr, 'b) wtype -> 'b
    val wstruct : ('th * 'tl, 'a, 'b) wtype -> (('th * 'tl) wstruct, 'a, 'b) wtype
    val show : 'a wexpr -> string

    (* Remove lval tag *)
    val delval : 't lval -> 't

    val decl : ('t, _, _) wtype -> 't wexpr -> 't wexpr decl

    val make_func : ('t0, _, _) wtype -> ('t1, _, _) wtype -> ('t0 wexpr -> 't1 wexpr) -> ('t0 -> 't1) wfunc
    val apply : ('t0 -> 't1) wfunc -> 't0 wexpr -> 't1 wexpr

    val addf : wf32 wexpr -> wf32 wexpr -> wf32 wexpr

    val assign : 'a wexpr lval -> 'a wexpr -> wstmt
  end