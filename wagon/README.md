# Wagon: Accelerated DSL for OCaml

**Wagon** is designed as an embedded DSL of OCaml for generating high-performance vectorized code with preserving type safety by taking advantage of strong OCaml type system.

# Introduction

## Problem: Lack of Scalar Type Preservation

* No preservation of bit-length of scalars in **Integer Vectors** 
  provided in Intel SIMD Intrinsics

**Example**: 256Bit Intel AVX2 Integer Vector
```c
typedef union  __declspec(intrin_type) __declspec(align(32)) __m256i {
    __int8              m256i_i8[32];
    __int16             m256i_i16[16];
    __int32             m256i_i32[8];
    __int64             m256i_i64[4];
    unsigned __int8     m256i_u8[32];
    unsigned __int16    m256i_u16[16];
    unsigned __int32    m256i_u32[8];
    unsigned __int64    m256i_u64[4];
} __m256i;
```

* NO Type checking of contained scalars
  
* Hard to debug

* Actual mistakes are hard to find

## Problem: Mistaken memory alignment in use of Intel SIMD Intrinsics

* Memory alignment requirements of Intel SIMD Instructions
  
**Example**: Memory space
```c
Aligned to (64bit * n)
v
[float64][float64][float64][float64] [float64][float64][float64][float64] ...
v        v(vb1)                               v(vb2)
[float64][float64][float64][float64] [float64][float64][float64][float64] ...
^(va1)                               ^(va2)
```

In case of 256-bit packed addition of double-precision floating-point decimals(ADDPD),
  + Passing `(va1,va2)` as arguments **WORKS**.

  + Passing `(vb1,vb2)` as arguments would result in **SIGSEGV**(Segmentation fault)
  
# This Research

* Tagless-final style OCaml module
* Generates C functions with **properly type checked** SIMD Intrinsics 
  and necessary stubs 
* Avoid mistaken aligned memory layout
  + Integrated memory management code in generated C code

# Roadmap
* **Wagon** Type System(with type inferences) (NOW)

    + Primitive types (Int8 ~ Int64, Float32, Float64, Bool) (OK)
    + Vector types(VI128, VI256, VS128, VS256, VD128, VD256) (OK)
    + Type Inference **(Now)**

* Tagless-final style C type system (IN PROGRESS)
