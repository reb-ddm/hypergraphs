module CI = Cstubs_internals

external _1_lace_start : Unsigned.uint -> Unsigned.size_t -> unit
  = "_1_lace_start" 

external _2_sylvan_set_limits : Unsigned.size_t -> int -> int -> unit
  = "_2_sylvan_set_limits" 

external _3_sylvan_init_package : unit -> unit = "_3_sylvan_init_package" 

external _4_sylvan_quit : unit -> unit = "_4_sylvan_quit" 

external _5_sylvan_init_ldd : unit -> unit = "_5_sylvan_init_ldd" 

external _6_lddmc_makenode
  : Unsigned.uint32 -> Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64
  = "_6_lddmc_makenode" 

external _7_ocaml_lddmc_union_f
  : Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64
  = "_7_ocaml_lddmc_union_f" 

external _8_ocaml_lddmc_intersect_f
  : Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64
  = "_8_ocaml_lddmc_intersect_f" 

external _9_ocaml_mtbdd_support_f : Unsigned.uint64 -> Unsigned.uint64
  = "_9_ocaml_mtbdd_support_f" 

external _10_ocaml_lddmc_project_f
  : Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64
  = "_10_ocaml_lddmc_project_f" 

external _11_lddmc_getvalue : Unsigned.uint64 -> Unsigned.uint32
  = "_11_lddmc_getvalue" 

external _12_lddmc_getdown : Unsigned.uint64 -> Unsigned.uint64
  = "_12_lddmc_getdown" 

external _13_lddmc_getright : Unsigned.uint64 -> Unsigned.uint64
  = "_13_lddmc_getright" 

external _14_lddmc_follow
  : Unsigned.uint64 -> Unsigned.uint32 -> Unsigned.uint64
  = "_14_lddmc_follow" 

external _15_lddmc_print : Unsigned.uint64 -> unit = "_15_lddmc_print" 

type 'a result = 'a
type 'a return = 'a
type 'a fn =
 | Returns  : 'a CI.typ   -> 'a return fn
 | Function : 'a CI.typ * 'b fn  -> ('a -> 'b) fn
let map_result f x = f x
let returning t = Returns t
let (@->) f p = Function (f, p)
let foreign : type a b. string -> (a -> b) fn -> (a -> b) =
  fun name t -> match t, name with
| Function (CI.Primitive CI.Uint64_t, Returns CI.Void), "lddmc_print" ->
  _15_lddmc_print
| Function
    (CI.Primitive CI.Uint64_t,
     Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Uint64_t))),
  "lddmc_follow" -> _14_lddmc_follow
| Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t)),
  "lddmc_getright" -> _13_lddmc_getright
| Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t)),
  "lddmc_getdown" -> _12_lddmc_getdown
| Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint32_t)),
  "lddmc_getvalue" -> _11_lddmc_getvalue
| Function
    (CI.Primitive CI.Uint64_t,
     Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t))),
  "ocaml_lddmc_project_f" -> _10_ocaml_lddmc_project_f
| Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t)),
  "ocaml_mtbdd_support_f" -> _9_ocaml_mtbdd_support_f
| Function
    (CI.Primitive CI.Uint64_t,
     Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t))),
  "ocaml_lddmc_intersect_f" -> _8_ocaml_lddmc_intersect_f
| Function
    (CI.Primitive CI.Uint64_t,
     Function (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t))),
  "ocaml_lddmc_union_f" -> _7_ocaml_lddmc_union_f
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.Primitive CI.Uint64_t, Returns (CI.Primitive CI.Uint64_t)))),
  "lddmc_makenode" -> _6_lddmc_makenode
| Function (CI.Void, Returns CI.Void), "sylvan_init_ldd" ->
  _5_sylvan_init_ldd
| Function (CI.Void, Returns CI.Void), "sylvan_quit" -> _4_sylvan_quit
| Function (CI.Void, Returns CI.Void), "sylvan_init_package" ->
  _3_sylvan_init_package
| Function
    (CI.Primitive CI.Size_t,
     Function
       (CI.Primitive CI.Int, Function (CI.Primitive CI.Int, Returns CI.Void))),
  "sylvan_set_limits" -> _2_sylvan_set_limits
| Function
    (CI.Primitive CI.Uint,
     Function (CI.Primitive CI.Size_t, Returns CI.Void)),
  "lace_start" -> _1_lace_start
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
