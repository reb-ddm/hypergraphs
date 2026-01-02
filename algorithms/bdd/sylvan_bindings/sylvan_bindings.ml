open Ctypes

type mdd = Unsigned.uint64
let mdd : mdd typ = uint64_t

let lddmc_false : mdd = Unsigned.UInt64.of_int 0
let lddmc_true : mdd = Unsigned.UInt64.of_int 1
let lddmc_equal = Unsigned.UInt64.equal
let branch_1 = Unsigned.UInt32.one
let branch_0 = Unsigned.UInt32.zero

(* The functor used by Cstubs to generate ML and C *)
module Bindings (F : Ctypes.FOREIGN) =
struct
open F
  let lace_start =
    foreign "lace_start" (uint @-> size_t @-> returning void)

  let sylvan_set_limits =
    foreign "sylvan_set_limits" (size_t @-> int @-> int @-> returning void)

  let sylvan_init_package =
    foreign "sylvan_init_package" (void @-> returning void)

  let sylvan_quit = foreign "sylvan_quit" (void @-> returning void)

  let sylvan_init_ldd = foreign "sylvan_init_ldd" (void @-> returning void)

  let lddmc_makenode =
    foreign "lddmc_makenode" (uint32_t @-> mdd @-> mdd @-> returning mdd)

  let lddmc_union = foreign "ocaml_lddmc_union_f" (mdd @-> mdd @-> returning mdd)

  let lddmc_intersect =
    foreign "ocaml_lddmc_intersect_f" (mdd @-> mdd @-> returning mdd)

  let mtbdd_support = foreign "ocaml_mtbdd_support_f" (mdd @-> returning mdd)

  let lddmc_project = foreign "ocaml_lddmc_project_f" (mdd @-> mdd @-> returning mdd)

  let lddmc_getvalue = foreign "lddmc_getvalue" (mdd @-> returning uint32_t)
  let lddmc_getdown  = foreign "lddmc_getdown"  (mdd @-> returning mdd)
  let lddmc_getright = foreign "lddmc_getright" (mdd @-> returning mdd)
  let lddmc_follow   = foreign "lddmc_follow"   (mdd @-> uint32_t @-> returning mdd)
  let lddmc_print    = foreign "lddmc_print"    (mdd @-> returning void)
end
