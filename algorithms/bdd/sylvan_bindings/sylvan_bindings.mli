type mdd = Unsigned.uint64
val mdd : mdd Ctypes_static.typ
val lddmc_false : mdd
val lddmc_true : mdd
val lddmc_equal : Unsigned.uint64 -> Unsigned.uint64 -> bool
val branch_1 : Unsigned.uint32
val branch_0 : Unsigned.uint32
module Bindings :
  functor (F : Ctypes.FOREIGN) ->
    sig
      val lace_start :
        (Unsigned.uint -> Unsigned.size_t -> unit F.return) F.result
      val sylvan_set_limits :
        (Unsigned.size_t -> int -> int -> unit F.return) F.result
      val sylvan_init_package : (unit -> unit F.return) F.result
      val sylvan_quit : (unit -> unit F.return) F.result
      val sylvan_init_ldd : (unit -> unit F.return) F.result
      val lddmc_makenode :
        (Unsigned.uint32 -> mdd -> mdd -> mdd F.return) F.result
      val lddmc_union : (mdd -> mdd -> mdd F.return) F.result
      val lddmc_intersect : (mdd -> mdd -> mdd F.return) F.result
      val mtbdd_support : (mdd -> mdd F.return) F.result
      val lddmc_project : (mdd -> mdd -> mdd F.return) F.result
      val lddmc_getvalue : (mdd -> Unsigned.uint32 F.return) F.result
      val lddmc_getdown : (mdd -> mdd F.return) F.result
      val lddmc_getright : (mdd -> mdd F.return) F.result
      val lddmc_follow : (mdd -> Unsigned.uint32 -> mdd F.return) F.result
      val lddmc_print : (mdd -> unit F.return) F.result
    end
