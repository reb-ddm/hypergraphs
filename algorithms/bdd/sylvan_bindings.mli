type mdd

val lace_start : Unsigned.uint -> Unsigned.size_t -> unit
val sylvan_set_limits : Unsigned.size_t -> int -> int -> unit
val sylvan_init_package : unit -> unit
val sylvan_init_ldd : unit -> unit
val lddmc_false : mdd
val lddmc_true : mdd
val lddmc_makenode : Unsigned.uint32 -> mdd -> mdd -> mdd
val lddmc_union : mdd -> mdd -> mdd
val lddmc_intersect : mdd -> mdd -> mdd
val mtbdd_support : mdd -> mdd
val lddmc_project : mdd -> mdd -> mdd -> mdd
val lddmc_getvalue : mdd -> Unsigned.uint32
val lddmc_getdown : mdd -> mdd
val lddmc_getright : mdd -> mdd
val lddmc_follow : mdd -> Unsigned.uint32 -> mdd
val lddmc_print : mdd -> unit
val lddmc_equal : mdd -> mdd -> bool
val branch_0 : Unsigned.uint32
val branch_1 : Unsigned.uint32
