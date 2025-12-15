open Ctypes
open Foreign

type mdd

let lace_start = foreign "lace_start" (uint @-> size_t @-> returning void)

let sylvan_set_limits = foreign "sylvan_set_limits" (size_t @-> int @-> int @-> returning void)

let sylvan_init_package = foreign "sylvan_init_package" (void @-> returning void)

let sylvan_init_mtbdd = foreign "sylvan_init_mtbdd" (void @-> returning void)