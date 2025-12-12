open Ctypes
open Foreign

type mdd

let init_mdd = foreign "sylvan_init_package" (void @-> returning void)