#include "ctypes_cstubs_internals.h"
#include "sylvan.h"
value _1_lace_start(value x2, value x1)
{
   unsigned int x3 = ctypes_uint_val(x2);
   size_t x6 = ctypes_size_t_val(x1);
   lace_start(x3, x6);
   return Val_unit;
}
value _2_sylvan_set_limits(value x12, value x11, value x10)
{
   size_t x13 = ctypes_size_t_val(x12);
   int x16 = Long_val(x11);
   int x19 = Long_val(x10);
   sylvan_set_limits(x13, x16, x19);
   return Val_unit;
}
value _3_sylvan_init_package(value x23)
{
   sylvan_init_package();
   return Val_unit;
}
value _4_sylvan_quit(value x25)
{
   sylvan_quit();
   return Val_unit;
}
value _5_sylvan_init_ldd(value x27)
{
   sylvan_init_ldd();
   return Val_unit;
}
value _6_lddmc_makenode(value x31, value x30, value x29)
{
   uint32_t x32 = Uint32_val(x31);
   uint64_t x35 = Uint64_val(x30);
   uint64_t x38 = Uint64_val(x29);
   uint64_t x41 = lddmc_makenode(x32, x35, x38);
   return integers_copy_uint64(x41);
}
value _7_ocaml_lddmc_union_f(value x43, value x42)
{
   uint64_t x44 = Uint64_val(x43);
   uint64_t x47 = Uint64_val(x42);
   uint64_t x50 = ocaml_lddmc_union_f(x44, x47);
   return integers_copy_uint64(x50);
}
value _8_ocaml_lddmc_intersect_f(value x52, value x51)
{
   uint64_t x53 = Uint64_val(x52);
   uint64_t x56 = Uint64_val(x51);
   uint64_t x59 = ocaml_lddmc_intersect_f(x53, x56);
   return integers_copy_uint64(x59);
}
value _9_ocaml_mtbdd_support_f(value x60)
{
   uint64_t x61 = Uint64_val(x60);
   uint64_t x64 = ocaml_mtbdd_support_f(x61);
   return integers_copy_uint64(x64);
}
value _10_ocaml_lddmc_project_f(value x66, value x65)
{
   uint64_t x67 = Uint64_val(x66);
   uint64_t x70 = Uint64_val(x65);
   uint64_t x73 = ocaml_lddmc_project_f(x67, x70);
   return integers_copy_uint64(x73);
}
value _11_lddmc_getvalue(value x74)
{
   uint64_t x75 = Uint64_val(x74);
   uint32_t x78 = lddmc_getvalue(x75);
   return integers_copy_uint32(x78);
}
value _12_lddmc_getdown(value x79)
{
   uint64_t x80 = Uint64_val(x79);
   uint64_t x83 = lddmc_getdown(x80);
   return integers_copy_uint64(x83);
}
value _13_lddmc_getright(value x84)
{
   uint64_t x85 = Uint64_val(x84);
   uint64_t x88 = lddmc_getright(x85);
   return integers_copy_uint64(x88);
}
value _14_lddmc_follow(value x90, value x89)
{
   uint64_t x91 = Uint64_val(x90);
   uint32_t x94 = Uint32_val(x89);
   uint64_t x97 = lddmc_follow(x91, x94);
   return integers_copy_uint64(x97);
}
value _15_lddmc_print(value x98)
{
   uint64_t x99 = Uint64_val(x98);
   lddmc_print(x99);
   return Val_unit;
}
