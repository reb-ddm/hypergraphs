let () =
  let ml_out = Format.formatter_of_out_channel (open_out_bin "bindings_generated.ml") in
  let c_out  = Format.formatter_of_out_channel (open_out_bin "bindings_stubs.c") in
  Cstubs.write_ml ml_out ~prefix:"" (module Sylvan_bindings.Bindings);
  Cstubs.write_c  c_out  ~prefix:"" (module Sylvan_bindings.Bindings);
  Format.pp_print_flush ml_out ();
  Format.pp_print_flush c_out ();
  ()
