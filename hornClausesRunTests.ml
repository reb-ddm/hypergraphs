open HornClausesTests
(* open Enabled_tests *)

let _run_tests (enabled_tests, s) =
  let module HypergraphAlg = (val InteractiveAgent.ask_module ()) in
  let module RunTestsIncremental = RunTests (HypergraphAlg) in
  RunTestsIncremental.run_tests
    ("BENCHMARKS " ^ HypergraphAlg.name ^ " Hypergraph " ^ s)
    enabled_tests

(* let _ =
  run_tests
    (if debug then
       debug_enabled_tests (), ""
     else
       enabled_tests ()) *)
let uzero = Unsigned.UInt.of_int 0
let sizezero = Unsigned.Size_t.of_int 0
let size = Unsigned.Size_t.of_int (512*1024*1024)

let _ = Sylvan_bindings.lace_start uzero sizezero
let _ = Sylvan_bindings.sylvan_set_limits size 1 5

