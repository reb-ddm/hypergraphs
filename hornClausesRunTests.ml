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

let main () =
  let () = Sylvan_wrapper.init () in
  let m = Sylvan_wrapper.mvbdu_of_association_list [ 0, 1; 2, 0 ] in
  let () = Sylvan_wrapper.print m in
  print_newline ()

let _ = main ()
