open HornClausesTests
open Enabled_tests

let run_tests (enabled_tests, s) =
  let module HypergraphAlg = (val InteractiveAgent.ask_module ()) in
  let module RunTestsIncremental = RunTests (HypergraphAlg) in
  RunTestsIncremental.run_tests
    ("BENCHMARKS " ^ HypergraphAlg.name ^ " Hypergraph " ^ s)
    enabled_tests

let _ =
  run_tests
    (if debug then
       debug_enabled_tests (), ""
     else
       enabled_tests ())

(* let main () =
     let () = Sylvan_wrapper.init () in
     let m = Sylvan_wrapper.mvbdu_of_association_list [ 0, 1; 2, 0; 3, 4 ] in
     let n = Sylvan_wrapper.mvbdu_of_association_list [ 0, 0; 2, 1 ] in
     let m = Sylvan_wrapper.mvbdu_or m n in
     Sylvan_wrapper.print m

   let _ = main () *)
