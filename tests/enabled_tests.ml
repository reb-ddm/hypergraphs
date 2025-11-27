open GenerateTests
open RegressionTests

let _generated_tests test_add2 = [test_add2 10 2; test_add2 100 2; test_add2 1000 2; test_add2 10000 2;
                           test_add2 10 20; test_add2 100 20; test_add2 1000 20; test_add2 10000 20;
                           test_add2 10 200; test_add2 100 200; test_add2 1000 200; test_add2 10000 200;
                           test_add2 10 2000; test_add2 100 2000; test_add2 1000 2000; 
                           test_add2 10000 2000;
                           test_add2 10 4000; test_add2 100 4000; test_add2 1000 4000; 
                           test_add2 10000 4000;
                          ] 
let _big_generated_tests () = [
  (* test_add2 10 4000; test_add2 100 4000; test_add2 1000 4000; test_add2 10000 4000; *)
  (* test_add2 20000 2; *)
  (* test_add2 30000 2;  *)
  test_add2 20000 4000;
  (* test_add2 10 10000; test_add2 100 10000; test_add2 1000 10000; test_add2 10000 10000; *)
]
let _generated_tests_remove () = [test_remove 10 2; test_remove 100 2; test_remove 1000 2; test_remove 10000 2;
                                  test_remove 10 20; test_remove 100 20; test_remove 1000 20; test_remove 10000 20;
                                  test_remove 10 200; test_remove 100 200; test_remove 1000 200; test_remove 10000 200;
                                  test_remove 10 2000; test_remove 100 2000; test_remove 1000 2000; test_remove 10000 2000;
                                 ] 
let _generated_tests_remove2 () = [test_remove2 10 2; test_remove2 100 2; test_remove2 1000 2; test_remove2 10000 2;
                                   test_remove2 10 20; test_remove2 100 20; test_remove2 1000 20; test_remove2 10000 20;
                                   test_remove2 10 200; test_remove2 100 200; test_remove2 1000 200; test_remove2 10000 200;
                                   test_remove2 10 2000; test_remove2 100 2000; test_remove2 1000 2000; test_remove2 10000 2000;
                                  ] 
let _generated_tests_add_and_remove () = 
  [test_add 7 13; test_add2 7 13; test_remove 7 13; test_remove2 7 13; test_remove3 7 13; test_remove4 7 13;]
  , "all generated tests small examples"

let _generated_tests_for_graphs test_gen test_name = 
  let steps = 1000 in
  let s = string_of_int steps ^ " steps " ^ test_name  in
  [test_gen 10 steps; test_gen 100 steps] @
  List.init 25 (fun j -> test_gen ((j+1)*2000) steps)
, s

let _generated_tests_for_graphs_steps test_gen test_name = 
  let size = 10000 in
  let s = "size " ^ string_of_int size ^ " " ^ test_name in
  List.init 39 (fun j -> 
      let steps = 30*(j+1) in
      let size = if size <= steps then 
          let () = print_endline "WARNING: the number of removals is bigger than the size of the graph!!" in
          size else size - steps in
      test_gen size steps)
, s

let enabled_tests () = _generated_tests_for_graphs_steps test_add2 "add 2"
let debug = false
let debug_enabled_tests () = [test_remove3 5 5]