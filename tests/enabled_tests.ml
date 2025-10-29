open GenerateTests

let t1 = "very_simple_add", 3, [
    ("add a:=b,c", []);
    ("add b:=", ["b"]);
    ("add c:=", ["a";"b";"c"]);
  ]

let t2 = "very_simple_remove",2, [
    ("add a:=", ["a"]);
    ("add b:=", ["a";"b"]);
    ("remove 1", ["b"]);
    ("remove 1", []);
  ]

let t3 = "simple_loop", 6, [
    ("add x1:=", ["x1"]);
    ("add x2:=", ["x1";"x2"]);
    ("add x3:=", ["x1";"x2";"x3"]);
    ("add x4:=", ["x1";"x2";"x3";"x4"]);
    ("add x5:=x1,x2", ["x1";"x2";"x3";"x4";"x5"]);
    ("add x6:=x3,x4,x5", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("add x5:=x6", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("remove 5", ["x1";"x2";"x3";"x4"]);
  ]

let t4 = "simple_loop_wrong_order", 6, [
    ("add x5:=x1,x2", []);
    ("add x1:=", ["x1"]);
    ("add x6:=x3,x4,x5", ["x1"]);
    ("add x5:=x6", ["x1"]);
    ("add x3:=", ["x1"; "x3"]);
    ("add x4:=", ["x1"; "x3"; "x4"]);
    ("add x2:=", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("remove 1", ["x1";"x2";"x3";"x4"]);
    ("add x6:=x4", ["x1";"x2";"x3";"x4";"x5";"x6"]);
  ]

let t5 = "reflexive_edge", 3, [
    ("add a:=a", []);
    ("remove 1", []);
    ("add a:=f,a,w", []);
    ("remove 1", []);
  ]

let t6 = "long_chain", 7, [
    ("add a:=", ["a"]);
    ("add b:=a", ["a";"b"]);
    ("add d:=a,b", ["a";"b";"d"]);
    ("add c2:=c", ["a";"b";"d"]);
    ("add d:=c2", ["a";"b";"d"]);
    ("add e:=d", ["a";"b";"d";"e"]);
    ("add f:=e", ["a";"b";"d";"e";"f"]);
    ("add c:=b", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("add e:=a", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("remove 2", ["a";"e";"f"]);
    ("remove 1", []);
    ("remove 1", []);
    ("remove 1", []);
    ("remove 1", []);
    ("remove 1", []);
    ("remove 1", []);
    ("remove 1", []);
    ("remove 1", []);
  ]

let t7 = "long_chain_v2_", 8, [
    ("add a:=", ["a"]);
    ("add d:=a,b", ["a"]);
    ("add c2:=c", ["a"]);
    ("add d:=c2", ["a"]);
    ("add e:=d", ["a"]);
    ("add f:=e", ["a"]);
    ("add b:=a", ["a";"b";"d";"e";"f"]);
    ("add c:=b", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("add e:=a", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("remove 2", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("add g:=a,b,c,d,e,f", ["a";"b";"c";"c2";"d";"e";"f";"g"]);
    ("remove 4", ["a";"b";"c";"c2";"d";"e";"f";"g"]);
    ("remove 6", ["a";"b";"e";"f"]);
    ("remove 5", ["a";"e";"f"]);
    ("add g:=a,d,e,f", ["a";"e";"f"]);
    ("add  d:=a,f,e", ["a";"d";"e";"f";"g"]);
    ("remove 1", []);
  ]

let t8 = "remove2_of_size_2_with_2_steps", 4, [
    ("add a1:=",["a1"]);
    ("add x0:=a1",["a1";"x0"]);
    ("add x1:=a1", ["a1";"x0";"x1"]);
    ("add y:=x0,x1", ["a1";"x0";"x1";"y"]);
    ("remove 1", []);
    ("add a1:=",["a1";"x0";"x1";"y"])
  ]

let _simple_tests = [t1; t2; t3; t4; t5; t6; t7; t8]
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
let _generated_tests_add_and_remove () = [test_add2 100 100; test_add2 1000 1000;
test_remove2 100 100; test_remove2 1000 1000]
let enabled_tests () = _generated_tests test_add2 (*they generate before the program starts running*)
let debug = false
let debug_enabled_tests () = [test_remove3 5 5]