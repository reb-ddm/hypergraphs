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

let _simple_tests = [t1; t2; t3; t4; t5]
let _generated_tests () = [test_add2 10 2; test_add2 100 2; test_add2 1000 2; test_add2 10000 2;
                           test_add2 10 20; test_add2 100 20; test_add2 1000 20; test_add2 10000 20;
                           test_add2 10 200; test_add2 100 200; test_add2 1000 200; test_add2 10000 200;
                           test_add2 10 2000; test_add2 100 2000; test_add2 1000 2000; test_add2 10000 2000;
                          ] 
let big_generated_tests () = [
  (* test_add2 10 4000; test_add2 100 4000; test_add2 1000 4000; test_add2 10000 4000; *)
(* test_add2 20000 2; *)
 (* test_add2 30000 2;  *)
   test_add2 10 10000; test_add2 100 10000; test_add2 1000 10000; test_add2 10000 10000;

]
let enabled_tests = big_generated_tests () (*they generate before the program starts running*)