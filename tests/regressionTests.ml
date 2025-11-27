
let t1 = "very_simple_add", None, 3, [
    ("add a:=b,c", []);
    ("add b:=", ["b"]);
    ("add c:=", ["a";"b";"c"]);
  ]

let t2 = "very_simple_remove", None, 2, [
    ("add a:=", ["a"]);
    ("add b:=", ["a";"b"]);
    ("remove 0", ["b"]);
    ("remove 1", []);
  ]

let t3 = "simple_loop", None, 6, [
    ("add x1:=", ["x1"]);
    ("add x2:=", ["x1";"x2"]);
    ("add x3:=", ["x1";"x2";"x3"]);
    ("add x4:=", ["x1";"x2";"x3";"x4"]);
    ("add x5:=x1,x2", ["x1";"x2";"x3";"x4";"x5"]);
    ("add x6:=x3,x4,x5", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("add x5:=x6", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("remove 4", ["x1";"x2";"x3";"x4"]);
  ]

let t4 = "simple_loop_wrong_order", None, 6, [
    ("add x5:=x1,x2", []);
    ("add x1:=", ["x1"]);
    ("add x6:=x3,x4,x5", ["x1"]);
    ("add x5:=x6", ["x1"]);
    ("add x3:=", ["x1"; "x3"]);
    ("add x4:=", ["x1"; "x3"; "x4"]);
    ("add x2:=", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    (* ("add x2:=", ["x5";"x1";"x2";"x6";"x3";"x4"]); *)
    ("remove 0", ["x1";"x2";"x3";"x4"]);
    ("add x6:=x4", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    (* ("add x6:=x4", ["x5";"x1";"x2";"x6";"x3";"x4"]); *)
  ]

let t5 = "reflexive_edge", None, 3, [
    ("add a:=a", []);
    ("remove 0", []);
    ("add a:=f,a,w", []);
    ("remove 1", []);
  ]

let t6 = "long_chain", None, 7, [
    ("add a:=", ["a"]);
    ("add b:=a", ["a";"b"]);
    ("add d:=a,b", ["a";"b";"d"]);
    ("add c2:=c", ["a";"b";"d"]);
    ("add d:=c2", ["a";"b";"d"]);
    ("add e:=d", ["a";"b";"d";"e"]);
    ("add f:=e", ["a";"b";"d";"e";"f"]);
    ("add c:=b", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("add e:=a", ["a";"b";"c";"c2";"d";"e";"f"]);
    (* ("add c:=b", ["a";"b";"d";"c2";"c";"e";"f"]);
       ("add e:=a", ["a";"b";"d";"c2";"c";"e";"f"]); *)
    ("remove 1", ["a";"e";"f"]);
    ("remove 0", []);
    ("remove 2", []);
    ("remove 3", []);
    ("remove 4", []);
    ("remove 5", []);
    ("remove 6", []);
    ("remove 7", []);
    ("remove 8", []);
  ]

let t7 = "long_chain_v2_", None, 8, [
    ("add a:=", ["a"]);
    ("add d:=a,b", ["a"]);
    ("add c2:=c", ["a"]);
    ("add d:=c2", ["a"]);
    ("add e:=d", ["a"]);
    ("add f:=e", ["a"]);
    ("add b:=a", ["a";"b";"d";"e";"f"]);
    ("add c:=b", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("add e:=a", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("remove 1", ["a";"b";"c";"c2";"d";"e";"f"]);
    ("add g:=a,b,c,d,e,f", ["a";"b";"c";"c2";"d";"e";"f";"g"]);
    ("remove 4", ["a";"b";"c";"c2";"d";"e";"f";"g"]);
    (* ("add b:=a", ["a";"d";"b";"e";"f"]);
       ("add c:=b", ["a";"d";"b";"c2";"c";"e";"f"]);
       ("add e:=a", ["a";"d";"b";"c2";"c";"e";"f"]);
       ("remove 2", ["a";"d";"b";"c2";"c";"e";"f"]);
       ("add g:=a,b,c,d,e,f", ["a";"d";"b";"c2";"c";"e";"f";"g"]);
       ("remove 4", ["a";"d";"b";"c2";"c";"e";"f";"g"]); *)
    ("remove 7", ["a";"b";"e";"f"]);
    ("remove 6", ["a";"e";"f"]);
    ("add g:=a,d,e,f", ["a";"e";"f"]);
    ("add  d:=a,f,e", ["a";"d";"e";"f";"g"]);
    ("remove 0", []);
  ]

let t8 = "remove2_of_size_2_with_2_steps", None, 4, [
    ("close",[]);
    ("add a1:=",["a1"]);
    ("add x0:=a1",["a1";"x0"]);
    ("add x1:=a1", ["a1";"x0";"x1"]);
    ("add y:=x0,x1", ["a1";"x0";"x1";"y"]);
    ("open 0", ["a1";"x0";"x1";"y"]);
    ("remove 0", []);
    ("add a1:=",["a1";"x0";"x1";"y"])
  ]

let t9 = "chapters_small_test", None, 7, [
    ("close",[]);
    ("add a1:=", ["a1"]);
    ("add a2:=", ["a1"; "a2"]);
    ("add a3:=", ["a1"; "a2"; "a3"]);
    ("add a4:=", ["a1"; "a2"; "a3"; "a4"]);
    ("add a5:=a1,a2,a3,a4", ["a1"; "a2"; "a3"; "a4"; "a5"]);
    ("open 0-2,4", ["a1"; "a2"; "a3"; "a4"; "a5"]);
    ("remove 4", ["a1"; "a2"; "a3"; "a4"]);
    ("remove 2", ["a1"; "a2"; "a4"]);
    ("remove 1", ["a1"; "a4"]);
    ("remove 0", ["a4"]);
  ]

let t10 = "close_chapter_small_test", None, 2, [
    ("add a:=b",[]);
    ("add b:=", ["a"; "b"]);
    ("remove 0", ["b"]);
    ("close", ["b"]);
    ("open 1", ["b"]);
    ("remove 1", [])
  ]

let t11 = "two_equal_edges", None, 3, [
    "add a:=c", [];
    "add c:=", ["a"; "c"];
    "add c:=", ["a";"c"];
    "add c:=", ["a";"c"];
    "add c:=", ["a";"c"];
    "add c:=", ["a";"c"];
    "remove 1", ["a";"c"];
    "remove 2", ["a";"c"];
    "remove 3", ["a";"c"];
    "remove 4", ["a";"c"];
    "remove 5", [];
  ]

let t12 = "remove_and_then_add", None, 2, [
    "add y:=x", [];
    "remove 0", [];
    "add y:=", ["y"]
  ]

let t13 = "alternative_local_path", None, 5, [
    "close", [];
    "add a:=", ["a"];
    "add b:=a", ["a"; "b"];
    "add c:=b", ["a"; "b"; "c"];
    "add d:=c", ["a"; "b"; "c"; "d"];
    "add b2:=b", ["a"; "b"; "b2"; "c"; "d"];
    "add c:=b2", ["a"; "b"; "b2"; "c"; "d"];
    "open 2, 3", ["a"; "b"; "b2"; "c"; "d"];
    "remove 2", ["a"; "b"; "b2"; "c"; "d"];
    "remove 3", ["a"; "b"; "b2"; "c"];
  ]

let t14 = "alternative_local_path_compound_node", None, 8, [
    "close", [];
    "add x1:=", ["x1"];
    "add x2:=x1", ["x1"; "x2"];
    "add x3:=x2", ["x1"; "x2"; "x3"];
    "add x4:=x2", ["x1"; "x2"; "x3"; "x4"];
    "add x5:=x3,x4", ["x1"; "x2"; "x3"; "x4"; "x5"];
    "add x6:=x5", ["x1"; "x2"; "x3"; "x4"; "x5"; "x6"];
    "add x7:=x3", ["x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7"];
    "add x8:=x7", ["x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7"; "x8"];
    "add x5:=x8", ["x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7"; "x8"];
    "open 4", ["x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7"; "x8"];
    "remove 4", ["x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7"; "x8"];
  ]

let t15 = "no_local_path", None, 5, [
    "close", [];
    "add a:=", ["a"];
    "add b:=a", ["a"; "b"];
    "add c:=b", ["a"; "b"; "c"];
    "add d:=c", ["a"; "b"; "c"; "d"];
    "add c:=b,d", ["a"; "b"; "c"; "d"];
    "open 2, 3", ["a"; "b"; "c"; "d"];
    "remove 2", ["a"; "b"];
    "remove 3", ["a"; "b"];
    "add d:=b", ["a"; "b"; "c"; "d"];
  ]

let simple_tests = [t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t11; t12; t13; t14; t15], "simple tests"