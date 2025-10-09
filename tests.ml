type action = Add | Remove

let t1 = [
  (Add, "a:=b,c", []);
  (Add, "b:=", []);
  (Add, "c:=", ["a","b","c"]);
]

let run_test = ()
  

let run_tests () = ()