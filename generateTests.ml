let test_add size =
  let nr_digits = String.length (string_of_int size) in
  let string_of_int_with_n_digits n = 
    let s = string_of_int n in
    let prefix = String.make (nr_digits - String.length s) '0' in
    prefix ^s in
  let name = "generated_test_add_of_size_" ^ string_of_int_with_n_digits size ^ "_" in
  let tests = ["add a2:=a1", ["a1";"a2"];
               "add a1:=", ["a1"]; ] in
  let reachable_vars = ["a1";"a2"] in
  let vars = List.init size (fun i -> "x" ^ string_of_int_with_n_digits i) in 
  let tests, reachable_vars = List.fold_left (fun (tests, reachable_vars) var -> 
      let reachable_vars = reachable_vars @ [var] in 
      ("add " ^ var ^ ":=a2", reachable_vars)::tests, reachable_vars
    ) (tests, reachable_vars) vars in
  let rec next_layer var_prefix nr_vars tests reachable_vars =
    if nr_vars = 1 then 
      tests, reachable_vars
    else
      let next_prefix = "x" ^ var_prefix in
      let next_size = nr_vars / 2 in
      let vars = List.init next_size (fun i -> next_prefix ^ string_of_int_with_n_digits i) in 
      let tests, reachable_vars, _ = List.fold_left (fun (tests, reachable_vars, i) var -> 
          let reachable_vars = reachable_vars @ [var] in 
          let v1 = var_prefix ^ string_of_int_with_n_digits (i * 2) in
          let v2 = var_prefix ^ string_of_int_with_n_digits (i * 2 + 1) in
          ("add " ^ var ^ ":=" ^v1^","^v2, reachable_vars)::tests, reachable_vars, i + 1
        ) (tests, reachable_vars, 0) vars in
      next_layer next_prefix next_size tests reachable_vars
  in
  let tests, reachable_vars = next_layer "x" size tests reachable_vars in
  let tests = ("add a2:=", reachable_vars)::tests in
  name, List.rev tests

