let calculate r = 
  let test = ClassicalHypergraph.init ~nr_vars:70 in
  let test = ClassicalHypergraph.add_clause ("a", ["b"; "c"]) test in
  let dot = ClassicalHypergraph.to_dot test in
  let () = print_endline dot in
  r

let rec loop () =
  print_string "> ";
  flush stdout;
  match read_line () with
  | "quit" -> print_endline "Goodbye!"
  | input ->
      let output = calculate input in
      print_endline output;
      loop ()

let start_agent () =
  print_endline "This program bla bla please input a horn clause.";
  loop ()
