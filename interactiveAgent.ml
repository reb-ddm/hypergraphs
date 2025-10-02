

let rec loop () =
  print_string "> ";
  flush stdout;
  match read_line () with
  | "quit" -> print_endline "Goodbye!"
  | input ->
      let output = Hypergraphs.calculate input in
      print_endline output;
      loop ()

let start_agent () =
  print_endline "This program bla bla please input a horn clause.";
  loop ()
