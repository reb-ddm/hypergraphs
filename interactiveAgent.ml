

open HcHandler
open Incremental_analysis_algorithms

module HypergraphHCH = HornClauseHandler(FullyDynamicHypergraph)

let help_message =
  "Available commands:
  - quit
  - help
  - add <horn clause>
  - remove <horn clause>
  - print
  - reachable
  - dot 

  Horn clause syntax:
    <head> := <body>
  "

let rec loop hc =
  print_string "> ";
  flush stdout;
  let error_message s = "Invalid input: " ^ s ^ ". Please try again. Type 'help' for help." in
  try
    match String.trim (read_line ()) with
    | "quit" -> ()
    | "help" -> 
      print_endline help_message
    | "dot" -> 
      let output = HypergraphHCH.to_dot hc in
      print_endline output;
      loop hc
    | "print" -> 
      let output = HypergraphHCH.to_string hc in
      print_endline output;
      loop hc
    | "reachable" -> 
      let output = HypergraphHCH.reachable_vars_string hc in
      print_endline output;
      loop hc
    | input ->
      let hc, index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
      let hc = if must_be_added then
          HypergraphHCH.add_clause clause hc
        else
          HypergraphHCH.remove_clause hc clause index 
      in
      print_endline "done";
      loop hc
  with 
  | FullyDynamicHypergraph.UnknownNodeId (loc, node) ->
    print_endline ("Exception: UnknownNodeId " ^ Hypergraph_sig.Node.to_string node ^ " in " ^ loc)
  | Hypergraph_sig.ExceededNrOfVars ->
    print_endline ("Exception: ExceededNrOfVars")
  | InvalidVarName s ->
    print_endline (error_message ("Not a valid variable name: " ^ s));
    loop hc
  | InvalidCharachter s ->
    print_endline (error_message ("Unexpected charachter: " ^ s));
    loop hc
  | InvalidCommand s ->
    print_endline (error_message ("Command not recognized: " ^ s));
    loop hc

let rec ask_nr_variables s =
  print_endline s;
  let s = String.trim (read_line ()) in
  match int_of_string_opt s with 
  | Some n -> n
  | None -> ask_nr_variables "Please enter a number. Input the number of variables: "

let start_agent () =
  print_endline "This program bla bla please input a horn clause.";
  let n = ask_nr_variables "Input the number of variables: " in
  let hc = HypergraphHCH.init ~nr_vars:n in 
  loop hc
