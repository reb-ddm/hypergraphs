

open HcHandler
open Incremental_analysis_algorithms
open Hypergraph_sig

let help_message =
  "Available commands:\n\
   - quit\n\
   - help\n\
   - add <horn clause>\n\
   - remove <horn clause>\n\
   - print\n\
   - reachable\n\
   - dot\n\
   \n\
   Horn clause syntax:\n\
   <head> := <body>\n\
  "

let init_loop n (module HypergraphAlg : FullyDynamicHornClauses) =
  let module HypergraphHCH = HornClauseHandler(HypergraphAlg) in
  let hc = HypergraphHCH.init ~nr_vars:n in 
  let rec loop hc = 
    print_string "> ";
    flush stdout;
    let error_message s = "Invalid input: " ^ s ^ ". Please try again. Type 'help' for help." in
    try
      match String.trim (read_line ()) with
      | "quit" -> ()
      | "help" -> 
        print_endline help_message;
        loop hc
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
  in loop hc

let rec ask_nr_variables s =
  print_endline s;
  let s = String.trim (read_line ()) in
  match int_of_string_opt s with 
  | Some n -> n
  | None -> ask_nr_variables "Please enter a number. Input the number of variables: "

let rec ask_module s =
  print_endline s;
  match String.trim (read_line ()) with 
  | "incremental" | "i" -> (module IncrementalHypergraph : FullyDynamicHornClauses)
  | "incremental2" | "i2" -> (module IncrementalHypergraph2 : FullyDynamicHornClauses)
  | "mst" | "m" -> (module FullyDynamicHypergraph : FullyDynamicHornClauses)
  | "parametric" | "p" -> (module ParametricHypergraph : FullyDynamicHornClauses)
  | "semiparametric"| "s" -> (module SemiParametricHypergraph : FullyDynamicHornClauses)
  | _ -> ask_module "Please enter 'incremental' or 'mst' or 'parametric'. "

let start_agent () =
  let n = ask_nr_variables "Input the number of variables: " in
  let module HypergraphAlg = (val (ask_module "Input the algorithm version: (incremental/mst/parametric)")) in
  print_endline "Please input a horn clause.";
  init_loop n (module HypergraphAlg)
