module HornClauseHandler (HC : Signatures.HornClauses) = struct
  (*TODO initial state
    TODO not add duplicate clauses?
    TODO all indexes change after removing something. This is not so good 
    TODO catch errors
    TODO maybe print for each add/delete and accept "p" "a" "r" as commands
    TODO ignore case in commands*)
  type t = 
    {
      horn_clauses_rev: (string* string list) list;
      horn_clause_handler: HC.t
    }

  exception InvalidVarName of string
  exception InvalidCharachter of string
  exception InvalidCommand of string

  let init ~nr_vars = 
    { horn_clauses_rev= []; 
      horn_clause_handler= HC.init ~nr_vars }
  let add_clause t clause = 
    { horn_clauses_rev= clause::t.horn_clauses_rev; 
      horn_clause_handler= HC.add_clause clause t.horn_clause_handler }

  let remove_clause t clause index = 
    { horn_clauses_rev = 
        List.filteri (fun i _ -> not (i = index)) t.horn_clauses_rev; 
      horn_clause_handler = 
        HC.remove_clause clause t.horn_clause_handler }

  let to_dot t = HC.to_dot t.horn_clause_handler

  (**
     returns (index, clause, bool)
     It returns true if the clause needs to be added and false if it needs to be removed.
  *)
  let parse_clause s hc =
    let is_alpha_or_digit = function 'a' .. 'z' | 'A' .. 'Z'| '0' .. '9' -> true | _ -> false in
    let is_valid_var_name s = 
      String.length s > 0 && String.for_all is_alpha_or_digit s in
    let s = String.trim s in
    if String.starts_with ~prefix:"add" s then 
      let s = String.sub s 3 (String.length s - 3) in
      match String.split_on_char ':' s with
      | [lhs] -> if is_valid_var_name lhs then -1, (lhs, []), true 
        else raise (InvalidVarName lhs)
      | [lhs; rest] ->
        let rest =
          if String.length rest > 0 && rest.[0] = '=' then
            String.sub rest 1 (String.length rest - 1)
          else
            raise (InvalidCharachter ":")
        in
        let rhs =
          rest
          |> String.trim
          |> (fun s ->
              if s = "" then []
              else
                s
                |> String.split_on_char ','
                |> List.map String.trim
            )
        in
        let () = List.iter (fun v -> if not (is_valid_var_name v) then 
                               raise (InvalidVarName v)) rhs in
        -1, (String.trim lhs, rhs), true
      | _ ->
        raise (InvalidCharachter ":")
    else if String.starts_with ~prefix:"remove" s then 
      let s = String.trim (String.sub s 6 (String.length s - 6)) in
      let n = try int_of_string s 
        with Failure s -> raise (InvalidCharachter s)
      in
      let hc_list = hc.horn_clauses_rev in
      let index_in_list = List.length hc_list - n in
      index_in_list, List.nth hc_list index_in_list, false
    else raise (InvalidCommand s)

  let to_string hc =
    let _, lines =
      List.fold_left
        (fun (i, acc) (lhs, rhs_list) ->
           let rhs = String.concat "," rhs_list
           in
           let line = Printf.sprintf "%d: %s := %s" i lhs rhs in
           (i - 1, line :: acc))
        (List.length hc.horn_clauses_rev, []) hc.horn_clauses_rev
    in
    String.concat "\n" lines
end


let calculate r = 
  let test = ClassicalHypergraph.init ~nr_vars:70 in
  let test = ClassicalHypergraph.add_clause ("a", ["b"; "c"]) test in
  let dot = ClassicalHypergraph.to_dot test in
  let () = print_endline dot in
  r

module HypergraphHCH = HornClauseHandler(ClassicalHypergraph)

let rec loop hc =
  print_string "> ";
  flush stdout;
  match String.trim (read_line ()) with
  | "quit" -> print_endline "Goodbye!"
  | "dot" -> 
    let output = HypergraphHCH.to_dot hc in
    print_endline output;
    loop hc
  | "print" -> 
    let output = HypergraphHCH.to_string hc in
    print_endline output;
    loop hc
  | input ->
    let index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
    let hc = if must_be_added then
        HypergraphHCH.add_clause hc clause
      else
        HypergraphHCH.remove_clause hc clause index 
    in
    print_endline "done";
    loop hc

let start_agent () =
  print_endline "This program bla bla please input a horn clause.";
  let hc = HypergraphHCH.init ~nr_vars:400 in 
  loop hc
