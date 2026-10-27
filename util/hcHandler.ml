open Hypergraph_sig

exception InvalidVarName of string
exception InvalidCharachter of string
exception InvalidCommand of string
exception InvalidIndex of string

module type HornClauseHandlerType = sig 
  type clause = int * IdSet.t
  type t

  val init : nr_vars:int -> t
  val add_clause : int * IdSet.t -> t -> t
  val clause_of_index : int -> t -> clause option
  val reachable_vars : t -> string list

  val first_reachable_vars : t -> string Seq.t
  (**Returns only the first 1000 reachable vars.*)

  val reachable_vars_string : t -> string
  val to_dot : t -> string

  type parse_output =
    | Add of clause list
    | Remove of clause * int
    | OpenChapter of clause list
    | CloseChapter

  val print_clause : t -> clause -> unit
  val parse_clause : string -> t -> t * parse_output
  val to_string : t -> string
  val remove_clause : t -> int * IdSet.t -> int -> t
  val open_chapter : t -> clause list -> t
  val close_chapter : t -> t
end

module HornClauseHandler (HC : FullyDynamicHornClauses) = struct
  (*TODO all indexes change after removing something. This is not so good
    TODO maybe print for each add/delete and accept "p" "a" "r" as commands
    TODO ignore case in commands
    TODO better help text*)
  type clause = int * IdSet.t
  type hc_schema = string * string

  type t = {
    vars: Vars.t;
    horn_clause_schemas: hc_schema list;
    horn_clauses_array: clause option Res.Array.t;
    horn_clause_handler: HC.t;
    compound_to_int: CompoundIntConversion.t;
  }

  let print_clause hc (head, body) =
    print_string (Vars.int_to_var hc.vars head);
    print_string " := ";
    let var_names = IdSet.fold (fun v l -> Vars.int_to_var hc.vars v :: l) body [] in
    print_endline (String.concat ", " var_names)

  let update_vars hc vars' = { hc with vars = vars' }

  let insert_var hc var =
    let vars', id, is_new = Vars.var_to_int hc.vars var in
    update_vars hc vars', id, is_new

  let init ~nr_vars =
    (*compound node for True*)
    let cti, true_node =
      Node.compound_node_of_intset (IdSet.empty ()) CompoundIntConversion.empty
    in
    {
      vars = Vars.init ~nr_vars;
      horn_clause_schemas = [];
      horn_clauses_array = Res.Array.empty ();
      horn_clause_handler = HC.init ~nr_vars true_node;
      compound_to_int = cti;
    }

  let clause_to_edge (head, body) hc =
    let head_node = Node.simple_node_of_id head in
    if IdSet.cardinal body = 1 then
      (head_node, Node.simple_node_of_id (IdSet.choose body), body), hc
    else (
      let cti, node = Node.compound_node_of_intset body hc.compound_to_int in
      (head_node, node, body), { hc with compound_to_int = cti }
    )

  let add_clause (head, body) hc =
    let edge, hc = clause_to_edge (head, body) hc in
    let () = Res.Array.add_one hc.horn_clauses_array (Some (head, body)) in
    let horn_clause_handler = HC.add_clause edge hc.horn_clause_handler in
    { hc with horn_clause_handler }

  let clause_of_index i hc = Res.Array.get hc.horn_clauses_array i

  let reachable_vars hc =
    Vars.get_vars_in_alphabetical_order hc.vars
    |> Seq.filter_map (fun (name, id) ->
        if HC.is_reachable (Node.simple_node_of_id id) hc.horn_clause_handler
        then
          Some name
        else
          None)
    |> List.of_seq |> List.rev
  (* let r_vars = HC.reachable_vars (Vars.get_nr_vars hc.vars) hc.horn_clause_handler in
     IdSet.fold (fun i vars -> Vars.int_to_var hc.vars i::vars) r_vars []
     |> List.rev *)

  let first_reachable_vars hc =
    Vars.get_vars_in_alphabetical_order hc.vars
    |> Seq.take 1000
    |> Seq.filter_map (fun (name, id) ->
        if HC.is_reachable (Node.simple_node_of_id id) hc.horn_clause_handler
        then
          Some name
        else
          None)

  let reachable_vars_string hc = String.concat ", " (reachable_vars hc)

  let to_dot hg =
    let b = Buffer.create 1024 in
    Buffer.add_string b "digraph hypergraph {\n";
    Buffer.add_string b "  rankdir=LR;\n";
    Buffer.add_string b "  node [fontsize=10, penwidth=1.2];\n";
    let nodes =
      List.map (fun (x, _) -> Node.Simple x) (Vars.get_vars hg.vars)
      @ List.map
        (fun (_, id) -> Node.Compound id)
        (CompoundIntConversion.bindings hg.compound_to_int)
    in
    (* Add nodes to buffer *)
    List.iter
      (fun node ->
         let reach = HC.weight_of hg.horn_clause_handler node in
         Node.dot_of_node node (Vars.int_to_var hg.vars) b reach)
      nodes;
    (* Add edges to buffer *)
    List.iter
      (fun node ->
         NodeSet.iter
           (fun successor -> Node.dot_of_edge node successor b)
           (HC.succs_of hg.horn_clause_handler node))
      nodes;
    Buffer.add_string b "}\n";
    Buffer.contents b

  let filter_regex pattern strings =
    let re = Re.Perl.compile_pat ("^" ^ pattern ^ "$") in
    List.filter (fun s -> Re.execp re s) strings

  let rec eval_hc_schemes new_vars new_hc_schemas hc = 
    if new_vars = [] then hc, [] 
    else 
      let rec aux hc = 
        function
        | [] -> hc, []
        | ((regex, clause) :: hc_schemes) ->
          let hc, other_clauses = aux hc hc_schemes in
          let matching_vars = filter_regex regex new_vars in
          let new_clauses = 
            List.map (fun v -> String.concat v (String.split_on_char 'v' clause)) matching_vars in
          let hc, new_clauses = 
            (List.fold_left_map 
               (fun hc c -> parse_added_clause c hc) hc new_clauses) in
          hc, List.flatten new_clauses @ other_clauses in
      aux hc new_hc_schemas
  and parse_added_clause s hc =
    let is_alpha_or_digit = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let is_valid_var_name s =
      String.length s > 0 && String.for_all is_alpha_or_digit s
    in
    let head, body = 
      match String.split_on_char ':' s with
      | [ lhs ] ->
        if is_valid_var_name lhs then
          lhs, []
        else
          raise (InvalidVarName lhs)
      | [ lhs; rest ] ->
        let rest =
          if String.length rest > 0 && rest.[0] = '=' then
            String.sub rest 1 (String.length rest - 1)
          else
            raise (InvalidCharachter ":")
        in
        let rhs =
          rest |> String.trim |> fun s ->
          if s = "" then
            []
          else
            s |> String.split_on_char ',' |> List.map String.trim
        in
        let () =
          List.iter
            (fun v ->
               if not (is_valid_var_name v) then raise (InvalidVarName v))
            rhs
        in
        String.trim lhs, rhs
      | _ -> raise (InvalidCharachter ":")
    in
    let hc, head_id, head_is_new = insert_var hc head in
    let new_vars = if head_is_new then [head] else [] in
    let hc, body_set, new_vars =
      List.fold_left
        (fun (hc, ids, new_vars) v ->
           let hc, id, is_new = insert_var hc v in
           hc, IdSet.add id ids, if is_new then v::new_vars else new_vars)
        (hc, IdSet.empty (), new_vars)
        body
    in
    let hc, new_clauses = eval_hc_schemes new_vars hc.horn_clause_schemas hc in
    hc, (head_id, body_set) :: new_clauses

  type parse_output =
    | Add of clause list
    | Remove of clause * int
    | OpenChapter of clause list
    | CloseChapter

  let parse_clause s hc =
    let s = String.trim s in
    (* ---- ADD SCHEMA ---- *)
    if String.starts_with ~prefix:"add schema" s then (
      let s = String.trim (String.sub s 10 (String.length s - 10)) in
      if String.starts_with ~prefix:"v" s then
        let s = String.trim (String.sub s 1 (String.length s - 1)) in
        if String.starts_with ~prefix:"=" s then
          let s = String.trim (String.sub s 1 (String.length s - 1)) in
          let s = String.split_on_char '#' s in
          if List.length s = 2 then 
            let hc_scheme = (String.trim (List.hd s), String.trim (List.hd (List.tl s))) in
            let vars = List.map fst (List.of_seq (Vars.get_vars_in_alphabetical_order hc.vars)) in
            let hc, new_clauses = eval_hc_schemes vars [hc_scheme] {hc with horn_clause_schemas = hc_scheme::hc.horn_clause_schemas} in
            hc, Add new_clauses
          else raise (InvalidCharachter ("Invalid schema: " ^ String.concat "" s))
        else raise (InvalidCharachter ("Invalid schema: " ^ s))
      else raise (InvalidCharachter ("Invalid schema: " ^ s))
    ) else
      (* ---- ADD ---- *)
    if String.starts_with ~prefix:"add" s then (
      let s = String.trim (String.sub s 3 (String.length s - 3)) in
      let hc, new_clauses = parse_added_clause s hc in
      hc, Add new_clauses
      (* ---- REMOVE ---- *)
    ) else if String.starts_with ~prefix:"remove" s then (
      let s = String.trim (String.sub s 6 (String.length s - 6)) in
      let n =
        try int_of_string s
        with Failure f -> raise (InvalidCharachter (s ^ " in function " ^ f))
      in
      let hc_list = hc.horn_clauses_array in
      match Res.Array.get hc_list n with
      | Some clause -> hc, Remove (clause, n)
      | None | (exception Invalid_argument _) ->
        raise
          (InvalidIndex ("There is no horn clause with index " ^ string_of_int n))
          (* ---- OPEN ---- *)
    ) else if String.starts_with ~prefix:"open" s then (
      let s = String.trim (String.sub s 4 (String.length s - 4)) in
      (* "open ()" -> empty list *)
      if s = "()" || s = "" then
        hc, OpenChapter []
      else (
        let s =
          if
            String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')'
          then
            String.sub s 1 (String.length s - 2) |> String.trim
          else
            s
        in
        let parts = String.split_on_char ',' s |> List.map String.trim in
        let indices =
          List.concat_map
            (fun p ->
               match String.split_on_char '-' p |> List.map String.trim with
               | [ a ] ->
                 (match int_of_string_opt a with
                  | None -> raise (InvalidCharachter ("Invalid index: " ^ p))
                  | Some i -> [ i ])
               | [ a; b ] ->
                 let a, b = int_of_string a, int_of_string b in
                 if a > b then
                   raise (InvalidCharachter ("Invalid range: " ^ p))
                 else
                   List.init (b - a + 1) (fun i -> a + i)
               | _ -> raise (InvalidCharachter ("Invalid range syntax: " ^ p)))
            parts
        in
        let hc_list = hc.horn_clauses_array in
        let selected_clauses =
          indices
          |> List.map (fun n ->
              match Res.Array.get hc_list n with
              | Some clause -> clause
              | None | (exception Invalid_argument _) ->
                raise
                  (InvalidIndex
                     ("There is no horn clause with index " ^ string_of_int n)))
        in
        hc, OpenChapter selected_clauses
        (* ---- CLOSE ---- *)
      )
    ) else if String.equal "close" s then
      hc, CloseChapter
    else
      raise (InvalidCommand s)

  let to_string hc =
    let _, lines =
      Res.Array.fold_left
        (fun (i, acc) clause_opt ->
           match clause_opt with
           | None -> i + 1, acc
           | Some (head, body) ->
             let lhs = Vars.int_to_var hc.vars head in
             let rhs_list =
               List.map
                 (fun x -> Vars.int_to_var hc.vars x)
                 (List.of_seq (IdSet.to_seq body))
             in
             let rhs = String.concat "," rhs_list in
             let line = Printf.sprintf "%d: %s := %s" i lhs rhs in
             i + 1, line :: acc)
        (0, []) hc.horn_clauses_array
    in
    String.concat "\n" (List.rev lines)

  let remove_clause hc (head, body) _index =
    (* let () = Res.Array.set hc.horn_clauses_array index None in *)
    let clause, hc = clause_to_edge (head, body) hc in
    {
      hc with
      horn_clause_handler = HC.remove_clause clause hc.horn_clause_handler;
    }

  let open_chapter hc clause_list =
    let clauses, hc =
      List.fold_left
        (fun (clauses, hc) clause ->
           let cl, hc = clause_to_edge clause hc in
           cl :: clauses, hc)
        ([], hc) clause_list
    in
    {
      hc with
      horn_clause_handler = HC.open_chapter clauses hc.horn_clause_handler;
    }

  let close_chapter hc =
    { hc with horn_clause_handler = HC.close_chapter hc.horn_clause_handler }
end
