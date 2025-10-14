open Hypergraph_sig

module IncrementalHypergraph : IncrementalHornClauses  = struct

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct include Int let empty = 1 end)

  type t = {
    adjacency_list : AdjacencyList.t;
    reachability_status : ReachabilityStatus.t;
    compound_to_int : compound_to_int;
    next_compound_int: int;
  }

  let compound_to_int t comp =
    let compound_to_int, i, next_compound_int = 
      compound_to_int t.compound_to_int t.next_compound_int comp
    in {t with compound_to_int; next_compound_int}, i

  let init ~nr_vars = 
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status = 
        ReachabilityStatus.add True 0 (ReachabilityStatus.empty nr_vars) IdSetMap.empty;
      compound_to_int = IdSetMap.empty;
      next_compound_int = 0;
    }

  let is_reachable node t = 
    0 = ReachabilityStatus.get node t.reachability_status t.compound_to_int

  let decrement_reachability t node = 
    let previous_i = 
      ReachabilityStatus.get node t.reachability_status t.compound_to_int in 
    {t with 
     reachability_status =
       ReachabilityStatus.add node (previous_i - 1) t.reachability_status t.compound_to_int}

  let succs_of t node =
    AdjacencyList.get node t.adjacency_list t.compound_to_int

  let add_successor node ~succ:succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      t
    else
      let new_set = NodeSet.add succ successors in
      let adjacency_list = 
        AdjacencyList.add node new_set t.adjacency_list t.compound_to_int in
      {t with adjacency_list}

  let add_node node t =
    match node with
    | Node.Compound set ->
      let t,_ = compound_to_int t set in 
      let reach = IdSet.fold (fun node reach ->
          let status = 
            ReachabilityStatus.get (Simple node) t.reachability_status t.compound_to_int
          in reach + status
        ) set 0 in
      let reachability_status = 
        ReachabilityStatus.add node reach t.reachability_status t.compound_to_int
      in
      {t with reachability_status}
    | Node.Simple _ | Node.True -> t

  (* ---------- Add edges ---------- *)

  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2 

  let add_hyperedge t tail head =
    if Node.is_reflexive_edge tail head then 
      (*reflexive edges are not allowed*)
      t
    else 
      let t = 
        if Node.is_simple tail then t else 
          (* add nodes from the simple edges to the compound edge "tail" *)
          Node.fold (
            fun i acc_hg -> add_simple_edge i tail acc_hg
          ) tail (add_node tail t)
      in
      add_simple_edge tail head t

  let rec closure t node =
    if is_reachable node t then 
      t 
    else
      let t = decrement_reachability t node in
      if is_reachable node t then 
        let succs = succs_of t node in 
        NodeSet.fold (fun succ t ->
            closure t succ) succs t
      else
        t

  type edge = Node.t * Node.t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x) t =
    let t = add_hyperedge t x y in
    if is_reachable x t then 
      closure t y 
    else t

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  let to_dot hg int_to_var =
    let b = Buffer.create 1024 in
    Buffer.add_string b "digraph hypergraph {\n";
    Buffer.add_string b "  rankdir=LR;\n";
    Buffer.add_string b "  node [fontsize=10, penwidth=1.2];\n";
    let nodes = 
      (Node.True) :: 
      List.init (Array.length hg.reachability_status.simple_nodes_map) 
        (fun x -> Node.Simple x) @ 
      List.map (fun (a,_) -> (Node.Compound a)) (IdSetMap.bindings hg.compound_to_int) 
    in
    (* Add nodes to buffer *)
    List.iter (fun node ->
        let reach = ReachabilityStatus.get node hg.reachability_status hg.compound_to_int in
        Node.dot_of_node node int_to_var b (Some reach)
      ) nodes;
    (* Add edges to buffer *)
    List.iter (fun node ->
        NodeSet.iter (fun successor ->
            Node.dot_of_edge node successor b 
          ) (succs_of hg node)
      ) nodes;
    Buffer.add_string b "}\n";
    Buffer.contents b

end

include IncrementalHypergraph