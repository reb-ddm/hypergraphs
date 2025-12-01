open Hypergraph_sig
let add_hyperedge add_compound_node add_simple_edge add_simple_edge2 nop tail_set tail head hg =
  if Node.is_reflexive_edge tail_set head then 
    (*reflexive edges are not allowed*)
    nop hg
  else 
    let hg = 
      if Node.is_simple tail then hg else 
        (* add nodes from the simple edges to the compound edge "tail" *)
        hg |> add_compound_node tail tail_set
        |> IdSet.fold (
          fun i hg -> add_simple_edge (Node.Simple i) tail hg
        ) tail_set
    in
    add_simple_edge2 tail head hg

let add_clauses add_clause cls hg =
  List.fold_left (fun acc c -> add_clause c acc) hg cls

let remove_hyperedge remove_simple_edge is_empty succs_of remove_successor remove_compound_node tail_set tail head hg =
  let hg = remove_simple_edge tail head hg in
  if Node.is_compound tail && not (Node.is_true tail) && is_empty (succs_of hg tail) then
    let hg = 
      IdSet.fold (fun i ->
          remove_successor (Node.Simple i) ~succ:tail
        ) tail_set hg
    in
    remove_compound_node hg tail
  else hg

let reachable_vars succs_of is_reachable true_node fold nr_vars hg = 
  let vars = Array.make nr_vars false in
  let rec find_next_reachable node reachable_vars =
    let visit_successor_simple_node succ i reachable_vars =
      if vars.(i) then 
        reachable_vars
      else
      if is_reachable succ hg then
        let () = vars.(i) <- true in
        find_next_reachable succ (IdSet.add i reachable_vars)
      else
        reachable_vars in
    fold (fun reachable_vars succ ->
        match succ with
        | Node.Simple i ->
          visit_successor_simple_node succ i reachable_vars
        | Node.Compound _ ->
          fold (fun reachable_vars succ ->
              match succ with
              | Node.Simple i ->
                visit_successor_simple_node succ i reachable_vars
              | Node.Compound _ -> reachable_vars) 
            reachable_vars (succs_of hg succ)
      ) reachable_vars (succs_of hg node)
  in
  find_next_reachable true_node (IdSet.empty ())

let update_successors_after_removal is_reachable increment_reachability_compound 
    increment_reachability_simple 
    visit_node succs_of y c_set hg =
  let rec aux y c_set hg =
    (* visit successors if y is reachable *)
    let was_reachable = is_reachable y hg in
    let hg, c_set = 
      if Node.is_compound y then 
        increment_reachability_compound hg y, c_set else
      if was_reachable then 
        increment_reachability_simple hg y, y::c_set
        (*c_set contains only simple nodes*)
      else 
        hg, c_set in
    let hg, c_set = if was_reachable then
        NodeSet.fold (fun v (hg, c_set) ->
            (*only visit the the successors whose min_pred is y*)
            if Node.is_compound v || visit_node v y hg then
              aux v c_set hg
            else
              hg, c_set
          ) (succs_of hg y) (hg, c_set)
      else hg, c_set
    in
    hg, c_set in
  aux y c_set hg

let propagate is_reachable preds_of hg closure c_set =
  (* propagate phase *)
  (* build the set of nodes we need to process *)
  List.fold_left (fun hg z ->
      (* if there is a reachable predecessor, then the node is still reachable. *)
      if not (is_reachable z hg) then 
        let preds = preds_of hg z in
        let found =
          NodeSet.to_seq preds |> Seq.find (fun p -> is_reachable p hg)
        in
        match found with
        | Some pred ->
          closure z ~pred hg
        | None ->
          hg
      else hg
    ) hg c_set
