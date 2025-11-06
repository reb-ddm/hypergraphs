open Hypergraph_sig

module SemiParametricHypergraph = struct
  include ParametricHypergraph

  let add_parametrized_edge (x, y) t =
    let parametrized_edges = EdgeSet.add (x, y) t.parametrized_edges in
    let is_parametrized = (fun edge -> EdgeSet.mem edge parametrized_edges) in
    { t with parametrized_edges; is_parametrized }

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let t, id, is_new = get_int_of_edge (x, y) t in
    if is_new then 
      let t = add_parametrized_edge (x, y) t in
      let old_mvbdu = reachability_mvbdu_of y t in
      let t = add_hyperedge x_set x y t in
      let new_mvbdu = reachability_mvbdu_of y t in
      if Edges_intbdu.equal old_mvbdu new_mvbdu then
        t
      else closure y t 
    else 
    if t.is_parametrized (x,y) then 
      add_parameter_valuation id true t
    else 
      let () = print_endline "ERROR: Can't remove an edge that is not contained in the current chapter!" in
      t

  let add_clauses =
    Hypergraphs.add_clauses add_clause


  let abstract_parameters mvbdu mvbdu_valuation parameters = 
    let mvbdu_inter = mvbdu_and mvbdu mvbdu_valuation in
    Edges_intbdu.mvbdu_project_abstract_away mvbdu_inter parameters

  let remove_parameters_for_all_nodes valuation_list parameters t =
    let valuation_list = Edges_intbdu.build_association_list valuation_list in (*TODO we probably need to reverse the lists*)
    let vars = Edges_intbdu.build_variables_list parameters in
    let mvbdu_valuation = Edges_intbdu.mvbdu_of_hconsed_asso valuation_list in
    let reachability_status = 
      ReachabilityStatus.map (fun _ mvbdu ->
          abstract_parameters mvbdu mvbdu_valuation vars
        ) t.reachability_status
    in
    {t with reachability_status}

  let open_chapter edge_list t = 
    (* build new parametrized set from edge_list *)
    let t = {t with 
             current_parameter_valuation = Res.Array.empty ();
             edge_to_int = EdgeIntConversion.empty } 
    in
    let parametrized_edges, restriction_bdu, t =
      List.fold_left 
        (fun (edges, restriction_bdu, t) (tail, head, _) ->
           let edge = tail, head in 
           let edges = EdgeSet.add edge edges in
           let t, id, _ = get_int_of_edge edge t in
           let t = add_parameter_valuation id true t in
           let range_mvbdu = Edges_intbdu.mvbdu_of_range_list ([id, (Some 0, Some 1)]) in 
           let restriction_bdu = mvbdu_and range_mvbdu restriction_bdu in
           edges, restriction_bdu, t ) 
        (EdgeSet.empty, Edges_intbdu.mvbdu_true (), t )
        edge_list
    in
    let to_unparam = EdgeSet.diff t.parametrized_edges parametrized_edges in
    let to_param = EdgeSet.diff parametrized_edges t.parametrized_edges in
    let is_parametrized = (fun edge -> EdgeSet.mem edge parametrized_edges) in
    let t = { t with parametrized_edges; is_parametrized ; restriction_bdu } in
    let valuation_list, vars = 
      EdgeSet.fold
        (fun (tail, head) (valuation_list, vars) -> 
           let _, i, is_new = get_int_of_edge (tail, head) t in
           if is_new then (valuation_list, vars)(*this should never happen*)
           else
             (i, if Res.Array.get t.current_parameter_valuation i then 1 else 0)::valuation_list, i::vars) 
        to_unparam ([],[]) 
    in
    let t = remove_parameters_for_all_nodes valuation_list vars t in
    EdgeSet.fold
      (fun (tail, head) t ->
         let old_mvbdu = reachability_mvbdu_of head t in
         let t = recalculate_mvbdu_simple_edge (tail, head) t true in
         let new_mvbdu = reachability_mvbdu_of head t in
         if Edges_intbdu.equal old_mvbdu new_mvbdu then
           t
         else closure head t 
      )
      to_param
      t 
end

include SemiParametricHypergraph
