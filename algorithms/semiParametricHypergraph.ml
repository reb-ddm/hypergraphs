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
  let open_chapter _ t = t

end

include SemiParametricHypergraph
