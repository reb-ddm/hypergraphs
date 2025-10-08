
module Id = struct
  type t = int
  let compare = Stdlib.compare
end

module IdSet = Set.Make(Id)

module Node = struct
  type t =
    | Simple of Id.t
    | Compound of IdSet.t (*this represents the source set of an edge*)

  let is_simple = function Simple _ -> true | Compound _ -> false
  let is_compound = function Simple _ -> false | Compound _ -> true

  let compare a b = (*TODO where is this used? Why not use the OCaml Set.compare function?*)
    match a, b with
    | Simple i, Simple j -> Id.compare i j
    | Simple _, Compound _ -> -1
    | Compound _, Simple _ -> 1
    | Compound s1, Compound s2 -> IdSet.compare s1 s2
end

module NodeSet = Set.Make(Node)
module NodeMap = Map.Make(Node)

(*TODO move to Hypergraph?*)
type node_data = {
  weight : int option; (* rank(/length) of the minimal path from the source (="true" node) to the current node. None if the node is unreachable. *)
  min_pred : Node.t option; (* Previous node in the minimal path from the source. None if the node is compound or if the node is unreachable. *)
  succs : NodeSet.t; (* outgoing edges *)
  preds : NodeSet.t; (* incoming edges *)
  is_compound : bool;
}

let make_node_data ?(weight=None) ?(min_pred=None) ?(succs=NodeSet.empty) ?(preds=NodeSet.empty) is_compound =
  { weight; min_pred; succs; preds; is_compound }

(* Priority queue 
Priority None represents infinity 
*)
module PQ = struct
  module IntMap = Map.Make(Int)
  type t = NodeSet.t IntMap.t * NodeSet.t

  let empty = IntMap.empty, NodeSet.empty
  let is_empty (map, set) = IntMap.is_empty map && NodeSet.is_empty set

  let add (map, set) ~prio node =
    match prio with 
    | None -> (map, NodeSet.add node set)
    | Some prio ->
    let set_prio =
      match IntMap.find_opt prio map with
      | None -> NodeSet.singleton node
      | Some s -> NodeSet.add node s
    in
    IntMap.add prio set_prio map, set

  let remove_min_opt (map, set) =
    match IntMap.min_binding_opt map with
    | None -> (
          match NodeSet.choose_opt set with
          | None -> None
          | Some node ->
          let set = NodeSet.remove node set in
          Some (None, node, (map, set))
    )
    | Some (prio, set) ->
      let node = NodeSet.min_elt set in
      let set' = NodeSet.remove node set in
      let map =
        if NodeSet.is_empty set' then IntMap.remove prio map
        else IntMap.add prio set' map
      in
      Some (Some prio, node, (map, set))
end

