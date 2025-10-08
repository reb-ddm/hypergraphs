
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

  let compare a b = (*TODO where is this used?*)
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

(**
Priority queue 
None represents the value infinity.
Source = https://lewinb.net/posts/22_priorityqueue_ocaml/
*)
module PriorityQueue (Elt : Set.OrderedType) = struct
  module EltWithPrio = struct
  type t = int option * Elt.t
  let compare (a,e1) (b,e2) = 
    match a, b with 
    | None, _ -> 1
    | _, None -> -1
    | Some x, Some y -> 
      let c = Int.compare x y in
      if c = 0 then Elt.compare e1 e2 else c
end
  module IntOptionSet = Set.Make(EltWithPrio)

  (* The priority queue (heap) functionality now basically comes for free. *)
  let empty = IntOptionSet.empty
  let is_empty = IntOptionSet.is_empty
  let add ~prio elt q = 
    IntOptionSet.add (prio, elt) q 

  let remove_min_opt q =
    match IntOptionSet.min_elt_opt q with
    | None -> None
    | Some (prio, elt) ->
    Some (prio, elt, IntOptionSet.remove (prio, elt) q)

  let decrease_key elt ~prio:new_prio q = (* TODO this is not very efficient *)
    match IntOptionSet.find_first_opt (fun (_, q_elt) -> Elt.compare q_elt elt = 0) q with
    | None -> q
    | Some old_elt ->
    q |> IntOptionSet.remove old_elt |> add ~prio:new_prio elt

end

