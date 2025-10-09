
module Id = struct
  type t = int
  let compare = Stdlib.compare
end

module IdSet = Set.Make(Id)

module Node = struct
  type t =
    | True
    | Simple of Id.t
    | Compound of IdSet.t (*this represents the source set of an edge*)

  let is_true_node = function True -> true | _ -> false
  let is_simple = function Simple _ | True -> true | _ -> false
  let is_compound = function Simple _ -> false | _ -> true

  let compare a b =
    match a, b with
    | True, True -> 0
    | True, _ -> -1
    | _, True -> 1
    | Simple i, Simple j -> Id.compare i j
    | Simple _, Compound _ -> -1
    | Compound _, Simple _ -> 1
    | Compound s1, Compound s2 -> IdSet.compare s1 s2


  let simple_node_of_id id = Simple id
  let compound_node_of_intset s = Compound s

  let is_reflexive_edge tail head =
    match tail, head with
    | Compound xset, Simple x -> IdSet.mem x xset
    | _ -> false

  let fold f node acc =
    match node with 
    | True -> acc
    | Simple _ -> f node acc
    | Compound xset -> IdSet.fold 
                         (fun i acc -> let node = simple_node_of_id i in f node acc) xset acc

  let string_of_node node =
    match node with
    | True -> "true"
    | Simple id -> "s" ^ string_of_int id
    | Compound id_set -> 
      "c" ^ let s = string_of_int (Hashtbl.hash id_set) 
      in String.sub s 0 2 
  (*TODO give the compound nodes an actual ID*)


  let dot_of_node node int_to_string b =
    match node with
    | True ->
      let id = string_of_node node in 
      Buffer.add_string b
        (Printf.sprintf "  n%s [label=\"%s\", shape=plaintext];\n"
           id id)
    | Simple id ->
      let vname = int_to_string id in
      Buffer.add_string b
        (Printf.sprintf "  n%s [label=\"%s\", shape=plaintext];\n"
           (string_of_node node) vname)
    | Compound _ -> 
      let id = string_of_node node in 
      Buffer.add_string b
        (Printf.sprintf "  n%s [label=\"%s\", shape=circle, style=filled, fillcolor=white, color=black, width=0.2, fixedsize=true];\n"
           id id)

  let dot_of_edge node successor b =
    let edge_style =
      match node, successor with
      | Simple _, Compound _ -> " [style=dashed]"
      | _ -> ""
    in
    Buffer.add_string b
      (Printf.sprintf "  n%s -> n%s%s;\n" (string_of_node node) (string_of_node successor) edge_style)
end

module NodeSet = Set.Make(Node)
module NodeMap = Map.Make(Node)

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


module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
exception ExceededNrOfVars

module Vars = struct
  type var = string
  type var_to_int_map = int StringMap.t
  type int_to_var_map = string IntMap.t

  type t = {
    var_name_to_int : var_to_int_map;
    int_name_to_var : int_to_var_map;
    nr_vars: int;
    next_unused_int: int;
  }

  let init ~nr_vars= {
    var_name_to_int = StringMap.empty;
    int_name_to_var = IntMap.empty;
    nr_vars;
    next_unused_int = 0;
  }

  (*If the variable is new then it is added to the data structure and the function returns true.*)
  let var_to_int vars var =
    match StringMap.find_opt var vars.var_name_to_int with
    | Some i -> (vars, i, false)
    | None ->
      if vars.next_unused_int = vars.nr_vars then (*TODO this seems unnecessary*)
        raise ExceededNrOfVars
      else
        let i = vars.next_unused_int in
        ({
          vars with
          var_name_to_int = StringMap.add var i vars.var_name_to_int;
          int_name_to_var = IntMap.add i var vars.int_name_to_var;
          next_unused_int = vars.next_unused_int + 1;
        }, i, true)

  let int_to_var vars id =
    IntMap.find id vars.int_name_to_var


end

module type HornClauses = sig
  type t
  type edge = Node.t * Node.t
  val init : nr_vars:int -> t
  val add_node : Node.t -> t -> t
  val add_clause : edge -> t -> t
  val add_clauses : edge list -> t -> t
  val remove_clause : edge -> t -> t
  val is_reachable : Node.t -> t -> bool
  val to_dot : t -> (int -> string) -> string
end
