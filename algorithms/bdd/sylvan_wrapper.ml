open Bdd_sig
open Sylvan_bindings

open Printf
module IntSet = Set.Make(struct type t = int let compare = compare end)

module SylvanBdd : Bdd = struct

  type mvbdu = mdd
  type key = int

  type t =
    | Bool of bool
    | Mvbdu of mvbdu

  let init () =
    let heap_nodes = 100_000_000 in
    let () = lace_start (Unsigned.UInt.of_int 0) (Unsigned.Size_t.of_int 0) in
    sylvan_set_limits (Unsigned.Size_t.of_int heap_nodes) 1 1;
    sylvan_init_package ();
    sylvan_init_ldd ();
    ()

  let mvbdu_true () = Mvbdu lddmc_true
  let mvbdu_false () = Mvbdu lddmc_false

  let print = function
    | Bool b -> printf "<Bool %b>\n" b
    | Mvbdu d -> lddmc_print d; print_newline ()

  let rec mvbdu_equal a b =
    match a, b with
    | Bool x, Bool y -> x = y
    | Bool x, Mvbdu d ->
      (* compare boolean const to a singleton leaf *)
      if x then lddmc_equal d lddmc_true
      else lddmc_equal d lddmc_false
    | Mvbdu d, Bool x -> mvbdu_equal (Bool x) (Mvbdu d)
    | Mvbdu x, Mvbdu y -> lddmc_equal x y

  let mvbdu_or a b =
    match a, b with
    | Bool true, _ | _, Bool true -> Bool true
    | Bool false, other -> other
    | other, Bool false -> other
    | Mvbdu x, Mvbdu y -> Mvbdu (lddmc_union x y)

  let mvbdu_and a b =
    match a, b with
    | Bool false, _ | _, Bool false -> Bool false
    | Bool true, other -> other
    | other, Bool true -> other
    | Mvbdu x, Mvbdu y -> Mvbdu (lddmc_intersect x y)

  (* TODO adapt for MDDs *)
  let is_terminal (n : mdd) : bool =
    lddmc_equal n lddmc_false ||
    lddmc_equal n lddmc_true

  let variables_list_of_mvbdu (m : mvbdu) : key list =
    let supp = mtbdd_support m in
    let seen = ref IntSet.empty in
    let stack = Stack.create () in
    if not (is_terminal supp) then Stack.push supp stack;
    while not (Stack.is_empty stack) do
      let nd = Stack.pop stack in
      if not (is_terminal nd) then (
        let v = Unsigned.UInt32.to_int (lddmc_getvalue nd) in
        seen := IntSet.add v !seen;
        let down = lddmc_getdown nd in
        let right = lddmc_getright nd in
        if not (is_terminal down) then Stack.push down stack;
        if not (is_terminal right) then Stack.push right stack;
      )
    done;
    IntSet.elements !seen

  let mvbdu_of_association_list _ = (*TODO*)
    mvbdu_false ()

  let mvbdu_project_abstract_away t _ = t (*TODO*)

  let mvbdu_to_formula _ = Logical_formulae.True (*TODO*)
end

(* Export the module *)
include SylvanBdd
