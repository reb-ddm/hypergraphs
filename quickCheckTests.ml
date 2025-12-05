open HcHandler
open Hypergraph_sig
open QCheck2

let gen_clause nr_vars =
  let open Gen in
  let+ lhs = int_bound (nr_vars - 1)
  and+ rhs = list (int_bound (nr_vars - 1)) in
  let rhs = List.filter (fun i -> i <> lhs) rhs in
  lhs, IdSet.of_seq (List.to_seq rhs)

type operations =
  | Add of (int * IdSet.t)
  | Remove of int
  | OpenChapter of int list
  | CloseChapter

let addnr = 0
let removenr = 2
let opennr = 1
let closenr = 3

let gen_operations nr_vars =
  (* First adds n clauses then removes some then adds some then removes some*)
  let open Gen in
  let choose_operation (nr_clauses, open_indexes) i =
    match i with
    | n when n = addnr ->
      let+ c = gen_clause nr_vars in
      Add c
    | n when n = removenr ->
      if nr_clauses >= 1 && not ([] = open_indexes) then
        let+ c = oneofl open_indexes in
        Remove c
      else
        let+ c = gen_clause nr_vars in
        Add c
    | n when n = opennr ->
      if nr_clauses >= 1 then
        let+ c = list (int_bound (nr_clauses - 1)) in
        OpenChapter c
      else
        let+ c = gen_clause nr_vars in
        Add c
    | n when n = closenr -> Gen.return CloseChapter
    | _ -> failwith "this should not happen"
  in
  join
    (join
       (let+ nr_clauses1 = map (fun i -> i + 1) (int_bound 40)
        and+ nr_clauses2 = map (fun i -> i + 1) (int_bound 40) in
        let+ open1 = list (int_bound (nr_clauses1 - 1))
        and+ open2 = list (int_bound (nr_clauses2 - 1)) in
        Gen.tup4 (return nr_clauses2) (return open2) (return nr_vars)
          (let+ addlist1 =
             Gen.list_size (return nr_clauses1) (choose_operation (0, []) addnr)
           and+ addlist2 =
             Gen.list_size (return nr_clauses2) (choose_operation (0, []) addnr)
           and+ remove_list1 =
             Gen.list_size (int_bound 40)
               (choose_operation (nr_clauses1, open1) removenr)
           and+ remove_list2 =
             Gen.list_size (int_bound 40)
               (choose_operation (nr_clauses2, open2) removenr)
           in
           [ CloseChapter ] @ addlist1 @ [ OpenChapter open1 ] @ remove_list1
           @ [ CloseChapter ] @ addlist2 @ [ OpenChapter open2 ] @ remove_list2)))

let clause_to_string (lhs, rhs) =
  "add " ^ string_of_int lhs ^ ":="
  ^ IdSet.fold (fun v s -> string_of_int v ^ "," ^ s) rhs ""

let print_operation = function
  | Add c -> clause_to_string c
  | Remove i -> "remove " ^ string_of_int i
  | OpenChapter il -> "open " ^ String.concat "," (List.map string_of_int il)
  | CloseChapter -> "close"

let print_operations os = String.concat "\n" (List.map print_operation os)

module HypergraphTestUtil (HC : FullyDynamicHornClauses) = struct
  module Handler = HornClauseHandler (HC)

  let clause_of_index hg i =
    match Handler.clause_of_index i hg with
    | None -> failwith "Wrong clause index"
    | Some c -> c

  let build_hypergraph (nr_vars, operations) =
    List.fold_left
      (fun hg op ->
        match op with
        | Add c -> Handler.add_clause c hg
        | OpenChapter l ->
          let l = List.map (clause_of_index hg) l in
          Handler.open_chapter hg l
        | CloseChapter -> Handler.close_chapter hg
        | Remove i ->
          let c = clause_of_index hg i in
          Handler.remove_clause hg c i)
      (Handler.init ~nr_vars) operations
end

module TestPropertiesHypergraph (HC : FullyDynamicHornClauses) = struct
  module TestHandler = HornClauseHandler (HC)
  module StringSet = Set.Make (String)
  module TestUtil = HypergraphTestUtil (HC)

  let to_set list = StringSet.of_seq (List.to_seq list)

  let add_reachable clause testHyp =
    let r_before = TestHandler.reachable_vars testHyp in
    let testHyp = TestHandler.add_clause clause testHyp in
    let r_after = TestHandler.reachable_vars testHyp in
    let c_lengths = List.compare_lengths r_before r_after in
    if c_lengths > 0 then
      false
    else if c_lengths = 0 then
      List.equal String.equal r_before r_after
    else
      StringSet.subset (to_set r_before) (to_set r_after)

  let test_add =
    Test.make ~name:"TEST ADD"
      ~print:(fun (clause, (_, _, _, operations)) ->
        clause_to_string clause ^ "\n" ^ print_operations operations)
      Gen.(
        (nat >|= fun i -> i + 1) >>= fun n ->
        pair (gen_clause n) (gen_operations n))
      (fun (clause, (_, _, nr_vars, operations)) ->
        let hg = TestUtil.build_hypergraph (nr_vars, operations) in
        add_reachable clause hg)

  let remove_reachable clause index testHyp =
    let r_before = TestHandler.reachable_vars testHyp in
    let testHyp = TestHandler.remove_clause testHyp clause index in
    let r_after = TestHandler.reachable_vars testHyp in
    let c_lengths = List.compare_lengths r_before r_after in
    if c_lengths < 0 then
      false
    else if c_lengths = 0 then
      List.equal String.equal r_before r_after
    else
      StringSet.subset (to_set r_after) (to_set r_before)

  let test_remove =
    Test.make ~name:"TEST REMOVE"
      ~print:(fun (i, _, operations) ->
        string_of_int i ^ "\n" ^ print_operations operations)
      Gen.(
        join
          ( nat >|= (fun i -> i + 1) >>= gen_operations
          >|= fun (_, open_ind, b, c) ->
            if open_ind = [] then
              return (-1, b, c)
            else
              let+ i = oneofl open_ind in
              i, b, c ))
      (fun (i, nr_vars, operations) ->
        if i = -1 then
          true
        else (
          let hg = TestUtil.build_hypergraph (nr_vars, operations) in
          let c = TestUtil.clause_of_index hg i in
          remove_reachable c i hg
        ))

  let open_reachable clauses testHyp =
    let r_before = TestHandler.reachable_vars testHyp in
    let testHyp = TestHandler.open_chapter testHyp clauses in
    let r_after = TestHandler.reachable_vars testHyp in
    List.equal String.equal r_before r_after

  let test_open =
    Test.make ~name:"TEST OPEN"
      ~print:(fun (i, _, operations) ->
        String.concat "," (List.map string_of_int i)
        ^ "\n"
        ^ print_operations operations)
      Gen.(
        join
          ( nat >|= (fun i -> i + 1) >>= gen_operations
          >|= fun (nr_clauses, _, b, c) ->
            if nr_clauses = 0 then
              return ([], b, c)
            else
              let+ open_ind = list (int_bound (nr_clauses - 1)) in
              open_ind, b, c ))
      (fun (i, nr_vars, operations) ->
        let hg = TestUtil.build_hypergraph (nr_vars, operations) in
        let c = List.map (TestUtil.clause_of_index hg) i in
        open_reachable c hg)

  let close_reachable testHyp =
    let r_before = TestHandler.reachable_vars testHyp in
    let testHyp = TestHandler.close_chapter testHyp in
    let r_after = TestHandler.reachable_vars testHyp in
    List.equal String.equal r_before r_after

  let test_close =
    Test.make ~name:"TEST CLOSE"
      ~print:(fun (_, _, _, operations) -> print_operations operations)
      Gen.(nat >|= (fun i -> i + 1) >>= gen_operations)
      (fun (_, _, nr_vars, operations) ->
        let hg = TestUtil.build_hypergraph (nr_vars, operations) in
        close_reachable hg)

  module ReferenceHandler = HornClauseHandler (IncrementalHypergraph)
  module ReferenceUtil = HypergraphTestUtil (IncrementalHypergraph)

  let first_reachable_correct incrHyp testHyp =
    Seq.equal String.equal
      (ReferenceHandler.first_reachable_vars incrHyp)
      (TestHandler.first_reachable_vars testHyp)

  let test_reference =
    Test.make ~name:"TEST REFERENCE"
      Gen.(nat >|= (fun i -> i + 1) >>= gen_operations)
      (fun (_, _, nr_vars, operations) ->
        let hg = TestUtil.build_hypergraph (nr_vars, operations) in
        let ref_hg = ReferenceUtil.build_hypergraph (nr_vars, operations) in
        first_reachable_correct ref_hg hg)

  let all_tests =
    [ test_close; test_add; test_remove; test_open; test_reference ]
end

module TestIncremental = TestPropertiesHypergraph (IncrementalHypergraph)

let _tests_incremental = TestIncremental.all_tests

module TestIncremental2 = TestPropertiesHypergraph (IncrementalHypergraph2)
module TestParametric = TestPropertiesHypergraph (SemiParametricHypergraph)
module TestMST = TestPropertiesHypergraph (FullyDynamicHypergraph)

let tests_parameteric = TestParametric.all_tests
let _tests_mst = TestMST.all_tests
let tests_incremental2 = TestIncremental2.all_tests
let _ = QCheck_runner.run_tests_main (tests_parameteric @ tests_incremental2)

(* let _ =
   Gen.generate ~n:1 (gen_operations 7)
   |> List.map (fun (_,_,_,a) ->a)
   |> List.map print_operations
   |> List.iter print_endline *)
