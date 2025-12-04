open HcHandler
open Hypergraph_sig
open QCheck2

let gen_clause nr_vars =
  let open Gen in
  let+ lhs = int_bound (nr_vars-1) 
  and+ rhs = list (int_bound (nr_vars-1)) in
  lhs, IdSet.of_seq (List.to_seq rhs)


type operations =
  | Add of (int * IdSet.t)
  | Remove of int
  | OpenChapter of int list
  | CloseChapter

let gen_operations nr_vars = (*TODO do not accept multiple equal clauses added*)
  let open Gen in
  let choose_operation (nr_clauses, open_indexes) i =
    match i with
    | 1 -> let+ c = gen_clause nr_vars in 
      Add c
    | 3 -> 
      if  nr_clauses >= 1 && not([] = open_indexes) then 
        let+ c = oneofl open_indexes in
        Remove c
      else 
        let+ c = gen_clause nr_vars in 
        Add c
    | 2 -> if nr_clauses >= 1 then 
        let+ c = list (int_bound (nr_clauses-1)) in
        OpenChapter c
      else 
        let+ c = gen_clause nr_vars in 
        Add c
    | 0 -> Gen.return CloseChapter
    | _ -> failwith "this should not happen"
  in
  let nr_clauses = ref 0 in
  let open_indexes = ref [] in
  Gen.pair (return nr_vars) (Gen.list (
      let+ o = Gen.int_bound 3 >>= choose_operation (!nr_clauses, !open_indexes) in
      let () = match o with 
        | Add _ -> nr_clauses := !nr_clauses + 1  
        | OpenChapter l -> open_indexes := l
        | CloseChapter -> open_indexes := []
        | Remove _ -> nr_clauses := !nr_clauses - 1
      in o))


(* module TestCompareHypergraphToReference (HC : FullyDynamicHornClauses) = struct
  module ReferenceHandler = HornClauseHandler (IncrementalHypergraph)

  module TestHandler = HornClauseHandler (HC)


  let check_property property (nr_vars, operations) =
    let clause_of_index hg i = 
      match ReferenceHandler.clause_of_index i hg with
      | None -> failwith "Wrong clause index"
      | Some c -> c in
    let hg, ref_hg = List.fold_left (fun (hg, ref_hg) op ->
        match op with
        | Add c -> TestHandler.add_clause c hg,
                   ReferenceHandler.add_clause c ref_hg
        | OpenChapter l -> 
          let l = List.map (clause_of_index ref_hg) l in
          TestHandler.open_chapter hg l,
          ReferenceHandler.open_chapter ref_hg l
        | CloseChapter -> TestHandler.close_chapter hg,
                          ReferenceHandler.close_chapter ref_hg
        | Remove i -> 
          let c = clause_of_index ref_hg i in
          TestHandler.remove_clause hg c i,
          ReferenceHandler.remove_clause ref_hg c i
      )
        (TestHandler.init ~nr_vars, ReferenceHandler.init ~nr_vars) operations
    in property ref_hg hg

  let first_reachable_correct incrHyp testHyp = 
    Seq.equal String.equal (ReferenceHandler.first_reachable_vars incrHyp)
      (TestHandler.first_reachable_vars testHyp)

end *)

module TestPropertiesHypergraph (HC : FullyDynamicHornClauses) = struct
  module TestHandler = HornClauseHandler (HC)
  module StringSet = Set.Make (String)

  let check_property property (nr_vars, operations) =
    let clause_of_index hg i = 
      match TestHandler.clause_of_index i hg with
      | None -> failwith "Wrong clause index"
      | Some c -> c in
    let hg = List.fold_left (fun hg op ->
        match op with
        | Add c -> TestHandler.add_clause c hg 
        | OpenChapter l -> 
          let l = List.map (clause_of_index hg) l in
          TestHandler.open_chapter hg l
        | CloseChapter -> TestHandler.close_chapter hg
        | Remove i -> 
          let c = clause_of_index hg i in
          TestHandler.remove_clause hg c i
      )
        (TestHandler.init ~nr_vars) operations
    in property hg

  let to_set list = 
    StringSet.of_seq (List.to_seq list)

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

  let open_reachable clauses testHyp = 
    let r_before = TestHandler.reachable_vars testHyp in 
    let testHyp = TestHandler.open_chapter testHyp clauses in
    let r_after = TestHandler.reachable_vars testHyp in 
    List.equal String.equal r_before r_after 

  let close_reachable testHyp = 
    let r_before = TestHandler.reachable_vars testHyp in 
    let testHyp = TestHandler.close_chapter testHyp in
    let r_after = TestHandler.reachable_vars testHyp in 
    List.equal String.equal r_before r_after

end

module TestIncremental = TestPropertiesHypergraph(IncrementalHypergraph)

let gen_operations = Gen.((nat >|= fun i -> i + 1) >>= gen_operations)

let test_close_incremental =
  Test.make gen_operations (TestIncremental.check_property TestIncremental.close_reachable)

(* let test_open_incremental = *)
  (*TODO*)

let _properties = [TestIncremental.close_reachable, TestIncremental.add_reachable, TestIncremental.remove_reachable, TestIncremental.open_reachable]

  let _ = QCheck_runner.run_tests_main [test_close_incremental]