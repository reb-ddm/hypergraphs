open Kappa_mvbdu
open Hypergraph_sig

module Edges_intbdu :
  Mvbdu_wrapper.Internalized_mvbdu
  with type key = Id.t
   and type value = int

include Hypergraph_sig.FullyDynamicHornClauses
