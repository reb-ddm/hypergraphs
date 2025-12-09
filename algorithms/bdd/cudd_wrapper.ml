open Cudd

type key = int
type mvbdu = Bdd.dt
type t = Bool of bool | Mvbdu of mvbdu

let man_ref : Man.dt option ref = ref None
let btrue_ref : mvbdu option ref = ref None
let bfalse_ref : mvbdu option ref = ref None

let ensure_init () =
  match !man_ref with
  | Some _ -> ()
  | None ->
    let m = Man.make_d () in
    man_ref := Some m;
    btrue_ref := Some (Bdd.dtrue m);
    bfalse_ref := Some (Bdd.dfalse m)

let get_man () =
  ensure_init ();
  match !man_ref with
  | Some m -> m
  | None -> assert false

let ithvar (v : key) =
  let m = get_man () in
  Bdd.ithvar m v

let init () = ensure_init ()

let variables_list_of_mvbdu (m : mvbdu) : key list =
  let supp = Bdd.support m in
  Bdd.list_of_support supp

let print (x : t) : unit =
  match x with
  | Bool b -> Format.printf "Bool %b\n" b
  | Mvbdu m ->
    let printer fmt k = Format.fprintf fmt "%d" k in
    Bdd.print printer Format.std_formatter m;
    print_newline ()

let mvbdu_equal (a : t) (b : t) : bool =
  match a, b with
  | Bool x, Bool y -> x = y
  | Bool true, Mvbdu m | Mvbdu m, Bool true -> Bdd.is_true m
  | Bool false, Mvbdu m | Mvbdu m, Bool false -> Bdd.is_false m
  | Mvbdu m1, Mvbdu m2 -> Bdd.is_equal m1 m2

let mvbdu_or (a : t) (b : t) : t =
  match a, b with
  | Bool false, x | x, Bool false -> x
  | Bool true, _ | _, Bool true -> Bool true
  | Mvbdu m1, Mvbdu m2 -> Mvbdu (Bdd.dor m1 m2)

let mvbdu_and (a : t) (b : t) : t =
  match a, b with
  | Bool true, x | x, Bool true -> x
  | Bool false, _ | _, Bool false -> Bool false
  | Mvbdu m1, Mvbdu m2 -> Mvbdu (Bdd.dand m1 m2)

let mvbdu_true () = Bool true
let mvbdu_false () = Bool false

let btrue () =
  ensure_init ();
  match !btrue_ref with
  | Some b -> b
  | None -> assert false

let mvbdu_of_association_list (alist : (int * int) list) : t =
  ensure_init ();
  match alist with
  | [] -> Bool true
  | _ ->
    let cube =
      List.fold_left
        (fun acc (v, value) ->
          let var = ithvar v in
          let lit =
            match value with
            | 1 -> var
            | 0 -> Bdd.dnot var
            | _ -> failwith "mvbdu_of_association_list: value must be 0 or 1"
          in
          if Bdd.is_false acc then
            lit
          else
            Bdd.dand acc lit)
        (btrue ()) alist
    in
    Mvbdu cube

let mvbdu_project_abstract_away (x : t) (vars : key list) : t =
  match x with
  | Bool b -> Bool b
  | Mvbdu m ->
    if vars = [] then
      Mvbdu m
    else (
      let var_array = Array.of_list (List.map ithvar vars) in
      let varset = Bdd.vectorsupport var_array in
      Mvbdu (Bdd.exist varset m)
    )

let mvbdu_to_formula mvbdu =
  match mvbdu with
  | Bool true -> Logical_formulae.True
  | Bool false -> Logical_formulae.False
  | Mvbdu mvbdu ->
    let rec to_formula mvbdu =
      match Bdd.inspect mvbdu with
      | Bdd.Bool true -> Logical_formulae.True
      | Bdd.Bool false -> Logical_formulae.False
      | Bdd.Ite (var, branch_true, branch_false) ->
        let atom = Logical_formulae.P var in
        let output_true = to_formula branch_true in
        let output_false = to_formula branch_false in
        let imply_false =
          Logical_formulae.IMPLY (Logical_formulae.NOT atom, output_false)
        in
        let imply_true = Logical_formulae.IMPLY (atom, output_true) in
        Logical_formulae.AND (imply_false, imply_true)
    in
    Logical_formulae.simplify (to_formula mvbdu)
