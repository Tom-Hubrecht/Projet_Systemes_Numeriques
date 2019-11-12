open Netlist_ast
open Graph

exception Combinational_cycle

let add lr = function
  | Avar(x) -> if not (List.mem x !lr) then lr := x::!lr;
  | _ -> ()

let read_exp eq =
  let v = ref [] and r = ref [] in
  let rec aux_read = function
    | Earg(a) | Enot(a) -> add v a
    | Ebinop(_, a1, a2) -> add v a1; add v a2
    | Emux(m, a1, a2) -> add v m; add v a1; add v a2
    | Econcat(a1, a2) -> add v a1; add v a2
    | Eslice(_, _, a) -> add v a
    | Eselect(_, a) -> add v a
    | Ereg(x) -> add r (Avar x) (* Les registres ont des dépendances inverses *)
    | Erom(_, _, r_a) -> add v r_a
    | Eram(_, _, r_a, _, _, w_d) -> add v r_a
  in
  aux_read eq;
  !v, !r

let schedule p =
  let g = mk_graph () in
  List.iter (fun v -> Graph.add_node g v) p.p_inputs;
  List.iter (fun (x, _) -> Graph.add_node g x) p.p_eqs;
  let rec aux_sch = function
    | [] -> ()
    | (x, e)::q ->
      begin
        let var, reg = read_exp e in
        List.iter (fun v -> try Graph.add_edge g v x with
              Not_found -> (Graph.add_node g v;
                            Graph.add_edge g v x))
          var;
        List.iter (fun r -> try Graph.add_edge g x r with
              Not_found -> (Graph.add_node g r;
                            Graph.add_edge g x r))
          reg;
        aux_sch q;
      end
  in
  aux_sch p.p_eqs;
  try
    let s_l = Graph.topological g in
    let n_eqs = ref [] in
    let rec find_eq x = function
      | [] -> []
      | (a, e)::q when a = x ->
        n_eqs := (a, e)::!n_eqs;
        q
      | t::q -> t::(find_eq x q)
    in
    let eqs = ref p.p_eqs in
    List.iter (fun x -> eqs := find_eq x !eqs) s_l;
    { p with p_eqs = (List.rev !n_eqs)}
  with
  | Graph.Cycle -> raise Combinational_cycle
