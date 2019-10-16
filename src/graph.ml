exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
  { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let rec print_list = function
[] -> print_newline ()
| e::l -> print_string e ; print_string " " ; print_list l

let has_cycle g =
  clear_marks g;
  let rec aux_vis = function
    | [] -> false
    | n::l ->
      begin
        n.n_mark <- InProgress;
        if (List.filter (fun n -> n.n_mark = InProgress) n.n_link_to != []) then
          true
        else
          (aux_vis n.n_link_to) || (n.n_mark <- Visited; aux_vis l)
      end
  in aux_vis g.g_nodes

let topological g =
  if has_cycle g then raise Cycle;
  let s_l = ref [] in
  clear_marks g;
  let rec aux_sort = function
    | [] -> ()
    | n::q ->
      begin
        aux_sort n.n_link_to;
        aux_sort q;
        if n.n_mark = NotVisited then s_l := n.n_label::!s_l;
        n.n_mark <- Visited;
      end
  in
  aux_sort (find_roots g);
  !s_l

