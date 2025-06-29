(* Polymorphic type *)
(*
type 'v graph  = {
  directed     : bool;
  nb_vertices  : int;
  is_edge      : 'v * 'v -> bool;
  get_adj_list : 'v -> 'v list;
  print        : unit -> unit
}
*)

type graph  = {
  directed     : bool;
  nb_vertices  : int;
  is_edge      : int * int -> bool;
  get_adj_list : int -> int list;
  print        : unit -> unit
}

val create_dir_graph_m : int -> (int * int) list -> graph
val create_dir_graph_l : int -> (int * int) list -> graph
val create_undir_graph_m : int -> (int * int) list -> graph
val create_undir_graph_l : int -> (int * int) list -> graph

val breath_first_search : graph -> int -> int list
val depth_first_search : graph -> int -> int list
val topological_sort : graph -> int -> int list
val connected_components_search : graph -> int list list

