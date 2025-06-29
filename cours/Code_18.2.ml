type graph = {
  directed     : bool;  
  nb_vertices  : int;
  is_edge      : int * int -> bool;
  get_adj_list : int -> int list
}
