type 'v graph = {
  directed     : bool;  
  nb_vertices  : int;
  is_edge      : 'v * 'v -> bool;
  get_adj_list : 'v -> 'v list
}
