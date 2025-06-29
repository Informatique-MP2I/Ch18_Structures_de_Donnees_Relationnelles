let create_dir_graph_l n setE =
  let adj_list_array = Array.make n [] in
  List.iter (fun (src,dst) ->
      adj_list_array.(src) <- dst::adj_list_array.(src)) setE; 
  {
    directed = true;
    nb_vertices = n;
    is_edge = (fun (v,w) -> List.mem w adj_list_array.(v));
    get_adj_list = (fun v -> adj_list_array.(v))
  }
