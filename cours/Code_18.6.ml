let depth_first_search graph root =
  let marked = Array.make graph.nb_vertices false in
  let rec aux curr_vertex acc =
    if marked.(curr_vertex) then
      acc
    else
      begin
        marked.(curr_vertex) <- true;
        List.fold_left (fun f_acc v -> aux v f_acc)
          (acc@[curr_vertex])
          (graph.get_adj_list curr_vertex)
      end
  in
  aux root []  
