let create_dir_graph_m n setE =
  let matrix = Array.make_matrix n n false in
  List.iter (fun (src,dst) -> matrix.(src).(dst) <- true) setE;
  {
    directed = true;
    nb_vertices = n;
    is_edge = (fun (v,w) -> matrix.(v).(w));
    get_adj_list = (fun v -> 
                      let rec aux w =  
                        if w=n then []
                        else if matrix.(v).(w) then w::(aux (w+1))
                             else aux (w+1)
                     in aux 0)
  }
