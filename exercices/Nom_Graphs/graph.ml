type graph = {
  directed : bool;
  nb_vertices : int;
  is_edge : int * int -> bool;
  get_adj_list : int -> int list;
  print : unit -> unit
}

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
                        else
                          if matrix.(v).(w) then
                            w::(aux (w+1))
                          else
                            aux (w+1)
                     in aux 0);
    print = (fun () -> Helper.print_graph_matrix matrix)
  }

let create_dir_graph_l n setE =
  let adj_list_array = Array.make n [] in
  List.iter (fun (src,dst) ->
      adj_list_array.(src) <- dst::adj_list_array.(src)) setE; 
  {
    directed = true;
    nb_vertices = n;
    is_edge = (fun (v,w) -> List.mem w adj_list_array.(v));
    get_adj_list = (fun v -> adj_list_array.(v));
    print = (fun () -> Helper.print_graph_array_list adj_list_array) 
  }

let create_undir_graph_m n setE =
  (* TODO *)
  {
    directed = false;
    nb_vertices = 0;
    is_edge = (fun (v,w) -> false);
    get_adj_list = (fun v -> []);
    print = (fun () -> Helper.print_graph_matrix [||]) 
  }

let create_undir_graph_l n setE =
  (* TODO *)
  {
    directed = false;
    nb_vertices = 0;
    is_edge = (fun (v,w) -> false);
    get_adj_list = (fun v -> []);
    print = (fun () -> Helper.print_graph_array_list [||]) 
  }


(* BFS - Breath-First Search *)
let breath_first_search graph root =
  let marked = Array.make graph.nb_vertices false in
  let q = Queue.create() in
  marked.(root)<-true;
  Queue.add root q;
  let rec aux visited =
    if not (Queue.is_empty q) then
      let v = Queue.pop q in
      let l = graph.get_adj_list v in
      List.iter (fun w -> if not marked.(w) then
                    begin
                      marked.(w)<-true;
                      Queue.add w q
                    end) l;
      aux (v::visited)
    else
      visited;
  in
  List.rev (aux [])



(* DFS - Depth-First Search *)
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






(* Topological sort for directed acyclic graphs *)
let topological_sort graph root = 
  []

(* Connected components search in an undirected graph *)
let connected_components_search graph =
  []

  
