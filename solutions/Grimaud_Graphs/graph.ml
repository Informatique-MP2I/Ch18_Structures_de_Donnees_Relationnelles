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
  let matrix = Array.make_matrix n n false in
  List.iter (fun (src,dst) ->
      matrix.(src).(dst) <- true;
      matrix.(dst).(src) <- true) setE;
  {
    directed = false;
    nb_vertices = n;
    is_edge = (fun (v,w) -> matrix.(v).(w));
    get_adj_list = (fun v -> let rec aux w =  
                               if w=n then
                                 []
                               else
                               if matrix.(v).(w) then
                                 w::(aux (w+1))
                               else
                                 aux (w+1)
                     in aux 0);
    print = (fun () -> Helper.print_graph_matrix matrix) 
  }

let create_undir_graph_l n setE =
  let adj_list_array = Array.make n [] in
  List.iter (fun (src,dst) ->
    if not (List.mem dst adj_list_array.(src)) then
      adj_list_array.(src) <- dst::adj_list_array.(src);
    if not (List.mem src adj_list_array.(dst)) then
      adj_list_array.(dst) <- src::adj_list_array.(dst)
  ) setE; 
  {
    directed = false;
    nb_vertices = n;
    is_edge = (fun (v,w) -> List.mem w adj_list_array.(v));
    get_adj_list = (fun v -> adj_list_array.(v));
    print = (fun () -> Helper.print_graph_array_list adj_list_array) 
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
  let marked = Array.make graph.nb_vertices false in
  let stack = Stack.create() in 
  let rec dfs curr_vertex =
    if not marked.(curr_vertex) then
      begin
        marked.(curr_vertex) <- true;
        List.iter dfs (graph.get_adj_list curr_vertex);
        Stack.push curr_vertex stack
      end
  in
  let rec visit_all curr_vertex =
    if curr_vertex < graph.nb_vertices then
      begin
        if not marked.(curr_vertex) then dfs curr_vertex;
        visit_all (curr_vertex + 1)
      end
  in
  (* Start with the root *)
  dfs root;
  (* Ensure all vertices are visited *)
  visit_all 0; 
  (* Collect all elements from the stack to form the topological sort order *)
  let rec collect acc =
    if Stack.is_empty stack then
      acc
    else
      collect (acc@[Stack.pop stack])
  in
  collect []


  







(* Connected components search in an undirected graph *)
let connected_components_search graph =
  let marked = Array.make graph.nb_vertices false in
  let rec aux curr_vertex acc = 
    if curr_vertex = graph.nb_vertices then
      acc
    else
    if not marked.(curr_vertex) then
      let res = depth_first_search graph curr_vertex in
      List.iter (fun v -> marked.(v)<-true) res;
      aux (curr_vertex+1) (res::acc)
    else
      aux (curr_vertex+1) acc
  in
  aux 0 []

  


(* Indicates if the graph is bipartite or not. *)
let is_bipartite graph =
  let color = Array.make graph.nb_vertices (-1) in
  let rec dfs vertex c =
    if color.(vertex) = -1 then
      begin
        color.(vertex) <- c;
        List.for_all (fun v -> dfs v (1 - c)) (graph.get_adj_list vertex)
      end
    else
      color.(vertex) = c
  in
  let rec check_all_vertices v =
    if v >= graph.nb_vertices then
      true
    else if color.(v) = -1 then
      if dfs v 0 then
        check_all_vertices (v + 1)
      else
        false
    else
      check_all_vertices (v + 1)
  in
  check_all_vertices 0
