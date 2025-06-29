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
