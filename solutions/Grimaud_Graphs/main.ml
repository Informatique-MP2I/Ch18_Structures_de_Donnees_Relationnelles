let parse_adjacencies str =
  let edges = ref [] in
  let pairs = String.split_on_char ' ' str in
  List.iter (fun pair ->
    match String.split_on_char ':' pair with
    | [src; dsts] ->
      let src = int_of_string src in
      let dsts = String.split_on_char ',' dsts in
      List.iter (fun dst -> edges := (src, int_of_string dst) :: !edges) dsts
    | _ -> failwith "Invalid adjacency format"
  ) pairs;
  !edges

let print_help prog_name =
  Printf.printf "Usage: %s [-h] [-d] [-m] -v <num_vertices> -a <adjacencies> [-bfs <start>] [-dfs <start>] [-ts <start>] [-ccs] [-s]\n" prog_name;
  Printf.printf "Options:\n";
  Printf.printf "  -h, --help            Show help\n";
  Printf.printf "  -d, --directed        Specify that the graph is a directed graph (default: undirected graph)\n";
  Printf.printf "  -m, --matrix          Specify that the graph will be encoded as a matrix (default: adjacency list)\n";
  Printf.printf "  -v, --vertex          Specify the number of vertices\n";
  Printf.printf "  -a, --adjacencies     Specify the adjacency list in the format \"src:dst1,dst2 ...\"\n";
  Printf.printf "  -bfs                  Call breath-first search on the graph by specifiying the start vertex (default: 0)\n";
  Printf.printf "  -dfs                  Call depth-first search on the graph by specifying the start vertex (default: 0)\n";
  Printf.printf "  -ts                   Call topological sort on a directed graph by specifying the start vertex (default: 0)\n";
  Printf.printf "  -ccs                  Call connected components search on a undirected graphx\n";
  Printf.printf "  -s, --show            Display the result in a window\n";
  Printf.printf "\nExamples:\n";
  Printf.printf "  %s -v 3 -a \"0:1,2 1:2\"\n" prog_name;
  Printf.printf "  %s -v 3 -a \"0:1,2 1:2 2:0\"\n" prog_name;
  Printf.printf "  %s --vertex 4 --adjacencies \"0:1 1:2,3 3:0\"\n" prog_name ;
  Printf.printf "  %s -v 5 -a \"1:3,0 2:0 3:4 4:0,1,2\"\n" prog_name;
  Printf.printf "  %s -v 5 -a \"1:3,0 2:0 3:4 4:0,1,2\" -bfs 0\n" prog_name;
  Printf.printf "  %s -v 5 -d -a \"1:3,0 2:0 3:4 4:0,1,2\" -dfs 3\n" prog_name;
  Printf.printf "  %s -v 5 -d -a \"1:3,0 2:0 3:4 4:0,1,2\" -dfs 3 -s\n" prog_name;
  Printf.printf "  %s -v 5 -d -a \"1:3,0 2:1,0 4:3,2\" -ts 0\n" prog_name;
  Printf.printf "  %s -v 8 -a \"1:3,0 2:0 3:4 4:0,1,2 5:6,7\" -ccs\n" prog_name

let () =
  let num_vertices = ref 0 in
  let adjacencies = ref "" in
  let is_directed = ref false in
  let is_matrix = ref false in
  let is_bfs = ref false in
  let bfs_start = ref 0 in
  let is_dfs = ref false in
  let dfs_start = ref 0 in
  let is_ts = ref false in
  let ts_start = ref 0 in
  let is_ccs = ref false in
  let show_x11 = ref false in
  let set_bfs s =
    is_bfs := true;
    try
      let start = int_of_string s in
      if start < !num_vertices then
        bfs_start := start
      else
        bfs_start := 0
    with Failure _ ->
      failwith "Invalid start vertex. It should be an integer." in 
  let set_dfs s =
    is_dfs := true;
    try
      let start = int_of_string s in
      if start < !num_vertices then
        dfs_start := start
      else
        dfs_start := 0
    with Failure _ ->
      failwith "Invalid start vertex. It should be an integer." in
  let set_ts s =
    is_ts := true;
    try
      let start = int_of_string s in
      if start < !num_vertices then
        ts_start := start
      else
        ts_start := 0
    with Failure _ ->
      failwith "Invalid start vertex. It should be an integer." in
  let speclist = [
    ("-h", Arg.Unit (fun () -> print_help Sys.argv.(0); exit 0), "Show help");
    ("--help", Arg.Unit (fun () -> print_help Sys.argv.(0); exit 0), "Show help");
    ("-d", Arg.Set is_directed, "Specify that the graph is a directed graph (default: undirected graph)");
    ("--directed", Arg.Set is_directed, "Specify that the graph is a directed graph (default: undirected graph)");
    ("-m", Arg.Set is_matrix, "Specify that the graph will be encoded as a matrix (default: adjacency list)");
    ("--matrix", Arg.Set is_matrix, "Specify that the graph will be encoded as a matrix (default: adjacency list)");
    ("-v", Arg.Set_int num_vertices, "Specify the number of vertices");
    ("--vertex", Arg.Set_int num_vertices, "Specify the number of vertices");
    ("-a", Arg.Set_string adjacencies, "Specify the adjacency list in the format \"src:dst1,dst2 ...\"");
    ("--adjacencies", Arg.Set_string adjacencies, "Specify the adjacency list in the format \"src:dst1,dst2 ...\"");
    ("-bfs", Arg.String set_bfs, "Call breath-first search on the graph by specifiying the start vertex (default: 0)\n");
    ("-dfs", Arg.String set_dfs, "Call depth-first search on the graph by specifying the start vertex (default: 0)\n");
    ("-ts", Arg.String set_ts, "Call topological sort on a directed graph by specifying the start vertex (default: 0)");
    ("-ccs", Arg.Set is_ccs, "Call connected components search on a undirected graph");
    ("-s", Arg.Set show_x11, "Display the result in a window");
    ("--show", Arg.Set show_x11, "Display the result in a window")
  ] in
  Arg.parse speclist print_endline (Printf.sprintf "Usage: %s [-h] [-d] [-m] -v <num_vertices> -a <adjacencies>" Sys.argv.(0));
  if !num_vertices <= 0 then (
    Printf.eprintf "\n\x1b[31m/!\\ Number of vertices must be set greater than 0 with -v\n\x1b[0m\n";
    print_help Sys.argv.(0);
    exit 1
  );
  if !adjacencies = "" then (
    Printf.eprintf "\n\x1b[31m/!\\ Adjacency list must be specified with -a\n\x1b[0m\n";
    print_help Sys.argv.(0);
    exit 1
  );
  let edges = parse_adjacencies !adjacencies in
  let graph = if !is_matrix then
      if not !is_directed then
        begin
          Printf.printf "Undirected graph\n";
          Graph.create_undir_graph_m !num_vertices edges
        end
      else
        begin
          Printf.printf "Directed graph\n";
          Graph.create_dir_graph_m !num_vertices edges
        end
    else
    if not !is_directed then
      begin
        Printf.printf "Undirected graph\n";
        Graph.create_undir_graph_l !num_vertices edges
      end
    else
      begin
        Printf.printf "Directed graph\n";
        Graph.create_dir_graph_l !num_vertices edges
      end
  in
  if (!show_x11 && graph.nb_vertices>9) then (
    Printf.eprintf "\n\x1b[31m/!\\ x11 graphic display doesn't work with more than 9 vertices.\n\x1b[0m\n";
    print_help Sys.argv.(0);
    exit 1
  );
  graph.print ();
  let lst = 
    if !is_bfs then (
      let bfs = (Graph.breath_first_search graph !bfs_start) in
      Printf.printf "Breath-first search : ";
      List.iter (fun x-> Printf.printf "%d  " x) bfs ;
      Printf.printf "\n" ;
      bfs
    ) else if !is_dfs then (
      let dfs = (Graph.depth_first_search graph !dfs_start) in
      Printf.printf "Depth-first search : ";
      List.iter (fun x-> Printf.printf "%d  " x) dfs ;
      Printf.printf "\n" ;
      dfs
    ) else if (!is_ts && !is_directed) then (
      let ts = (Graph.topological_sort graph !ts_start) in 
      Printf.printf "Topological sort : ";
      List.iter (fun x-> Printf.printf "%d  " x) ts ; 
      Printf.printf "\n" ;
      ts
    ) else
      [] 
  in
  let grp = if (!is_ccs && (not !is_directed)) then (
    let ccs = (Graph.connected_components_search graph) in
    Printf.printf "Connected components search : ";
    List.iteri (fun i x ->
      List.iter (fun y -> Printf.printf "%d " y) x;
      if i <> (List.length ccs) - 1 then
        Printf.printf "- ";
      ) ccs;
      Printf.printf "\n" ;
      ccs
    ) else [ ]
  in
  if !show_x11 then
    Helper.display_x11 graph grp lst (! is_dfs || !is_bfs || !is_ts) ( !is_dfs )
  else
    ()

