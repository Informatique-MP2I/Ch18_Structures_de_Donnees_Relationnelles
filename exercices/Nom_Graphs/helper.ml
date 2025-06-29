open Graph
open Graphics
    
let printf format =
  Printf.kfprintf (fun out_channel -> flush out_channel) stdout format

let print_graph_array_list l = 
  Printf.printf   "┌─────┐\n" ;
  Array.iteri (fun i adj_list ->
    Printf.printf "│ %3d │" i;List.iter (fun adj -> Printf.printf " → %d" adj ) adj_list; Printf.printf "\n"
  ) l ;
  Printf.printf   "└─────┘\n"

let print_graph_matrix m = 
  Printf.printf   "┌───────┬" ; Array.iter  (fun _   -> Printf.printf "───")   m  ; Printf.printf "┐\n";
  Printf.printf   "│src\\dst│"; Array.iteri (fun i v -> Printf.printf "%2d " i) m ; Printf.printf "│\n";
  Printf.printf   "├───────┼" ; Array.iter  (fun _   -> Printf.printf "───")   m  ; Printf.printf "┤\n" ;
  Array.iteri (fun i row -> 
    Printf.printf "│   %3d │" i; Array.iter (fun cell -> Printf.printf " %s " (if cell then "✓" else " " ) ) row ; Printf.printf "│\n"
  ) m ;
  Printf.printf   "└───────┴" ; Array.iter (fun _ -> Printf.printf "───") m ; Printf.printf "┘\n"

let vertex_radius = 0.08 

(* Main function to create positions for vertices of a given graph g *)
let create_positions g =
  (* Function to calculate the score (number of edge intersections) for a given position set for g *)
  let score positions =
    (* Function to calculate the intersection point of two line segments adjusted by vertex_radius, if it exists *)
    let intersect (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
      (* Function to check if a segment intersects with a circle *)
      let segment_intersects_circle (cx, cy) radius (x1, y1) (x2, y2) =
        let dx = x2 -. x1 in
        let dy = y2 -. y1 in
        let fx = x1 -. cx in
        let fy = y1 -. cy in
        let a = dx *. dx +. dy *. dy in
        let b = 2.0 *. (fx *. dx +. fy *. dy) in
        let c = (fx *. fx +. fy *. fy) -. (radius *. radius) in
        let discriminant = b *. b -. 4.0 *. a *. c in
        if discriminant >= 0.0 then
          let discriminant_sqrt = sqrt discriminant in
          let t1 = (-.b -. discriminant_sqrt) /. (2.0 *. a) in
          let t2 = (-.b +. discriminant_sqrt) /. (2.0 *. a) in
          (t1 >= 0.0 && t1 <= 1.0) || (t2 >= 0.0 && t2 <= 1.0)
        else
          false
      in
      (* Check intersection with vertex circles first *)
      if segment_intersects_circle (x3, y3) vertex_radius (x1, y1) (x2, y2) then Some (x3, y3)
      else if segment_intersects_circle (x4, y4) vertex_radius (x1, y1) (x2, y2) then Some (x4, y4)
      else 
        (* Calculate intersection point of two line segments *)
        let denominator = (y4 -. y3) *. (x2 -. x1) -. (x4 -. x3) *. (y2 -. y1) in
        if abs_float denominator < 1e-6 then None
        else
          let ua = ((x4 -. x3) *. (y1 -. y3) -. (y4 -. y3) *. (x1 -. x3)) /. denominator in
          let ub = ((x2 -. x1) *. (y1 -. y3) -. (y2 -. y1) *. (x1 -. x3)) /. denominator in
          let segment_length_1 = sqrt ((x2 -. x1) ** 2.0 +. (y2 -. y1) ** 2.0) in
          let segment_length_2 = sqrt ((x4 -. x3) ** 2.0 +. (y4 -. y3) ** 2.0) in
          let vr = vertex_radius /. 1. in
          let ua_min = vr /. segment_length_1 in
          let ub_min = vr /. segment_length_2 in
          if ua >= ua_min && ua <= 1.0 && ub >= ub_min && ub <= 1.0 then
            let x = x1 +. ua *. (x2 -. x1) in
            let y = y1 +. ua *. (y2 -. y1) in
            Some (x, y)
          else
            None
    in
    let rec num_intersections i j acc =
      if i < 0 then acc
      else if j < 0 then num_intersections (i - 1) (g.nb_vertices - 1) acc
      else if i = j then num_intersections i (j - 1) acc
      else
        let adj_i = g.get_adj_list i in
        let adj_j = g.get_adj_list j in
        let new_intersections =
          List.fold_left (fun acc vi ->
            List.fold_left (fun acc vj ->
              if vi <> vj && i <> vj && j <> vi then
                if (not g.directed) && vi>vj || g.directed then
                  match intersect positions.(i) positions.(vi) positions.(j) positions.(vj) with
                  | Some _ -> acc + 1
                  | None -> acc
                else acc
              else acc
            ) acc adj_j
          ) acc adj_i
        in
        num_intersections i (j - 1) new_intersections
    in
      num_intersections (g.nb_vertices - 1) (g.nb_vertices - 1) 0
  in
  (* Function to generate all combinations of s elements from a list *)
  let rec combinations s lst =
    if s = 0 then [[]]
    else match lst with
    | [] -> []
    | hd :: tl ->
      let with_hd = List.map (fun l -> hd :: l) (combinations (s - 1) tl) in
      let without_hd = combinations s tl in
      with_hd @ without_hd
  in
  (* Function to generate all permutations of a list *)
  let rec permutations lst =
    let rec insert_all_positions x lst =
      match lst with
      | [] -> [[x]]
      | hd :: tl -> (x :: lst) :: List.map (fun l -> hd :: l) (insert_all_positions x tl)
    in
    match lst with
    | [] -> [[]]
    | hd :: tl -> List.flatten (List.map (insert_all_positions hd) (permutations tl))
  in
  (* function to compute distance between (x1,y1) and (x2,y2) *)
  let distance (x1, y1) (x2, y2) = sqrt ((x2 -. x1) ** 2.0 +. (y2 -. y1) ** 2.0) in
  (* Function to calculate the forces on a vertex v *)
  let forces_on_vertex v positions =
    let d1 = 0.78 in (* ditance of a standard zzz *)
    let d1' = 0.8 in (* distance of a standard edge *)
    let d3 = 0.08 in (* d3 quantify the intensity of the attraction/repulsion force *) 
    let normalize (x, y) =
      let mag = sqrt (x *. x +. y *. y) in
      if mag = 0.0 then (0.0, 0.0) else (x /. mag, y /. mag)
    in
    let scale_vector (x, y) scale = (x *. scale, y *. scale) in
    let vector (x1, y1) (x2, y2) = (x2 -. x1, y2 -. y1) in
    let rec aux w_lst acc =
      match w_lst with
      | [] -> acc
      | w :: tl ->
        if w = v then aux tl acc
        else
          let pos_v = positions.(v) in
          let pos_w = positions.(w) in
          let d = distance pos_v pos_w in
          let vec = vector pos_v pos_w in
          let normalized_vec = normalize vec in
          let result =
            if d < d1 then
              let scale = d3 *. (d1 -. d) /. d1 in
              let force = scale_vector normalized_vec (-.scale) in
              force :: acc
            else if d > d1' && (g.is_edge (v, w) || g.is_edge (w, v)) then
              let scale = d3 *. (d -. d1') /. d1' in
              let force = scale_vector normalized_vec scale in
              force :: acc
            else acc
          in
          aux tl result
    in
  aux (List.init g.nb_vertices (fun x -> x)) []
  in
  (* Function to apply forces to the positions *)
  let apply_forces positions =
    let new_positions = Array.copy positions in
    let apply_force v (fx, fy) =
      let original_pos = new_positions.(v) in
      let new_pos = (fst original_pos +. fx, snd original_pos +. fy) in
      new_positions.(v) <- new_pos;
      let original_score = score positions in
      let new_score = score new_positions in
      if new_score <= original_score then ()  (* Keep the new position *)
      else new_positions.(v) <- original_pos  (* Revert to the original position *)
    in
    Array.iteri (fun v _ ->
      let forces = forces_on_vertex v positions in
      List.iter (apply_force v) forces
    ) positions;
    new_positions
  in
  assert (g.nb_vertices <= 9);
  let nb_dots = [|2;2;2;2;3;3;3;3;3;3|].(g.nb_vertices) in 
  (* Generate the set of possible positions on a 5x5 grid *)
  let positions_set = 
    let vl = List.init nb_dots (fun i -> -1. +. 2. *. (float_of_int i) /. (float_of_int (nb_dots - 1))) in
    List.concat (List.map (fun v -> List.map (fun w -> if v=0. then (0.7*.w,0.1*.w) else if w=0. then (0.1*.v,0.7*.v) else (w,v)) vl) vl)
  in
  (* Generate all combinations of positions for the number of vertices *)
  let possible_positions = combinations g.nb_vertices positions_set in
  (* Function to evaluate permutations of a given position combination and update the best score *)
  let evaluate_combination (best_score, best_pos_list) pos_comb =
    let perms = permutations pos_comb in
    List.fold_left (fun (current_best_score, current_best_pos_list) perm ->
      if current_best_score<=0 then
        (current_best_score, current_best_pos_list)
      else
        let perm_array = Array.of_list perm in
        let current_score = score perm_array in
        if current_score < current_best_score then
          (current_score, perm)
        else
          (current_best_score, current_best_pos_list)
    ) (best_score, best_pos_list) perms
  in
  (* Fold over all possible combinations to find the best one *)
  let (final_best_score, final_best_pos_list) =
    List.fold_left evaluate_combination (max_int, []) possible_positions
  in
  let optimize_positions p =
    Printf.printf "score : %d\n" (score p) ; 
    let rec aux positions max_iter = 
      if max_iter<=0 then
        positions
      else
        let new_positions = apply_forces positions in
        let total_mvt =
          let rec sum_mvt t = 
            if t>=Array.length positions then 0.0
            else ( distance positions.(t) new_positions.(t) ) +. ( sum_mvt (t+1) )
          in
          sum_mvt 0
        in
        if total_mvt < 0.001 then new_positions
        else aux new_positions (max_iter - 1)
    in
    aux p 1000
  in
  let scale_positions positions =
    let x_bottomleft, y_bottomleft = -0.9, -0.9 in
    let x_topright, y_topright = 0.9, 0.9 in
    (* Trouver min_x, min_y, max_x, max_y *)
    let min_x, min_y, max_x, max_y =
      Array.fold_left (fun (min_x, min_y, max_x, max_y) (x, y) ->
        (min min_x x, min min_y y, max max_x x, max max_y y)
      ) (max_float, max_float, min_float, min_float) positions
    in
    (* Calculer les dimensions et les facteurs d'échelle *)
    let width = max_x -. min_x in
    let height = max_y -. min_y in
    let target_width = x_topright -. x_bottomleft in
    let target_height = y_topright -. y_bottomleft in
    let scale = min (target_width /. width) (target_height /. height) in
    (* Calculer les décalages pour centrer le tout *)
    let x_offset = x_bottomleft -. (min_x *. scale) +. (target_width -. (width *. scale)) /. 2.0 in
    let y_offset = y_bottomleft -. (min_y *. scale) +. (target_height -. (height *. scale)) /. 2.0 in
    (* Appliquer le changement d'échelle et de décalage *)
    Array.map (fun (x, y) ->
      let new_x = x *. scale +. x_offset in
      let new_y = y *. scale +. y_offset in
      (new_x, new_y)
    ) positions
  in
  (* Convert the best position list to an array and return *)
  scale_positions (optimize_positions (Array.of_list final_best_pos_list))

(* Function to display the graph in an X11 window *)
let display_x11 g grp lst draw_links deep_first =
  let positions = create_positions g in
  let vertices = List.init g.nb_vertices (fun x -> x) in
  let nodes = List.map (fun v -> "s" ^ string_of_int v) vertices in 
  let edges = 
    let get_edges_from_vertex v =
      List.fold_left (fun acc h ->
        if g.is_edge (v, h) then (v, h) :: acc else acc
      ) [] (g.get_adj_list v) in
    List.fold_left (fun acc v ->
      let edges_from_v = get_edges_from_vertex v in
      edges_from_v @ acc
    ) [] vertices in
  let width = 800 in
  let height = 800 in
  (* Function to convert normalized coordinates to pixel coordinates *)
  let normalize_position (x, y) =
    let w0 = float_of_int width *. 0.1 in
    let h0 = float_of_int height *. 0.1 in
    let w1 = float_of_int width *. 0.9 in
    let h1 = float_of_int height *. 0.9 in
    let pixel_x = int_of_float (w0 *. 0.5 +. (w1 +. x *. w1) ) / 2 in
    let pixel_y = int_of_float (h0 *. 0.5 +. (h1 +. y *. h1) ) / 2 in
    (pixel_x + 15, pixel_y + 15)
  in
  (* Function to draw an arrow *)
  let draw_arrow (src_x, src_y) (dst_x, dst_y) = 
    let arrow_length = 25.0 in
    let angle = atan2 (float_of_int (dst_y - src_y)) (float_of_int (dst_x - src_x)) in
    let src_x' = int_of_float (float_of_int src_x +. arrow_length *. cos angle) in
    let src_y' = int_of_float (float_of_int src_y +. arrow_length *. sin angle) in
    let dst_x' = int_of_float (float_of_int dst_x -. arrow_length *. cos angle) in
    let dst_y' = int_of_float (float_of_int dst_y -. arrow_length *. sin angle) in
    moveto src_x' src_y';
    lineto dst_x' dst_y';
    if (g.directed) then 
    begin
      let arrow_angle = 30.0 *. (Float.pi /. 180.0) in
      let arrow_point1 = 
        let x = float_of_int dst_x' -. arrow_length *. cos (angle -. arrow_angle) in
        let y = float_of_int dst_y' -. arrow_length *. sin (angle -. arrow_angle) in
        (int_of_float x, int_of_float y) in
      let arrow_point2 = 
        let x = float_of_int dst_x' -. arrow_length *. cos (angle +. arrow_angle) in
        let y = float_of_int dst_y' -. arrow_length *. sin (angle +. arrow_angle) in
        (int_of_float x, int_of_float y) in
      lineto (fst arrow_point1) (snd arrow_point1);
      moveto dst_x' dst_y';
      lineto (fst arrow_point2) (snd arrow_point2)
    end
  in
  (* Function to draw a cross at the given coordinates *)
  (*
  let draw_cross (x, y) =
    let size = 10 in
    set_color green;
    for t=0 to 2 do
    let i=t-1 in
    moveto (x - size + i) (y - size);
    lineto (x + size + i) (y + size);
    moveto (x - size + i) (y + size);
    lineto (x + size + i) (y - size)
    done
  in
  *)
  (* Open the graphics window *)
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Graph Visualization";
  (* Clear the window *)
  clear_graph ();
  (* Draw the edges *)
  List.iter (fun (src, dst) ->
    let (src_x, src_y) = normalize_position (positions.(src)) in
    let (dst_x, dst_y) = normalize_position (positions.(dst)) in
    set_color red;
    draw_arrow (src_x, src_y) (dst_x, dst_y);
  ) edges;
  (* Draw the nodes *)
  List.iteri (fun i node ->
    let (x, y) = normalize_position (positions.(i)) in
    set_color black;
    fill_circle x y 25;
    set_color white;
    let text_size = 6 * String.length node in
    moveto (x - text_size / 2) (y - 5); (* Adjust the position to center the text *)
    draw_string node;
  ) nodes;
  (* Draw blue numbers at the top right of the circles of the specified vertices *)
  List.iteri (fun i v ->
    let (x, y) = normalize_position (positions.(v)) in
    let offset_x = 25 in
    let offset_y = 25 in
    let text = string_of_int (i + 1) in
    set_color (rgb 0 192 0);
    moveto (x + offset_x) (y + offset_y);
    draw_string text;
  ) lst;
  (* Function to draw a link vector with an adjusted for the arrow length *)
  let draw_link (xa,ya) (xb,yb) =
    let (x1,y1) = (float_of_int xa, float_of_int ya) in 
    let (x2,y2) = (float_of_int xb, float_of_int yb) in 
    let length = 80.0 in 
    (* Function to calculate the midpoint of a segment and adjust for the arrow length *)
    let (start_pos, end_pos) =
      let mx = (x1 +. x2) /. 2.0 in
      let my = (y1 +. y2) /. 2.0 in
      let dx = x2 -. x1 in
      let dy = y2 -. y1 in
      let distance = sqrt (dx *. dx +. dy *. dy) in
      let ratio = length /. distance in
      let offset_x = ratio *. dx /. 2.0 in
      let offset_y = ratio *. dy /. 2.0 in
      ((mx -. offset_x, my -. offset_y), (mx +. offset_x, my +. offset_y))
    in
    (* Function to draw a force vector as an arrow *)
    let draw_force (x1, y1) (x2, y2) =
      let angle = atan2 (y2 -. y1) (x2 -. x1) in
      let arrow_length = 16.0 in
      let arrow_angle = 30.0 *. (Float.pi /. 180.0) in
      let arrow_point1 =
        let x = x2 -. arrow_length *. cos (angle -. arrow_angle) in
        let y = y2 -. arrow_length *. sin (angle -. arrow_angle) in
        (x, y) in
      let arrow_point2 =
        let x = x2 -. arrow_length *. cos (angle +. arrow_angle) in
        let y = y2 -. arrow_length *. sin (angle +. arrow_angle) in
        (x, y) in
      set_color (rgb 255 192 0);
      let ix1 = int_of_float x1 in
      let iy1 = int_of_float y1 in
      let ix2 = int_of_float x2 in
      let iy2 = int_of_float y2 in
      let ix3 = int_of_float (fst arrow_point1) in
      let iy3 = int_of_float (snd arrow_point1) in
      let ix4 = int_of_float (fst arrow_point2) in
      let iy4 = int_of_float (snd arrow_point2) in 
      List.iter (fun (sx,sy) ->
      moveto (ix1+sx) (iy1+sy) ;
      lineto (ix2+sx) (iy2+sy) ;
      moveto (ix2+sx) (iy2+sy) ;
      lineto (ix3+sx) (iy3+sy) ;
      moveto (ix2+sx) (iy2+sy) ;
      lineto (ix4+sx) (iy4+sy)
      ) [(-1,-1);(0,0);(-1,1)] 
    in  
    draw_force start_pos end_pos
  in
  if draw_links then
  (* Draw link vectors between the specified vertices *)
  begin
    List.iteri (fun i u ->
      let start_j, end_j, step =
        if deep_first then (i-1,-1,-1) else (0, i, 1)
      in
      let rec iter_j j =
        if j <> end_j then
          let v = List.nth lst j in
          if g.is_edge (v, u) then 
            let src = normalize_position (positions.(v)) in
            let dst = normalize_position (positions.(u)) in
            draw_link src dst
          else 
            iter_j (j + step)
      in
      iter_j start_j
    ) lst ;
  end ; 
  (* Draw group indices in violet at the bottom right of the circles of the vertices *)
  let vertex_groups = Array.make (Array.length positions) "" in
  List.iteri (fun grp_idx grp_list ->
    List.iter (fun vertex ->
      let existing = vertex_groups.(vertex) in
      let new_entry = Printf.sprintf "g%d" grp_idx in
      vertex_groups.(vertex) <- if existing = "" then new_entry else existing ^ ";" ^ new_entry
    ) grp_list
  ) grp;
  Array.iteri (fun vertex groups_str ->
    if groups_str <> "" then (
      let (x, y) = normalize_position (positions.(vertex)) in
      let offset_x = 25 in
      let offset_y = -25 in
      set_color (rgb 192 0 255);
      moveto (x + offset_x) (y + offset_y);
      draw_string groups_str;
    )
  ) vertex_groups ;
  (* Wait for a key press *)
  ignore (read_key ());
  (* Close the graphics window *)
  close_graph ()
