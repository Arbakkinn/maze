#load "graphics.cma"
type status = Input | Output | Inside ;;


let color_of_status = fun st -> match st with
  | Input -> Graphics.green
  | Output -> Graphics.red
  | Inside -> Graphics.rgb 192 192 192 (* light gray *)
;;


let draw_wall = fun f -> fun u -> fun v ->
  match List.map f (get_wall u v) with
  | [(x1,y1); (x2,y2)] ->
    let _ = Graphics.moveto x1 y1 in
    Graphics.lineto x2 y2
  | _ -> ()
;;


let draw_vertex = fun f -> fun st -> fun v -> fun succ ->
  let contour = List.map f (get_contour v) in
  let contour' = Array.of_list contour in
  let _ = Graphics.set_color (color_of_status st) in
  let _ = Graphics.fill_poly contour' in
  let _ = Graphics.set_color Graphics.black in
  let walls = List.filter (fun e -> not (CellSet.mem e succ)) (get_neighbours v) in
  List.iter (fun u -> draw_wall f u v) walls
;;


let get_status = fun v_in -> fun v_out -> fun v ->
  if v = v_in then       Input
  else if v = v_out then Output
  else                   Inside
;;


let init = fun w -> fun h ->
  let _ = Graphics.open_graph "" in
  let _ = Graphics.resize_window w h in
  Graphics.clear_graph ()
;;

let wait_and_close = fun () ->
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
  Graphics.close_graph ()
;;


let draw_maze = fun f -> fun g -> fun v_in -> fun v_out ->
  CellMap.iter (fun v succ -> draw_vertex f (get_status v_in v_out v) v succ) g
;;


let rec draw_path_aux = fun f -> fun g -> fun v -> fun succ -> fun path ->
  match path with
  | [] -> ()
  | v'::vs ->
    if CellSet.mem v' succ then
      try
        let succ' = CellMap.find v' g in
        let x,y = f (get_center_coord v) in
        let x',y' = f (get_center_coord v') in
        let _ = Graphics.moveto x y in
        let _ = Graphics.lineto x' y' in
        draw_path_aux f g v' succ' vs
      with _ ->
        let (i',j') = v' in Printf.printf "(%d,%d) is outside of the maze\n" i' j'
    else
      let (i,j) = v in
      let (i',j') = v' in
      Printf.printf "No path from (%d,%d) to (%d,%d)\n" i j i' j'
;;

let draw_path = fun f -> fun g -> fun path -> match path with
  | [] -> ()
  | v::vs ->
    try
      let succ = CellMap.find v g in
      let _ = Graphics.set_color Graphics.blue in
      draw_path_aux f g v succ vs
    with _ ->
      let (i,j) = v in Printf.printf "(%d,%d) is outside of the maze\n" i j
;;


let test = fun (w,h) -> fun (z,sw,sh) -> fun g -> fun v_in -> fun v_out -> fun path ->
  let _ = init w h in
  let f = fun (x,y) -> (z*x+sw, z*y+sh) in
  let _ = draw_maze f g v_in v_out in
  let _ = draw_path f g path in
  wait_and_close ()
;;
