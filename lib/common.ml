let read_line i = try Some(input_line i) with End_of_file -> None

let read_input path = 
  let rec lines_from_files_aux i acc = match (read_line i) with
    | None -> List.rev acc
    | Some s -> lines_from_files_aux i (s :: acc) in
  lines_from_files_aux (open_in path) []

let modulo (x: int) (y: int) : int =
  let result = x mod y in 
  if result >= 0 then result
  else result + y

let get_neighbors (char_array: char array array) ((j, k) : int * int) : char list  =
  let deltas = [-1; 0; 1] in
  let x_length = Array.length char_array in
  let y_length = Array.length char_array.(0) in
  let deltas = List.concat_map (fun dx -> List.map (fun dy -> (dx, dy)) deltas) deltas in
  let filtered_deltas = deltas |>
  List.filter (fun (x,y) -> x <> 0 || y <> 0 ) |>
  List.filter (fun (x,_) -> not (x = -1 && j = 0)) |>
  List.filter (fun (_,y) -> not (y = -1 && k = 0)) |>
  List.filter (fun (x,_) -> not (x = 1 && j = x_length - 1)) |>
  List.filter (fun (_,y) -> not (y = 1 && k = y_length - 1)) in
  List.map(fun (x,y) -> (char_array.(j + x)).(k + y)) filtered_deltas

let range i j = 
  List.init (j - i) (fun i -> i + 1) |>
  List.map (fun k -> k + i - 1)

let range_from_zero = range 0



