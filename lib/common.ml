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

