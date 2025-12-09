open Batteries

let input = Adventofcode_2025.Common.read_input "fixtures/day4/input.txt" |>
  List.map(fun line -> Array.of_seq (String.to_seq line)) |>
  List.to_seq |>
  Array.of_seq

let paper_rolls = List.concat_map (fun x -> (List.map (fun y -> (x, y)) (Adventofcode_2025.Common.range_from_zero (Array.length input.(0))))) (Adventofcode_2025.Common.range_from_zero (Array.length input)) |>
  List.filter (fun (x, y) -> input.(x).(y) = '@')

let paper_rolls_neighbors = 
  List.map (fun (x, y) -> Adventofcode_2025.Common.get_neighbors input (x, y)) paper_rolls

let result = paper_rolls_neighbors |>
  List.map (fun neighbors -> List.filter( fun x -> x = '@') neighbors) |>
  List.map (fun rolls -> List.length rolls) |>
  List.filter (fun num -> num < 4) |>
  List.length 

let () = Printf.printf "%d\n" result

let neighbor_checker input_array (j, k) = 
  let valid_neighbors = Adventofcode_2025.Common.get_neighbors input_array (j, k) |>
  List.filter( fun x -> x = '@') in
  ((List.length valid_neighbors < 4) && input_array.(j).(k) = '@')

let rec b_solver_helper input modified total = 
  if not modified then total 
  else
    let total_mod = ref total in
    let modded = Array.map Array.copy input in 
    for j = 0 to Array.length input - 1 do
        for k = 0 to Array.length input.(j) -1 do
          if neighbor_checker input (j, k) then  begin
          modded.(j).(k) <- '.';
            total_mod := !total_mod + 1
          end
        done 
      done;
  b_solver_helper modded (total <> !total_mod) !total_mod

let b_solver input = 
  b_solver_helper input true 0

let _ = Printf.printf "%d\n" (b_solver input)
