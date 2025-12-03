let lines = Adventofcode_2025.Common.read_input "fixtures/day3/input.txt"

let int_value (c: char) : int = 
  int_of_char c - int_of_char '0'
  
let rec find_highest_char_helper current_loc max str (loc, value) = 
  if current_loc > max then (loc, value)
  else let current_val = int_value str.[current_loc] in 
  if current_val > value then find_highest_char_helper (current_loc + 1) max str (current_loc, current_val)
  else find_highest_char_helper (current_loc + 1) max str (loc, value)

let find_highest_char (first_loc: int) (max: int) (str: string): int * int = 
  find_highest_char_helper first_loc max str (0, -1)


let line_total (previous_total: int) (line: string): int = 
  let (loc, first_val) = find_highest_char 0 (String.length line - 2) line in
  let (_, second_val) = find_highest_char (loc + 1) (String.length line - 1) line in
  let new_val = (10 * first_val + second_val) in
  let () = Printf.printf "%d\n" new_val in
  previous_total + new_val

let sol = List.fold_left line_total 0 lines

let () = Printf.printf "%d\n" sol

