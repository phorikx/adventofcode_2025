exception CharacterInputError of string

let lines = Adventofcode_2025.Common.read_input "fixtures/day1/input.txt";;

let count_zeroes ((point, total) : int * int) (s: string) : int * int =
  let val_leng = String.length s - 1 in
  let shift = String.sub s 1 val_leng |>
    int_of_string in
  let new_point = if s.[0] = "L".[0] then Adventofcode_2025.Common.modulo (point - shift) 100
    else if s.[0] = "R".[0] then Adventofcode_2025.Common.modulo (point + shift) 100
  else raise (CharacterInputError "Correct character not found") in
let new_total = if new_point = 0 then total + 1
else total in
  (new_point, new_total)

let count_passing_zeroes ((point, total) : int * int) (s: string) : int * int =
  let val_leng = String.length s - 1 in
  let shift = String.sub s 1 val_leng |>
    int_of_string in
  let new_point = if s.[0] = "L".[0] then (point - shift)
    else if s.[0] = "R".[0] then (point + shift)
    else raise (CharacterInputError "Correct character not found") in
  let new_point_mod = Adventofcode_2025.Common.modulo new_point 100 in
  let point_div = new_point / 100 in
  let adder = if new_point <= 0 && point <> 0 then - point_div + 1
    else if new_point < 0 && point = 0 then - point_div
    else point_div in
  let new_total = total + adder in
    (new_point_mod, new_total)


let result_a = List.fold_left count_zeroes (50, 0) lines

let result_b = List.fold_left count_passing_zeroes (50, 0) lines

let () = result_a |>
  snd |>
  string_of_int |>
  print_endline

let () = result_b |>
  snd |>
  string_of_int |>
  print_endline
