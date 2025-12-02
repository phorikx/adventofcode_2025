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


let result = List.fold_left count_zeroes (50, 0) lines

let () = result |>
  snd |>
  string_of_int |>
  print_endline
