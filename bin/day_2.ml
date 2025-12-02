let range_strings = Adventofcode_2025.Common.read_input "fixtures/day2/input.txt" |>
  List.hd |>
  String.split_on_char ','

let string_helper range_string = 
  let range_list = (String.split_on_char '-' range_string) in
  List.map (fun x -> int_of_string x) range_list

let ranges = List.map string_helper range_strings

let rec range_summer (min: int) (max: int) (tot: int): int = 
  if min > max then tot
  else let str = string_of_int min in
    let string_length = String.length str in
  if string_length mod 2 <> 0 then range_summer (min + 1) max tot
  else if (String.sub str (string_length/2) (string_length/2)) = (String.sub str 0 (string_length/2)) then range_summer (min + 1) max (tot + min)
  else range_summer (min + 1) max tot


let sum_range tot (l: int list) : int = 
  let local_tot = range_summer (List.hd l) (List.nth l 1) 0 in
  tot + local_tot

let total = List.fold_left sum_range 0 ranges

let () = Printf.printf "%d\n" total
