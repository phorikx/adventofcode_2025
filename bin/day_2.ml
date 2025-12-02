open Batteries

let range_strings = Adventofcode_2025.Common.read_input "fixtures/day2/input.txt" |>
  List.hd |>
  String.split_on_char ','

let string_helper range_string = 
  let range_list = (String.split_on_char '-' range_string) in
  List.map (fun x -> int_of_string x) range_list

let ranges = List.map string_helper range_strings

let is_valid_num (a: int): bool = 
  let str = string_of_int a in
  let string_length = String.length str in
  if string_length mod 2 <> 0 then false 
  else (String.sub str (string_length/2) (string_length/2)) = (String.sub str 0 (string_length/2))

let rec range_summer (min: int) (max: int) (tot: int): int = 
  if min > max then tot
  else if is_valid_num min then range_summer (min + 1) max (tot + min)
  else range_summer (min + 1) max tot

let equal_chunks str n = 
  let len = String.length str in
  List.init (len / n) (fun i -> String.sub str (i*n) n)

let rec list_checker list = 
  if List.length list < 2 then true
  else if List.hd list = List.nth list 1 then list_checker (List.tl list)
  else false

let rec valid_checker a cur max = 
  if cur > max then false
  else let str = string_of_int a in
  let string_length = String.length str in
  if string_length mod cur <> 0 && string_length / cur > 1 then valid_checker a (cur + 1) max
  else let chunks = equal_chunks str cur in 
  if list_checker chunks then true
  else valid_checker a (cur + 1) max

let is_valid_num_b (a: int): bool = 
  let str = string_of_int a in
  let string_length = String.length str in
  valid_checker a 1 (string_length / 2)

let rec range_summer_b (min: int) (max: int) (tot: int): int = 
  if min > max then tot
  else if is_valid_num_b min then 
    range_summer_b (min + 1) max (tot + min)
  else range_summer_b (min + 1) max tot

let sum_range tot (l: int list) : int = 
  let local_tot = range_summer (List.hd l) (List.nth l 1) 0 in
  tot + local_tot

let sum_range_b tot (l: int list) : int = 
  let local_tot = range_summer_b (List.hd l) (List.nth l 1) 0 in
  tot + local_tot

let total = List.fold_left sum_range 0 ranges
let total_b = List.fold_left sum_range_b 0 ranges

let () = Printf.printf "%d\n" total
let () = Printf.printf "%d\n" total_b
