let input = Adventofcode_2025.Common.read_input "fixtures/day5/input_test.txt" 

let make_func str = 
  let bounds = String.split_on_char '-' str |>
    List.map (fun x -> int_of_string x) in
  fun y -> y >= (List.hd bounds) && y <= (List.nth bounds 1)

let rec split_input_helper tail functions = 
  match tail with
    | "" :: rest -> (functions, List.map (fun x -> int_of_string x) rest)
    | expr :: rest -> split_input_helper rest (make_func(expr) :: functions)
    | [] -> (functions, [])

let split_input input = split_input_helper input [] 

let rec any list = 
  match list with
    | false :: tail -> any tail
    | true :: _ -> true
    | [] -> false

let list_counter funcs value = 
  if any (List.map (fun bound_fun -> (bound_fun value)) funcs) then 1
  else 0

let (funcs, vals) = (split_input input)
let total = List.fold_left (fun acc value -> acc + list_counter funcs value) 0 vals 
let _ = Printf.printf "%d\n" total
