let get_lists file_content = 
  let content_list = String.split_on_char '\n' file_content |>
  List.map (String.split_on_char ' ') |>
  List.map (List.filter (fun elem -> elem <> "")) |>
  List.flatten
  in
  let rec get_lists_aux l_list r_list i = function
    | [] -> List.sort compare l_list, List.sort compare r_list
    | x::xs -> 
      if i mod 2 = 1 then 
        get_lists_aux (int_of_string x::l_list) r_list (succ i) xs
      else 
        get_lists_aux l_list (int_of_string x::r_list) (succ i) xs
  in
  get_lists_aux [] [] 1 content_list

let task_1 file_content =
  let l_list, r_list = get_lists file_content in
  let rec get_distance dist = function
    | [], [] -> dist
    | x::xs, y::ys -> 
      if x - y < 0 then
        get_distance (dist + (y - x)) (xs, ys)
      else
        get_distance (dist + (x - y)) (xs, ys)
    | _ -> failwith ""
  in
  string_of_int (get_distance 0 (l_list, r_list))


let task_2 file_content =
  let l_list, r_list = get_lists file_content in
  let rec counter_aux count elem = function
    | [] -> count
    | x::xs -> 
      if elem = x then 
        counter_aux (succ count) elem xs
      else 
        counter_aux count elem xs
  in
  let rec get_score score = function
    | [] -> score
    | x::xs -> 
      let term = x * (counter_aux 0 x r_list) in
      get_score (score + term) xs
  in
  string_of_int (get_score 0 l_list)

let _ =
  let read_file filename =
      let chan = open_in filename in
      let content = really_input_string chan (in_channel_length chan) in
      close_in chan;
      content
  and write_file filename content =
      let chan = open_out filename in
      output_string chan content;
      close_out chan
  in
  let file_content = read_file "day_1.in" in
  let answer_1 = task_1 file_content
  and answer_2 = task_2 file_content
  in
  write_file "day_1_1.out" answer_1;
  write_file "day_1_2.out" answer_2