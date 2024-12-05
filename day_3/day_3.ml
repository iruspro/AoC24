let task_1 file_content =
  let rec get_result result = function
    | "" -> result
    | program -> (
        let i = String.index_opt program '(' and
        program_length = String.length program
        in
        match i with
        | None -> get_result result ""
        | Some i when i > 2 -> (
            let comand = String.sub program (i - 3) 3 in
            if comand <> "mul" then
              get_result result (String.sub program (succ i) (program_length - (i + 1)))
            else
              let j = String.index_from_opt program i ')' in
              match j with
              | None -> get_result result ""
              | Some j -> (
                  let numbers = 
                    String.sub program (succ i) (j - (i + 1)) |>
                    String.split_on_char ','
                  in
                  if List.length numbers <> 2 then
                    get_result result (String.sub program (succ i) (program_length - (i + 1)))
                  else
                    let numbers_list = List.filter_map int_of_string_opt numbers in
                    if List.length numbers_list <> 2 then
                      get_result result (String.sub program (succ i) (program_length - (i + 1)))
                    else
                      let term = List.fold_right ( * ) numbers_list 1 in
                      get_result (result + term) (String.sub program (succ j) (program_length - (j + 1)))
                )
          )
        | Some i -> get_result result (String.sub program (succ i) (program_length - (i + 1)))
      )
  in
  string_of_int (get_result 0 file_content)


let task_2 file_content =
  "10"

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
  let file_content = read_file "day_3.in" in
  let answer_1 = task_1 file_content
  and answer_2 = task_2 file_content
  in
  write_file "day_3_1.out" answer_1;
  write_file "day_3_2.out" answer_2