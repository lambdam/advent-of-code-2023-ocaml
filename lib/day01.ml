let is_digit c =
  let open Char in
  code c >= code '0' && code c <= code '9'

let extract_numbers line =
  let chars = List.init (String.length line) (String.get line) in
  let rec parse first last list =
    match (first, last, list) with
    | (Some f, Some l, []) -> (f, l)
    | (None, _, []) | (_, None, []) -> failwith "All lines must have digits"
    | (None, None, c :: tail) when is_digit c -> parse (Some c) (Some c) tail
    | (Some _, _, c :: tail) when is_digit c -> parse first (Some c) tail
    | (_, _, _ :: tail) -> parse first last tail
  in
  parse None None chars

let get_result_day_1_part_1 lines =
  lines
  |> List.map extract_numbers
  |> List.map (fun chars ->
    let (first, last) = chars in
    [first; last]
    |> List.to_seq
    |> String.of_seq
    |> int_of_string)
  |> List.fold_left (+) 0
