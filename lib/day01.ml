(* Shared *)

let make_get_result_fun f =
  fun lines ->
  lines
  |> List.map f
  |> List.map (fun chars ->
      let (first, last) = chars in
      [first; last]
      |> List.to_seq
      |> String.of_seq
      |> int_of_string)
  |> List.fold_left (+) 0

(* Part 1 *)

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

let get_result_day_1_part_1 = make_get_result_fun extract_numbers

(* Part 2 *)

let regex = "^([0-9]|one|two|three|four|five|six|seven|eight|nine)"
            |> Re.Pcre.re
            |> Re.compile

let is_matching = Re.execp regex

let s_to_char = function
  | "0" | "zero" -> '0'
  | "1" | "one" -> '1'
  | "2" | "two" -> '2'
  | "3" | "three" -> '3'
  | "4" | "four" -> '4'
  | "5" | "five" -> '5'
  | "6" | "six" -> '6'
  | "7" | "seven" -> '7'
  | "8" | "eight" -> '8'
  | "9" | "nine" -> '9'
  | _ -> failwith "Undefined string"

let pop_char text =
  let len = String.length text in
  String.sub text 1 (len - 1)

let extract text =
  let c = Re.exec regex text |> (fun x -> Re.Group.get x 1) |> s_to_char in
  let tail = pop_char text in
  (c, tail)

let extract_numbers_2 text' =
  let rec parse first last text =
    match (first, last, text) with
    | (Some f, Some l, "") -> (f, l)
    | (None, _, "") | (_, None, "") -> failwith "All lines must have digits"
    | (None, None, text) when is_matching text -> begin
        let (c, tail) = extract text in
        parse (Some c) (Some c) tail
      end
    | (Some _, _, text) when is_matching text -> begin
        let (c, tail) = extract text in
        parse first (Some c) tail
      end
    | (_, _, text) -> parse first last (pop_char text)
  in
  parse None None text'

let get_result_day_1_part_2 = make_get_result_fun extract_numbers_2
