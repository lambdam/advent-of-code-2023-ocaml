let day_01_puzzle =
  let open In_channel in
  with_open_bin "puzzles/day01.txt" input_lines

let () = print_endline begin
    "Day01 Part 1: " ^ (day_01_puzzle
                        |> Advent_of_code_2023_ocaml.Day01.get_result_day_1_part_1
                        |> Int.to_string)
  end
