open Year2022.Day01
open Angstrom
open Core

let part_one = find_top_elves 1
let part_two = find_top_elves 3

let () =
  let input = In_channel.read_all "../input/2022/day01.txt" in
  let go lbl f = f >>> Int.to_string >>> ( ^ ) lbl >>> print_endline in
  match parse_string ~consume:Prefix inventory input with
  | Ok inventories ->
      go "Part One: " part_one inventories;
      go "Part Two: " part_two inventories
  | Error msg -> Stdio.print_endline msg
