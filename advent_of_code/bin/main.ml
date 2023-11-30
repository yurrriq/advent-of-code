open Advent_of_code.Parse
open Angstrom
open Base

(* let ( <<< ) f g x = f (g x) *)
let sum = List.fold ~f:( + ) ~init:0

let part_one =
  let maximum = List.reduce_exn ~f:max in
  Fn.compose maximum (List.map ~f:sum)

let part_two xxs =
  xxs |> List.map ~f:sum
  |> List.sort ~compare:(Fn.flip compare)
  |> Fn.flip List.take 3 |> sum

let example =
  String.concat ~sep:"\n"
    [
      "1000";
      "2000";
      "3000";
      "";
      "4000";
      "";
      "5000";
      "6000";
      "";
      "7000";
      "8000";
      "9000";
      "";
      "10000";
      "";
    ]

let () =
  match parse_string ~consume:Prefix inventory example with
  | Ok res ->
      res |> part_one |> Printf.sprintf "Part one: %d" |> Stdio.print_endline;
      res |> part_two |> Printf.sprintf "Part two: %d" |> Stdio.print_endline
  | Error msg -> Stdio.print_endline msg
