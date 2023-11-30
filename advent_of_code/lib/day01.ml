open Angstrom
open Core

let ( >>> ) f g = Fn.compose g f
let is_digit digit = match digit with '0' .. '9' -> true | _ -> false

let decimal =
  Fn.compose Int.of_string String.of_char_list <$> many1 (satisfy is_digit)

let inventory =
  let elf_inventory = many1 (decimal <* end_of_line) in
  sep_by end_of_line elf_inventory

let find_top_elves n =
  let sum = List.sum (module Int) ~f:Fn.id in
  List.map ~f:sum
  >>> List.sort ~compare:Int.descending
  >>> Fn.flip List.take n >>> sum

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
