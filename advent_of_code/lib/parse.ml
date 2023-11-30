open Angstrom
open Base

let is_digit digit = match digit with '0' .. '9' -> true | _ -> false

let inventory =
  let elf_inventory =
    many1
      ((fun x -> Int.of_string (String.of_char_list x))
      <$> many1 (satisfy is_digit)
      <* end_of_line)
  in
  sep_by end_of_line elf_inventory
