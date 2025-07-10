open Bigarray

let print_sa sa_output =
  Printf.printf "SA: [ ";
  for i = 0 to Array1.dim sa_output - 1 do
    Printf.printf "%d " (Array1.unsafe_get sa_output i)
  done;
  Printf.printf "]\n"

let print_snipet t sa d id2word =
  let is_whitespace c =
    match char_of_int c with
    | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' -> true
    | _ -> false
  in
  for i = 0 to d - 1 do
    let c = Array1.unsafe_get t (sa + i) in
    if Array.length id2word > 0 then Printf.printf "%s " id2word.(c)
    else if is_whitespace c then print_char '_'
    else print_char (char_of_int c)
  done
