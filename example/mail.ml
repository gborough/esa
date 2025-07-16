open Esa
open Utils
open Bigarray

let () =
  let input =
    "OCaml is one of the greatest language for performance engineering"
  in
  let n = String.length input in
  let alphabet_size = 256 in
  let str_input = Array1.create int c_layout n in
  for i = 0 to n - 1 do
    Array1.unsafe_set str_input i (Char.code input.[i])
  done;
  let sa_output = Array1.create int c_layout n in
  let l_output = Array1.create int c_layout n in
  let r_output = Array1.create int c_layout n in
  let d_output = Array1.create int c_layout n in

  Printf.printf "String Length: %d\n" n;

  let node_num =
    try esa ~str_input ~sa_output ~l_output ~r_output ~d_output ~alphabet_size
    with Esa_error msg ->
      Printf.printf "Error: %s\n" msg;
      exit 1
  in
  print_sa sa_output;
  Printf.printf "Node Number: %d\n" node_num;

  Printf.printf "Full ESA:\n";
  for i = 0 to node_num - 1 do
    let l = Array1.unsafe_get l_output i in
    let r = Array1.unsafe_get r_output i in
    let d = Array1.unsafe_get d_output i in
    Printf.printf "%d\t%d\t%d\t" i (r - l) d;
    print_snipet str_input (Array1.unsafe_get sa_output l) d [||];
    Printf.printf "\n"