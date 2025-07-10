open Esa
open Utils
open Bigarray

let%expect_test "bucket_test" =
  let str = "abracadabra" in
  let str_len = String.length str in
  let str_input =
    Array1.of_array int c_layout
      (Array.init str_len (fun i -> int_of_char str.[i]))
  in
  let freq_bucket = Array1.create int c_layout 256 in
  set_freq_bucket ~str_input ~freq_bucket;
  Printf.printf "freq_bucket (for 'abracadabra'):\n";
  for i = 0 to 255 do
    let count = Array1.unsafe_get freq_bucket i in
    if count > 0 then Printf.printf "char %c (%d): %d\n" (char_of_int i) i count
  done;

  let boundary_bucket = Array1.create int c_layout 256 in
  set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary:false;
  Printf.printf "boundary_bucket (start):\n";
  for i = 0 to 255 do
    let b = Array1.unsafe_get boundary_bucket i in
    if Array1.unsafe_get freq_bucket i > 0 then
      Printf.printf "char %c (%d): %d\n" (char_of_int i) i b
  done;
  set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary:true;
  Printf.printf "boundary_bucket (end):\n";
  for i = 0 to 255 do
    let b = Array1.unsafe_get boundary_bucket i in
    if Array1.unsafe_get freq_bucket i > 0 then
      Printf.printf "char %c (%d): %d\n" (char_of_int i) i b
  done;
  [%expect
    {|
    freq_bucket (for 'abracadabra'):
    char a (97): 5
    char b (98): 2
    char c (99): 1
    char d (100): 1
    char r (114): 2
    boundary_bucket (start):
    char a (97): 0
    char b (98): 5
    char c (99): 7
    char d (100): 8
    char r (114): 9
    boundary_bucket (end):
    char a (97): 5
    char b (98): 7
    char c (99): 8
    char d (100): 9
    char r (114): 11
    |}]

let%expect_test "induce_test" =
  let str_input =
    Array1.of_array int c_layout
      (Array.init 11 (fun i -> int_of_char "abracadabra".[i]))
  in
  let freq_bucket = Array1.create int c_layout 256 in
  let boundary_bucket = Array1.create int c_layout 256 in
  let sa_output0 =
    Array1.of_array int c_layout [| 0; 0; 3; 5; 7; 0; 0; 0; 0; 0; 0 |]
  in
  let sa_output1 =
    Array1.of_array int c_layout [| 0; 0; 7; 3; 5; 0; 0; 0; 0; 0; 0 |]
  in
  induce_sa ~str_input ~sa_output:sa_output0 ~freq_bucket ~boundary_bucket
    ~str_len:11;
  induce_sa ~str_input ~sa_output:sa_output1 ~freq_bucket ~boundary_bucket
    ~str_len:11;
  Printf.printf "sa0:";
  for i = 0 to 10 do
    Printf.printf "%d " (Array1.unsafe_get sa_output0 i)
  done;
  Printf.printf "\nsa1:";
  for i = 0 to 10 do
    Printf.printf "%d " (Array1.unsafe_get sa_output1 i)
  done;
  [%expect
    {|
    sa0:10 7 0 3 5 8 1 4 6 9 2
    sa1:10 7 0 3 5 8 1 4 6 9 2
    |}]

let%expect_test "esa_test" =
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
  done;
  [%expect
    {|
    String Length: 65
    SA: [ 53 37 19 5 28 12 8 41 15 1 0 34 2 49 30 23 51 52 36 18 11 22 59 54 43 60 25 14 38 45 64 35 56 20 32 17 57 62 6 4 29 48 3 50 10 58 63 55 31 13 9 39 46 42 40 21 44 61 47 7 26 27 24 16 33 ]
    Node Number: 23
    Full ESA:
    0	2	2	_o
    1	9	1	_
    2	2	2	an
    3	5	1	a
    4	4	2	e_
    5	2	2	er
    6	10	1	e
    7	2	3	for
    8	3	1	f
    9	5	1	g
    10	2	2	in
    11	3	1	i
    12	2	1	l
    13	2	1	m
    14	2	2	ne
    15	3	2	ng
    16	6	1	n
    17	2	2	or
    18	4	1	o
    19	5	1	r
    20	2	1	s
    21	3	1	t
    22	65	0
    |}]
