open Bigarray

type t = (int, int_elt, c_layout) Array1.t

exception Sais_error of string
exception Esa_error of string

let set_freq_bucket ~str_input ~freq_bucket =
  let len = Array1.dim str_input in
  for i = 0 to Array1.dim freq_bucket - 1 do
    Array1.unsafe_set freq_bucket i 0
  done;
  for i = 0 to len - 1 do
    let sym = Array1.unsafe_get str_input i in
    Array1.unsafe_set freq_bucket sym (Array1.unsafe_get freq_bucket sym + 1)
  done

let set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary =
  let sum = ref 0 in
  let len = Array1.dim freq_bucket in
  for i = 0 to len - 1 do
    if use_end_boundary then (
      sum := !sum + Array1.unsafe_get freq_bucket i;
      Array1.unsafe_set boundary_bucket i !sum)
    else (
      Array1.unsafe_set boundary_bucket i !sum;
      sum := !sum + Array1.unsafe_get freq_bucket i)
  done

let has_high_bit j = j land (1 lsl (Sys.word_size - 1)) <> 0

let induce_sa ~str_input ~sa_output ~freq_bucket ~boundary_bucket ~str_len =
  set_freq_bucket ~str_input ~freq_bucket;
  set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary:false;

  let j = ref (str_len - 1) in
  let c0 = ref 0 in
  let c1 = ref (Array1.unsafe_get str_input !j) in
  let index = ref (Array1.unsafe_get boundary_bucket !c1) in
  Array1.unsafe_set sa_output !index
    (if !j > 0 && Array1.unsafe_get str_input (!j - 1) < !c1 then lnot !j
     else !j);
  incr index;

  for i = 0 to str_len - 1 do
    j := Array1.unsafe_get sa_output i;
    Array1.unsafe_set sa_output i (lnot !j);
    if (not (has_high_bit !j)) && !j > 0 then (
      decr j;
      c0 := Array1.unsafe_get str_input !j;
      if !c0 <> !c1 then (
        Array1.unsafe_set boundary_bucket !c1 !index;
        c1 := !c0;
        index := Array1.unsafe_get boundary_bucket !c1);
      Array1.unsafe_set sa_output !index
        (if
           !j > 0
           && (not (has_high_bit !j))
           && Array1.unsafe_get str_input (!j - 1) < !c1
         then lnot !j
         else !j);
      incr index)
  done;

  set_freq_bucket ~str_input ~freq_bucket;
  set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary:true;
  c1 := 0;
  index := Array1.unsafe_get boundary_bucket !c1;
  for i = str_len - 1 downto 0 do
    j := Array1.unsafe_get sa_output i;
    if !j > 0 && not (has_high_bit !j) then (
      decr j;
      c0 := Array1.unsafe_get str_input !j;
      if !c0 <> !c1 then (
        Array1.unsafe_set boundary_bucket !c1 !index;
        c1 := !c0;
        index := Array1.unsafe_get boundary_bucket !c1);
      decr index;
      Array1.unsafe_set sa_output !index
        (if !j = 0 || Array1.unsafe_get str_input (!j - 1) > !c1 then lnot !j
         else !j))
    else Array1.unsafe_set sa_output i (lnot !j)
  done

let rec suffix_sort (str_input : t) (sa_output : t) (free_size : int)
    (str_len : int) (alphabet_size : int) : unit =
  let freq_bucket = Array1.create int c_layout alphabet_size in
  let boundary_bucket = Array1.create int c_layout alphabet_size in

  set_freq_bucket ~str_input ~freq_bucket;
  set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary:true;

  for i = 0 to str_len - 1 do
    Array1.unsafe_set sa_output i 0
  done;

  let c_index = ref 0 in
  let c0 = ref 0 in
  let c1 = ref (Array1.unsafe_get str_input (str_len - 1)) in
  for i = str_len - 2 downto 0 do
    c0 := Array1.unsafe_get str_input i;
    if !c0 < !c1 + !c_index then c_index := 1
    else if !c_index <> 0 then (
      let b = Array1.unsafe_get boundary_bucket !c1 - 1 in
      Array1.unsafe_set boundary_bucket !c1 b;
      Array1.unsafe_set sa_output b (i + 1);
      c_index := 0);
    c1 := !c0
  done;
  induce_sa ~str_input ~sa_output ~freq_bucket ~boundary_bucket ~str_len;

  let p = ref 0 in
  let j = ref 0 in
  let m = ref 0 in
  for i = 0 to str_len - 1 do
    p := Array1.unsafe_get sa_output i;
    c0 := Array1.unsafe_get str_input !p;
    if !p > 0 && Array1.unsafe_get str_input (!p - 1) > !c0 then (
      j := !p + 1;
      if !j < str_len then c1 := Array1.unsafe_get str_input !j;
      while !j < str_len && !c0 = !c1 do
        c1 := Array1.unsafe_get str_input !j;
        incr j
      done;
      if !j < str_len && !c0 < !c1 then (
        Array1.unsafe_set sa_output !m !p;
        incr m))
  done;
  j := !m + (str_len lsr 1);
  for i = !m to !j - 1 do
    Array1.unsafe_set sa_output i 0
  done;

  j := str_len;
  let c_index = ref 0 in
  c1 := Array1.unsafe_get str_input (str_len - 1);
  for i = str_len - 2 downto 0 do
    c0 := Array1.unsafe_get str_input i;
    if !c0 < !c1 + !c_index then c_index := 1
    else if !c_index <> 0 then (
      Array1.unsafe_set sa_output (!m + ((i + 1) lsr 1)) (!j - i - 1);
      j := i + 1;
      c_index := 0);
    c1 := !c0
  done;

  let name = ref 0 in
  let q = ref str_len in
  let qlen = ref 0 in
  let plen = ref 0 in
  let diff = ref false in
  for i = 0 to !m - 1 do
    p := Array1.unsafe_get sa_output i;
    plen := Array1.unsafe_get sa_output (!m + (!p lsr 1));
    diff := true;
    if !plen = !qlen then (
      j := 0;
      while
        !j < !plen
        && Array1.unsafe_get str_input (!p + !j)
           = Array1.unsafe_get str_input (!q + !j)
      do
        incr j
      done;
      if !j = !plen then diff := false);
    if !diff then (
      incr name;
      q := !p;
      qlen := !plen);
    Array1.unsafe_set sa_output (!m + (!p lsr 1)) !name
  done;

  if !name < !m then (
    let ra_index = str_len + free_size - !m in
    j := !m - 1;
    let a = !m + (str_len lsr 1) in
    for i = a - 1 downto !m do
      let v = Array1.unsafe_get sa_output i in
      if v <> 0 then (
        Array1.unsafe_set sa_output (ra_index + !j) (v - 1);
        decr j)
    done;
    let ra = Array1.sub sa_output ra_index !m in
    suffix_sort ra sa_output (free_size + str_len - (!m * 2)) !m !name;
    j := !m - 1;
    let c_index = ref 0 in
    c1 := Array1.unsafe_get str_input (str_len - 1);
    for i = str_len - 2 downto 0 do
      c0 := Array1.unsafe_get str_input i;
      if !c0 < !c1 + !c_index then c_index := 1
      else if !c_index <> 0 then (
        Array1.unsafe_set sa_output (ra_index + !j) (i + 1);
        decr j;
        c_index := 0);
      c1 := !c0
    done;
    for i = 0 to !m - 1 do
      let idx = Array1.unsafe_get sa_output i in
      Array1.unsafe_set sa_output i
        (Array1.unsafe_get sa_output (ra_index + idx))
    done);

  set_freq_bucket ~str_input ~freq_bucket;
  set_boundary_bucket ~freq_bucket ~boundary_bucket ~use_end_boundary:true;
  for i = !m to str_len - 1 do
    Array1.unsafe_set sa_output i 0
  done;
  for i = !m - 1 downto 0 do
    j := Array1.unsafe_get sa_output i;
    Array1.unsafe_set sa_output i 0;
    let idx =
      Array1.unsafe_get boundary_bucket (Array1.unsafe_get str_input !j) - 1
    in
    Array1.unsafe_set boundary_bucket (Array1.unsafe_get str_input !j) idx;
    Array1.unsafe_set sa_output idx !j
  done;
  induce_sa ~str_input ~sa_output ~freq_bucket ~boundary_bucket ~str_len

let sais ~str_input ~sa_output ~str_len ~alphabet_size =
  if str_len = 0 then raise (Sais_error " SA-IS input string must not be empty");
  if str_len = 1 then (
    Array1.unsafe_set sa_output 0 0;
    ())
  else
    let free_size = 0 in
    suffix_sort str_input sa_output free_size str_len alphabet_size

let suffixtree str_input sa_output l_output r_output d_output =
  let n = Array1.dim str_input in
  if n = 0 then 0
  else (
    Array1.unsafe_set l_output
      (Array1.unsafe_get sa_output 0)
      (Array1.unsafe_get sa_output (n - 1));
    for i = 1 to n - 1 do
      Array1.unsafe_set l_output
        (Array1.unsafe_get sa_output i)
        (Array1.unsafe_get sa_output (i - 1))
    done;

    let h = ref 0 in
    for i = 0 to n - 1 do
      let j = Array1.unsafe_get l_output i in
      while
        !h + i < n
        && !h + j < n
        && Array1.unsafe_get str_input (i + !h)
           = Array1.unsafe_get str_input (j + !h)
      do
        incr h
      done;
      Array1.unsafe_set r_output i !h;
      if !h > 0 then decr h
    done;

    for i = 0 to n - 1 do
      Array1.unsafe_set l_output i
        (Array1.unsafe_get r_output (Array1.unsafe_get sa_output i))
    done;
    Array1.unsafe_set l_output 0 (-1);

    let stack = ref [ (-1, -1) ] in
    let node_num = ref 0 in
    let i = ref 0 in
    let break = ref false in
    while not !break do
      let cur =
        if !i = n then (!i, -1) else (!i, Array1.unsafe_get l_output !i)
      in
      let cur_ref = ref cur in
      let cand = ref (List.hd !stack) in
      let rec pop_stack () =
        if snd !cand > snd !cur_ref then (
          if !i - fst !cand > 1 then (
            Array1.unsafe_set l_output !node_num (fst !cand);
            Array1.unsafe_set r_output !node_num !i;
            Array1.unsafe_set d_output !node_num (snd !cand);
            incr node_num;
            if !node_num >= n then break := true);
          cur_ref := (fst !cand, snd !cur_ref);
          stack := List.tl !stack;
          cand := List.hd !stack;
          pop_stack ())
      in
      pop_stack ();

      if snd !cand < snd !cur_ref then stack := !cur_ref :: !stack;
      if !i = n then break := true
      else (
        stack := (!i, n - Array1.unsafe_get sa_output !i + 1) :: !stack;
        incr i)
    done;
    !node_num)

let esa ~str_input ~sa_output ~l_output ~r_output ~d_output ~alphabet_size =
  let str_len = Array1.dim str_input in
  try
    sais ~str_input ~sa_output ~str_len ~alphabet_size;
    suffixtree str_input sa_output l_output r_output d_output
  with Esa_error msg -> raise (Esa_error ("Esa failed: " ^ msg))
