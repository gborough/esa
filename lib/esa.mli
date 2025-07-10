open Bigarray

type t = (int, int_elt, c_layout) Array1.t
(** Type of array container. *)

exception Sais_error of string
exception Esa_error of string

val set_freq_bucket : str_input:t -> freq_bucket:t -> unit
(** [set_freq_bucket si fb] takes a string input [si] and a frequency bucket
    [fb] to store the calculated letter frequencies. *)

val set_boundary_bucket :
  freq_bucket:t -> boundary_bucket:t -> use_end_boundary:bool -> unit
(** [set_boundary_bucket fb bb ueb] takes a frequency bucket [fb] and a boundary
    bucket [bb] to store the calculated boundaries for each letter. The suffix
    array is induced twice from left to right and right to left, so [ueb]
    boolean determines if the calculation commences from the start or the end of
    the boundary bucket. *)

val induce_sa :
  str_input:t ->
  sa_output:t ->
  freq_bucket:t ->
  boundary_bucket:t ->
  str_len:int ->
  unit
(** [induce_sa si sa fb bb sl] induces the suffix array using an output array
    [sa] for a string input [si] of length [sl], by utilising precalculated
    frequency and boundary bucket [fb] [bb] *)

val sais :
  str_input:t -> sa_output:t -> str_len:int -> alphabet_size:int -> unit
(** [sais si sa sl az] recursively sot all suffixes lexicographically for string
    input [si] of length [sl], the result of which is stored in [sa]. Alphabet
    size [az] is usually 256 as ASCII. *)

val esa :
  str_input:t ->
  sa_output:t ->
  l_output:t ->
  r_output:t ->
  d_output:t ->
  alphabet_size:int ->
  int
(** [esa si sa lt rt dt az] is the top level function to compute both suffix
    array and enhanced suffix array for string input [si], storing the suffix
    output in array [sa], the LMS characters in array [lt], RMS characters in
    array [rt], the depth array in [dt]. Alphabet size [az] is usually 256 as
    ASCII *)
