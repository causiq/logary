module Logary.Tests.Diff
// https://github.com/gjaldon/simple-diff/blob/f035463b4cba65d28301b432750e5dcf81c9e899/src/simple_diff.ml
// Copyright (c) 2017 Gabriel Jaldon gjaldon85@gmail.com
// Permission to use, copy, modify, and distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

type Diff =
  | Deleted of string[]
  | Added of string[]
  | Equal of string[]

type T = Diff list

type SequenceInfo =
  { (* Starting index of longest subsequence in the list of new values *)
    sub_start_new: int
    (* Starting index of longest subsequence in the list of old values *)
    sub_start_old: int
    (* The length of the longest subsequence *)
    longest_subsequence: int }

(* Returns a map with the line as key and a list of indices as value.
   Represents counts of all the lines. *)
let map_counter keys =
  let keys_and_indices = Array.mapi (fun index key -> index, key) keys
  Array.fold (fun map (index, key) ->
    let indices =
      match map |> Map.tryFind key with
      | None ->
        []
      | Some vs ->
        vs
    map |> Map.add key (index :: indices)
  ) Map.empty keys_and_indices


(* Computes longest subsequence and returns data on the length of longest
   subsequence and the starting index for the longest subsequence in the old
   and new versions. *)
let get_longest_subsequence old_lines new_lines =
  let old_values_counter = map_counter old_lines
  let overlap = ref Map.empty
  let sub_start_old = ref 0
  let sub_start_new = ref 0
  let longest_subsequence = ref 0

  Array.iteri (fun new_index new_value ->
      let indices =
        match old_values_counter |> Map.tryFind new_value with
        | None -> []
        | Some xs -> xs

      List.iter (fun old_index ->
          let prev_subsequence =
            match !overlap |> Map.tryFind (old_index - 1) with
            | None -> 0
            | Some x -> x
          let new_subsequence = prev_subsequence + 1 in
          overlap := !overlap |> Map.add old_index new_subsequence

          if new_subsequence > !longest_subsequence then
            sub_start_old := old_index - new_subsequence + 1;
          sub_start_new := new_index - new_subsequence + 1;
          longest_subsequence := new_subsequence;
        ) indices;
    ) new_lines;

  { sub_start_new = !sub_start_new;
    sub_start_old = !sub_start_old;
    longest_subsequence = !longest_subsequence }

let rec get_diff old_lines new_lines =
  match old_lines, new_lines with
  | [||], [||] -> []
  | _, _ ->
    let si = get_longest_subsequence old_lines new_lines

    if si.longest_subsequence = 0 then
      [ Deleted old_lines; Added new_lines]
    else
      let old_lines_presubseq = Array.sub old_lines 0 si.sub_start_old in
      let new_lines_presubseq = Array.sub new_lines 0 si.sub_start_new in
      let old_lines_postsubseq =
        let start_index = si.sub_start_old + si.longest_subsequence in
        let end_index = Array.length old_lines - start_index in
        Array.sub old_lines start_index end_index
      in
      let new_lines_postsubseq =
        let start_index = si.sub_start_new + si.longest_subsequence in
        let end_index = Array.length new_lines - start_index in
        Array.sub new_lines start_index end_index
      in
      let unchanged_lines = Array.sub new_lines si.sub_start_new si.longest_subsequence in
      get_diff old_lines_presubseq new_lines_presubseq @
      [Equal unchanged_lines] @
      get_diff old_lines_postsubseq new_lines_postsubseq
