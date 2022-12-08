let read_file (name : string) : string =
  let ch = open_in name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let prepare_data (filename : string) : string list =
  filename |> read_file |> String.split_on_char '\n' |> List.map String.trim

let rec take (n : int) (l : 'a list) : 'a list =
  match l with [] -> [] | t :: q -> if n <= 0 then [] else t :: take (n - 1) q

let sum (l : int list) : int =
  let rec tail_rec (l : int list) (accu : int) : int =
    match l with [] -> accu | t :: q -> tail_rec q (accu + t)
  in
  tail_rec l 0

let split_chars s = List.init (String.length s) (String.get s)

exception Utils_trouve of int

let find_in_array (arr : 'a array) (value : 'a) : int =
  try
    for i = 0 to Array.length arr - 1 do
      if arr.(i) = value then raise (Utils_trouve i)
    done;
    -1
  with Utils_trouve i -> i

let time (f : 'a -> 'b) (x : 'a) : ('b * float) =
  let start = Unix.gettimeofday () in
  let res = f x in
  let stop = Unix.gettimeofday () in
  res, (stop -. start)
