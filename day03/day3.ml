let overlapping ((gauche, droite) : string * string) : char =
  gauche |> Utils.split_chars |> List.filter (String.contains droite) |> List.hd

let scores : char array =
  Array.init 52 (fun i ->
      if i < 26 then Char.chr (i + 97) else Char.chr (i + 65 - 26))

let parse_ligne (ligne : string) : string * string =
  let len = String.length ligne in
  (String.sub ligne 0 (len / 2), String.sub ligne (len / 2) (len / 2))

let part_one (data : string list) : int =
  data |> List.map parse_ligne |> List.map overlapping
  |> List.map (Utils.find_in_array scores)
  |> Utils.sum

let rec group (data : string list) : (string * string * string) list =
  match data with t1 :: t2 :: t3 :: q -> (t1, t2, t3) :: group q | _ -> []

let common ((s1, s2, s3) : string * string * string) : char =
  s1 |> Utils.split_chars
  |> List.filter (String.contains s2)
  |> List.filter (String.contains s3)
  |> List.hd

let part_two (data : string list) : int =
  data |> group |> List.map common
  |> List.map (Utils.find_in_array scores)
  |> Utils.sum

let solve (filename : string) : unit =
  let maxi = filename |> Utils.prepare_data in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one maxi in
  Format.printf "Part 1 : %d, ran in %fs%!\n" res time;
  let res, time = Utils.time part_two maxi in
  Format.printf "Part 2 : %d, ran in %fs%!\n" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve