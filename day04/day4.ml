type range = int * int

let make_range (r : string list) : range =
  match r with
  | [ low; high ] -> (int_of_string low, int_of_string high)
  | _ -> failwith "Invalid range"

let build_ranges (l : string list) : range * range =
  match l with
  | [ r1; r2 ] ->
      ( make_range (String.split_on_char '-' r1),
        make_range (String.split_on_char '-' r2) )
  | _ -> failwith "Invalid ranges: "

let get_ranges (data : string) : range * range =
  data |> String.split_on_char ',' |> build_ranges

let contain (((l1, h1), (l2, h2)) : range * range) : bool =
  (l1 <= l2 && h1 >= h2) || (l1 >= l2 && h1 <= h2)

let part_one (data : string list) : int =
  data |> List.map get_ranges |> List.filter contain |> List.length

let overlap (((l1, h1), (l2, h2)) : range * range) : bool =
  (l1 <= l2 && l2 <= h1)
  || (l2 <= l1 && l1 <= h2)
  || (h2 <= h1 && h2 >= l1)
  || (h1 <= h2 && h1 >= l2)

let part_two (data : string list) : int =
  data |> List.map get_ranges |> List.filter overlap |> List.length

let solve (filename : string) : unit =
  let maxi = filename |> Utils.prepare_data in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one maxi in
  Format.printf "Part 1 : %d, ran in %fs\n%!" res time;
  let res, time = Utils.time part_two maxi in
  Format.printf "Part 2 : %d, ran in %fs\n%!" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve
