let part_one (data : string list) : int =
  let rec find_max (l : string list) (sum : int) (maxi : int) =
    match l with
    | [] -> maxi
    | "" :: q -> find_max q 0 (max sum maxi)
    | t :: q -> find_max q (sum + int_of_string t) maxi
  in
  find_max data 0 0

let part_two (data : string list) : int =
  let rec calc_sums (l : string list) (sum : int) : int list =
    match l with
    | [] -> [ sum ]
    | "" :: q -> sum :: calc_sums q 0
    | t :: q -> calc_sums q (sum + int_of_string t)
  in
  calc_sums data 0 |> List.sort ( - ) |> List.rev |> Utils.take 3 |> Utils.sum

let solve (filename : string) : unit =
  let maxi = filename |> Utils.prepare_data in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one maxi in
  Format.printf "Part 1 : %d, ran in %fs%!\n" res time;
  let res, time = Utils.time part_two maxi in
  Format.printf "Part 2 : %d, ran in %fs%!\n" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve
