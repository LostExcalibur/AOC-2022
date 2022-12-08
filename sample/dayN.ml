let part_one (data : string list) : int =
  ignore data;
  failwith "TODO part one"

let part_two (data : string list) : int =
  ignore data;
  failwith "TODO part two"

let solve (filename : string) : unit =
  let maxi = filename |> Utils.prepare_data in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one maxi in
  Format.printf "Part 1 : %d, ran in %fs%!\n" res time;
  let res, time = Utils.time part_two maxi in
  Format.printf "Part 2 : %d, ran in %fs%!\n" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve
