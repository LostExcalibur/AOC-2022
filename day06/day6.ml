exception Found of int

let unique (s : string) : bool =
  let n = String.length s in
  try
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        if s.[i] = s.[j] then raise (Found 0)
      done
    done;
    true
  with Found _ -> false

let find_unique_id (data : string) (size : int) : int =
  let n = String.length data in
  try
    for i = 0 to n - size - 1 do
      if unique (String.sub data i size) then raise (Found (i + size))
    done;
    failwith "Invalid data"
  with Found i -> i

let part_one (data : string) : int = find_unique_id data 4
let part_two (data : string) : int = find_unique_id data 14

let solve (filename : string) : unit =
  let data = filename |> Utils.prepare_data |> List.hd in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one data in
  Format.printf "Part 1 : %d, ran in %fs\n%!" res time;
  let res, time = Utils.time part_two data in
  Format.printf "Part 2 : %d, ran in %fs\n%!" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve