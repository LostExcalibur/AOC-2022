let width = 9

let parse_state_line (line : string) (current : char list array) : unit =
  for i = 0 to width - 1 do
    let c = line.[(4 * i) + 1] in
    if c <> ' ' then current.(i) <- c :: current.(i)
  done

let rec parse_file (data : string list) :
    char Stack.t array * (int * int * int) list =
  let rec read_until_empty_line (l : string list) (acc : char list array) =
    match l with
    | _ :: "" :: q ->
        ( Array.map (fun x -> x |> List.to_seq |> Stack.of_seq) acc,
          parse_instrs q )
    | t :: q ->
        parse_state_line t acc;
        read_until_empty_line q acc
    | [] -> failwith "Bad parsing"
  in
  read_until_empty_line data (Array.make width [])

and parse_instrs (data : string list) : (int * int * int) list =
  match data with
  | [] -> []
  | t :: q ->
      let line = String.split_on_char ' ' t in
      ( int_of_string (List.nth line 1),
        int_of_string (List.nth line 3),
        int_of_string (List.nth line 5) )
      :: parse_instrs q

let apply_one_by_one (state : char Stack.t array)
    ((count, from, dest) : int * int * int) : unit =
  for _ = 0 to count - 1 do
    let c = Stack.pop state.(from - 1) in
    Stack.push c state.(dest - 1)
  done

let top_crates (state : char Stack.t array) : string =
  String.init width (fun x -> Stack.top state.(x))

let part_one (data : string list) : string =
  let start_state, instr_list = parse_file data in
  instr_list |> List.iter (apply_one_by_one start_state);
  top_crates start_state

let apply_all_at_one (state : char Stack.t array)
    ((count, from, dest) : int * int * int) : unit =
  let temp = Stack.create () in
  for _ = 0 to count - 1 do
    let c = Stack.pop state.(from - 1) in
    Stack.push c temp
  done;
  for _ = 0 to count - 1 do
    let c = Stack.pop temp in
    Stack.push c state.(dest - 1)
  done

let part_two (data : string list) : string =
  let start_state, instr_list = parse_file data in
  instr_list |> List.iter (apply_all_at_one start_state);
  top_crates start_state

let solve (filename : string) : unit =
  (* On ne veut pas trim ici ! *)
  let maxi = filename |> Utils.read_file |> String.split_on_char '\n' in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one maxi in
  Format.printf "Part 1 : %s, ran in %fs\n%!" res time;
  let res, time = Utils.time part_two maxi in
  Format.printf "Part 2 : %s, ran in %fs\n%!" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve
