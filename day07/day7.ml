(* type inode = File of int * string | Directory of string * (inode list) *)

type registry = (string, int) Hashtbl.t
type stack = string Stack.t

let unique_name (dir : string) (depth : int) : string =
  dir ^ (string_of_int depth)

let parse_input (data : string list) : registry =
  let seen : registry = Hashtbl.create 50
  and history : stack = Stack.create () in
  let rec parse (l : string list) (depth : int) : int =
    match l with
    | [] -> depth
    | line :: rest -> (
        let n = String.length line in
        if String.starts_with ~prefix:"$ " line then (
          (* Si on commence par $, c'est une commande *)
          let command = String.sub line 2 (n - 2) in
          match command with
          | "ls" ->
              parse rest depth
              (* Commence par un dollar et n'est pas `ls`, donc c'est `cd $dir` *)
          | _ ->
              let dir = String.sub command 3 (n - 5) in
              let new_depth = 
              if dir = ".." then (
                let exited_dir = unique_name (Stack.pop history) depth in
                let parent_dir = unique_name (Stack.top history) (depth - 1) in
                (* Printf.printf
                  "Exited dir %s, going back to dir %s. %s had size %d\n"
                  exited_dir parent_dir exited_dir
                  (Hashtbl.find seen exited_dir); *)
                Hashtbl.replace seen parent_dir
                  (Hashtbl.find seen parent_dir + Hashtbl.find seen exited_dir);
                  depth - 1)
              else (Stack.push dir history; depth + 1) in
              parse rest new_depth)
        else
          (* Commence pas par un dollar, c'est un dossier ou un fichier après `ls` *)
          (* On split par espace et on discrimine selon le premier élément *)
          let elems = String.split_on_char ' ' line in
          match elems with
          | [ "dir"; dir ] ->
              Hashtbl.add seen (unique_name dir (depth + 1)) 0;
              parse rest depth
          | [ size; _ ] ->
              let current = unique_name (Stack.top history) depth in

              (* Printf.printf "Currently in %s at depth %d\n" current depth; *)

              assert (Hashtbl.mem seen current);
              Hashtbl.replace seen current
                (Hashtbl.find seen current + int_of_string size);
              parse rest depth
          | _ -> failwith "Malformed data")
  and exit_all (max_depth : int) : unit =
    if not (Stack.is_empty history) then
      let exited_dir = unique_name (Stack.pop history) max_depth in
      if not (Stack.is_empty history) then (
        let parent_dir = unique_name (Stack.top history) (max_depth - 1) in
        (* Printf.printf "Exited dir %s, going back to dir %s. %s had size %d\n" 
          exited_dir parent_dir exited_dir
          (Hashtbl.find seen exited_dir); *)
        Hashtbl.replace seen parent_dir
          (Hashtbl.find seen parent_dir + Hashtbl.find seen exited_dir);
        exit_all (max_depth - 1))
  in
  Hashtbl.add seen "/0" 0;
  parse data (-1) |> exit_all;
  seen

let part_one (data : string list) : int =
  data |> parse_input |> Hashtbl.to_seq_values
  |> Seq.filter (fun x -> x <= 100000)
  |> Seq.fold_left ( + ) 0

let part_two (data : string list) : int =
  let map = parse_input data in
    let used_space = Hashtbl.find map "/0" in 
      let unused = 70000000 - used_space in 
        let needed = 30000000 - unused in
        (* Printf.printf "Used space : %d, unused space : %d, needed : %d\n" used_space unused needed; *)
        map |> Hashtbl.to_seq_values |> Seq.filter (fun (x) -> x >= needed) |> List.of_seq |> Utils.list_min

let solve (filename : string) : unit =
  let data = filename |> Utils.prepare_data |> List.filter (fun (l) -> l <> "") in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one data in
  Format.printf "Part 1 : %d, ran in %fs\n%!" res time;
  let res, time = Utils.time part_two data in
  Format.printf "Part 2 : %d, ran in %fs\n%!" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve
