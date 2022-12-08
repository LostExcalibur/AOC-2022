let nb_points (moi : string) (adv : string) : int =
  match moi with
  | "X" -> (
      1
      + match adv with "A" -> 3 | "B" -> 0 | "C" -> 6 | _ -> failwith "protu")
  | "Y" -> (
      2
      + match adv with "A" -> 6 | "B" -> 3 | "C" -> 0 | _ -> failwith "protu")
  | "Z" -> (
      3
      + match adv with "A" -> 0 | "B" -> 6 | "C" -> 3 | _ -> failwith "protu")
  | _ -> failwith "Noulle"

let nb_points_result (res : string) (adv : string) : int =
  match res with
  | "X" -> (
      match adv with
      | "A" -> nb_points "Z" adv
      | "B" -> nb_points "X" adv
      | "C" -> nb_points "Y" adv
      | _ -> failwith "protu")
  | "Y" -> (
      match adv with
      | "A" -> nb_points "X" adv
      | "B" -> nb_points "Y" adv
      | "C" -> nb_points "Z" adv
      | _ -> failwith "protu")
  | "Z" -> (
      match adv with
      | "A" -> nb_points "Y" adv
      | "B" -> nb_points "Z" adv
      | "C" -> nb_points "X" adv
      | _ -> failwith "protu")
  | _ -> failwith "Noulle"

let parse_ligne_one (ligne : string) : int =
  let[@warning "-partial-match"] [ lui; moi ] =
    String.split_on_char ' ' ligne
  in
  nb_points moi lui

let parse_ligne_two (ligne : string) : int =
  let[@warning "-partial-match"] [ lui; moi ] =
    String.split_on_char ' ' ligne
  in
  nb_points_result moi lui

let part_one (data : string list) : int =
  data |> List.map parse_ligne_one |> Utils.sum

let part_two (data : string list) : int =
  data |> List.map parse_ligne_two |> Utils.sum

let solve (filename : string) : unit =
  let maxi = filename |> Utils.prepare_data in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one maxi in
  Format.printf "Part 1 : %d, ran in %fs%!\n" res time;
  let res, time = Utils.time part_two maxi in
  Format.printf "Part 2 : %d, ran in %fs%!\n" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve