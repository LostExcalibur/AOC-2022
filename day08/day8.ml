exception Trouve
exception Break

let part_one (data : string list) : int =
  let m =
    Array.of_list (List.map (fun x -> Array.of_list (Utils.split_chars x)) data)
  in
  let n = Array.length m in
  let res = Array.make_matrix n n false in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      try
        let ok = ref true in
        for k = 0 to j - 1 do
          if m.(i).(k) >= m.(i).(j) then ok := false
        done;
        if !ok then raise Trouve;
        let ok = ref true in
        for k = j + 1 to n - 1 do
          if m.(i).(k) >= m.(i).(j) then ok := false
        done;
        if !ok then raise Trouve;

        let ok = ref true in
        for k = 0 to i - 1 do
          if m.(k).(j) >= m.(i).(j) then ok := false
        done;
        if !ok then raise Trouve;

        let ok = ref true in
        for k = i + 1 to n - 1 do
          if m.(k).(j) >= m.(i).(j) then ok := false
        done;
        if !ok then raise Trouve
      with Trouve -> res.(i).(j) <- true
    done
  done;

  List.length
    (List.filter
       (fun x -> x)
       (List.flatten (List.map Array.to_list (Array.to_list res))))

let part_two (data : string list) : int =
  ignore data;
  let m =
    Array.of_list (List.map (fun x -> Array.of_list (Utils.split_chars x)) data)
  in
  let n = Array.length m in
  let res = Array.make_matrix n n 1 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let count = ref 0 in
      (try
         for k = j - 1 downto 0 do
           incr count;
           if m.(i).(k) >= m.(i).(j) then raise Break
         done
       with Break -> ());
      res.(i).(j) <- res.(i).(j) * !count;

      let count = ref 0 in
      (try
         for k = j + 1 to n - 1 do
           incr count;
           if m.(i).(k) >= m.(i).(j) then raise Break
         done
       with Break -> ());
      res.(i).(j) <- res.(i).(j) * !count;

      let count = ref 0 in
      (try
         for k = i - 1 downto 0 do
           incr count;
           if m.(k).(j) >= m.(i).(j) then raise Break
         done
       with Break -> ());
      res.(i).(j) <- res.(i).(j) * !count;

      let count = ref 0 in
      (try
         for k = i + 1 to n - 1 do
           incr count;
           if m.(k).(j) >= m.(i).(j) then raise Break
         done
       with Break -> ());
      res.(i).(j) <- res.(i).(j) * !count
    done
  done;

  List.fold_left max min_int
    (List.flatten (List.map Array.to_list (Array.to_list res)))

let solve (filename : string) : unit =
  let data = filename |> Utils.prepare_data in
  Format.printf "Input File : %s\n" filename;
  let res, time = Utils.time part_one data in
  Format.printf "Part 1 : %d, ran in %fs%!\n" res time;
  let res, time = Utils.time part_two data in
  Format.printf "Part 2 : %d, ran in %fs%!\n" res time

let () = Sys.argv |> Array.to_list |> List.tl |> List.iter solve
