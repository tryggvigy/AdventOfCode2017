open Core.Std

module StringSet = Set.Make(String)

let sort_string str = str
  |> String.to_list_rev
  |> List.map ~f:Char.to_string
  |> List.sort ~cmp:String.ascending
  |> List.fold ~init:"" ~f:(^)

let verify_passphrase passphrase =
  let rec loop words found_phrases = match words with
    | [] -> 1
    | hd::tl when (StringSet.exists found_phrases ~f:(fun w -> w = hd)) -> 0
    | hd::tl -> loop tl (StringSet.add found_phrases hd)
  in
  loop (passphrase
    |> String.split ~on:' '
    |> List.map ~f:sort_string
  ) StringSet.empty

let () =
  let result = List.fold
    (Utils.lines_from_file (Filename.realpath "./input"))
    ~f:(fun acc line -> acc + (verify_passphrase line))
    ~init:0
  in
  print_int result
