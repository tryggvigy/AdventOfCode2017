open Core.Std

module StringSet = Set.Make(String)

let verify_passphrase passphrase =
  let rec loop words found_phrases = match words with
    | [] -> 1
    | hd::tl when (StringSet.exists found_phrases ~f:(fun w -> w = hd)) -> 0
    | hd::tl -> loop tl (StringSet.add found_phrases hd)
  in
  loop (String.split passphrase ~on:' ') StringSet.empty

let () =
  let result = List.fold
    (Utils.lines_from_file (Filename.realpath "./input"))
    ~f:(fun acc -> fun line -> acc + (verify_passphrase line))
    ~init:0
  in
  print_int result
