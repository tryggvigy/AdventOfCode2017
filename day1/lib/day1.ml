open Core.Std

let solveCaptcha digits =
  let rec loop ls sum = match ls with
    | hd::[] when (List.hd_exn digits) = hd -> sum + hd
    | hd::sd::tl when hd = sd               -> loop (sd::tl) (sum + hd)
    | hd::tl                                -> loop tl sum
    | _                                     -> sum
  in
  loop digits 0
