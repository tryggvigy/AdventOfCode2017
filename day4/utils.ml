open Core.Std

let lines_from_file filename =
  let file = In_channel.create filename in
  protect ~f:(fun () -> In_channel.input_lines file)
    ~finally:(fun () -> In_channel.close file)
