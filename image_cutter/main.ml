let dir = ref ""
let size = ref 0
let output = ref "output"
let prefix = ref "test"

let speclist =
  [ "-s", Arg.Set_int size, "Size of the individual cuts"
  ; "-o", Arg.Set_string output, "Output directory"
  ; "-p", Arg.Set_string prefix, "Names of the output files"
  ]
;;

let anon_fun input = dir := input

let crop path x y out_file =
  let size = size.contents in
  let input =
    Unix.open_process_in
    @@ Printf.sprintf "convert %s -crop %dx%d+%d+%d %s" path x y size size out_file
  in
  close_in input
;;

let slice_image (file, width, height) =
  let size = size.contents in
  let output = output.contents in
  let prefix = prefix.contents in
  let rec loop x y idx =
    if y < height
    then
      if x < width
      then (
        crop file x y (Printf.sprintf "%s/%s%d.tiff" output prefix idx);
        loop (x + size) y (idx + 1))
      else loop 0 (y + size) idx
  in
  loop 0 0 0
;;

let rec read_dir dir_handle files =
  try
    let file = Unix.readdir dir_handle in
    read_dir dir_handle (file :: files)
  with
  | End_of_file -> files
;;

let identify_image file =
  let dir = dir.contents in
  let input =
    Unix.open_process_in @@ Printf.sprintf "identify -format \"%%w %%h\" %s/%s" dir file
  in
  let shape = input_line input in 
  let () = close_in input in 
  let () = print_endline shape in 
  file, 0, 0
;;

let () =
  Arg.parse speclist anon_fun "Just pass a directory to cut up";
  read_dir (Unix.opendir dir.contents) []
  |> List.map identify_image
  |> List.iter slice_image
;;
