let run_command work_dir command =
  let cmd =
    match work_dir with
    | "" | "./" | "." -> command
    | _ -> Printf.sprintf "cd %s && %s" work_dir command
  in
  let chan = Unix.open_process_in cmd in
  let rec read_out acc =
    try
      let line = input_line chan in
      read_out (line :: acc)
    with End_of_file -> List.rev acc
  in
  let result = read_out [] in
  close_in chan;
  result

let pwd work_dir =
  let r = run_command work_dir "pwd" in
  Printf.printf "%s" @@ String.concat "\n" r;
  r

let ls ?filter dir =
  match filter with
  | None -> run_command dir (Printf.sprintf "ls -p | grep -v /")
  | Some filter ->
      run_command dir (Printf.sprintf "ls -p %s* | grep -v /" filter)

let clear_unused_pages dir file =
  let _ = Printf.sprintf {|convert "%s[0]" %s|} file file |> run_command dir in
  ()

let get_image_shape dir file =
  let cmd = Printf.sprintf "identify -format \"%%w %%h|\" %s" file in
  let shape =
    match run_command dir cmd with
    | [] ->
        Printf.eprintf "Cannot read shape of image %s in directory %s\n" file
          dir;
        assert false
    | shape :: _ -> shape
  in
  let width, height =
    match String.split_on_char '|' shape with
    | [] ->
        raise
          (Invalid_argument
             "Not a .tif as expected. should have multiple rasters")
    | x :: _ -> (
        match String.split_on_char ' ' x with
        | [] ->
            let error_msg =
              Printf.sprintf "Expected format 'width height'. Got %s" x
            in
            raise (Invalid_argument error_msg)
        | width :: height :: _ -> (int_of_string width, int_of_string height)
        | _ -> raise (Invalid_argument "Not enough elements in list"))
  in
  let () = print_endline shape in
  (width, height)

let slice_image dir size prefix output (file, width, height) =
  let _ = run_command dir ("mkdir -p " ^ output) in
  let crop_line temp_name x y idx file =
    let file_name =
      match String.split_on_char '.' file with [] -> prefix | n :: _ -> n
    in
    Printf.sprintf "\\( mpr:%s -crop %dx%d+%d+%d +write %s/%s_%d.tif \\) \\\n"
      temp_name size size x y output file_name idx
  in

  let crop file temp_name crop_block =
    let cmd =
      Printf.sprintf
        {|
        convert %s \
        -write mpr:%s \
        +delete \
        -respect-parentheses \
        %s \
        NULL:
        end
    |}
        file temp_name crop_block
    in
    (* let () = print_endline @@ "Running command: " ^ cmd in *)
    cmd
  in

  let temp_name = Printf.sprintf "swiss_%s" file in
  let rec loop x y idx block =
    if y < height then
      if x < width then
        block
        ^ crop_line temp_name x y idx file
        ^ loop (x + size) y (idx + 1) block
      else loop 0 (y + size) idx block
    else block
  in
  let crop_block = loop 0 0 0 "" in
  let _ = crop file temp_name crop_block |> run_command dir in
  ()

let rm dir files =
  let delete_str = String.concat " " files in
  if String.trim delete_str = String.empty |> not then (
    let cmd = Printf.sprintf "rm -rdf %s" delete_str in
    Printf.printf "%s\n" cmd;
    let _ = run_command dir cmd in
    ())

let get_black_threshold dir file =
  let cmd =
    Printf.sprintf "convert %s -format %%c histogram:info:- | rg black" file
  in
  match run_command dir cmd with
  | [ line ] ->
      let open Re in
      let grp = Re.exec (compile (rep1 digit)) line in
      let black_pixels = int_of_string @@ Re.Group.get grp 0 in
      (* Printf.printf "%d for image %s\n" black_pixels file; *)
      black_pixels
  | _ ->
      Printf.eprintf "Cannot get black pixels of picture %s in directory %s\n"
        file dir;
      0
