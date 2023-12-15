let dir = ref ""
let size = ref 1000
let output = ref "output"
let prefix = ref "test"
let black_threshold = ref 80000
let processed_files = ref []
let m_processed = Mutex.create ()

module T = Domainslib.Task

let speclist =
  [
    ("-s", Arg.Set_int size, "Size of the individual cuts");
    ("-o", Arg.Set_string output, "Output directory");
    ("-p", Arg.Set_string prefix, "Names of the output files");
  ]

let anon_fun input = dir := input

let load_processed () =
  let pf = Printf.sprintf "%s/%s/processed" !dir !output in
  match Sys.file_exists pf with
  | false -> []
  | true ->
      let ic = open_in pf in
      let rec read acc =
        try
          let r = input_line ic in
          read (r :: acc)
        with End_of_file -> List.rev acc
      in
      let result = read [] in
      close_in ic;
      result

let write_processed processed_file =
  Mutex.lock m_processed;

  let out_file = Printf.sprintf "%s/%s/processed" !dir !output in

  let oc = open_out_gen [ Open_creat; Open_text; Open_append ] 0o640 out_file in
  output_string oc (Printf.sprintf "%s\n" processed_file);
  flush oc;
  close_out oc;
  Mutex.unlock m_processed;
  ()

let identify_image file =
  let width, height = Lib.Magick.get_image_shape !dir file in
  (file, width, height)

let process_file file =
  identify_image file |> Lib.Magick.slice_image !dir !size !prefix !output;

  let out_dir = Printf.sprintf "%s/%s" !dir !output in
  let file_prefix =
    match String.split_on_char '.' file with
    | [] ->
        Printf.eprintf "Not a propper file name %s" file;
        assert false
    | f :: _ -> f
  in
  let _ =
    (* Printf.printf "ls %s/%s*\n" out_dir file_prefix; *)
    Lib.Magick.ls out_dir ~filter:file_prefix
    |> List.filter_map (fun file ->
           match Lib.Magick.get_black_threshold out_dir file with
           | x when x < !black_threshold -> Some file
           | _ -> None)
    |> Lib.Magick.rm out_dir
  in
  write_processed file

let () =
  Arg.parse speclist anon_fun "Just pass a directory to cut up";

  processed_files := load_processed ();

  let pool = T.setup_pool ~num_domains:4 () in

  (* let _ = Lib.Magick.rm !dir [ !output ] in *)
  T.run pool (fun _ ->
      Lib.Magick.ls !dir
      |> List.filter_map (fun file ->
             match List.mem file !processed_files with
             | true ->
                 print_endline
                   (Printf.sprintf "Already processed %s, ignoring" file);
                 None
             | false ->
                 print_endline (Printf.sprintf "Processing %s" file);
                 Some
                   (T.async pool (fun () ->
                        Lib.Magick.clear_unused_pages !dir file;
                        process_file file)))
      |> List.iter (T.await pool));
  ()
