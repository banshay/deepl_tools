open Cohttp_lwt_unix
open Lwt

let read_csv =
  let in_chan = open_in "input.csv" in
  let csv = Csv.of_channel in_chan in
  let data = Csv.input_all csv in
  close_in in_chan;
  List.flatten data
;;

let filename url =
  "../deepl_mapfiles/"
  ^
  match List.rev (String.split_on_char '/' url) with
  | [] -> assert false
  | hd :: _ -> hd
;;

let download i url =
  Printf.printf "Downloading %d %s...\n" i url;
  flush stdout;
  Client.get @@ Uri.of_string url
  >>= fun (_, body) ->
  body
  |> Cohttp_lwt.Body.to_string
  >|= fun body ->
  let oc = open_out_bin @@ filename url in
  output_string oc body;
  close_out oc;
  ()
;;

let process_file i url =
  let name = filename url in
  if Sys.file_exists name
  then Printf.printf "File %d %s exists, skipping...\n" i name
  else Lwt_main.run @@ download i url
;;

let () = List.iteri process_file read_csv
