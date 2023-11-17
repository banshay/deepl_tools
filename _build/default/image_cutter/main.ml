open Bimage

let slice_image file output_prefix size =
  match Bimage_unix.Stb.read f32 rgb file with
  | Ok img ->
    let width, height, _ = Image.shape img in
    let save_slice x y idx =
      print_endline @@ Printf.sprintf "Croping x:%d y:%d for image %d" x y idx;
      let slice = Image.crop img ~x ~y ~width:size ~height:size in
      let output_file =
        Printf.sprintf "../deepl_map_tiles/%s_%d.tiff" output_prefix idx
      in
      Bimage_unix.Magick.write output_file slice;
      print_endline @@ Printf.sprintf "Wrote file %s" output_file
    in
    let rec loop x y idx =
      if y < height
      then
        if x < width
        then (
          save_slice x y idx;
          loop (x + size) y (idx + 1))
        else loop 0 (y + size) idx
    in
    loop 0 0 0
  | Error e -> Printf.eprintf "Failed to read image: %s\n" (Error.to_string e)
;;

let () =
  slice_image
    "../deepl_mapfiles/swiss-map-raster10_2023_1032-3_krel_0.5_2056.tif"
    "test"
    1000
;;
