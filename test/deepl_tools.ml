let run_command_test () =
  Alcotest.(check (list string))
    "pwd"
    [ "/home/banshay/projects/deepl_tools/_build/default/test" ]
    (Lib.Magick.pwd "")

let ls_test () =
  Alcotest.(check (list string))
    "ls"
    [ "sample.tif"; "sample2.tif" ]
    (Lib.Magick.ls "../../../examples/cutter")

let () =
  let open Alcotest in
  run "Magick"
    [
      ( "Utils",
        [
          test_case "pwd" `Quick run_command_test; test_case "ls" `Quick ls_test;
        ] );
    ]
