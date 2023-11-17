(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

let config_file = "/home/banshay/projects/deepl_tools/image_cutter2/_opam/lib/findlib.conf";;

let ocaml_stdlib = "/home/banshay/projects/deepl_tools/image_cutter2/_opam/lib/ocaml";;

let ocaml_ldconf = Filename.concat ocaml_stdlib "ld.conf";;

let ocaml_has_autolinking = true;;

let libexec_name = "stublibs";;

let system = "linux";;
(* - "mingw", "mingw64", "win32", "cygwin", "linux_elf", ... *)

let dll_suffix =
  match Sys.os_type with
    | "Unix"  | "BeOS"   -> ".so"
    | "Win32" | "Cygwin" -> ".dll"
    | "MacOS"            -> ""        (* don't know *)
    | _ -> failwith "Unknown Sys.os_type"
;;
