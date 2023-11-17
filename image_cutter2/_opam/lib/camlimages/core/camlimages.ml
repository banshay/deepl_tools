# 1 "camlimages.cppo.ml"
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: camlimages.ml.in,v 1.3.2.1 2010/05/13 13:14:47 furuse Exp $ *)

  
# 19 "camlimages.cppo.ml"
let version = "5.0.0"


# 23 "camlimages.cppo.ml"
(* Supported libraries *)
let lib_gif      = 
# 24 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 
# 25 "camlimages.cppo.ml"
let lib_png      = 
# 25 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 
# 26 "camlimages.cppo.ml"
let lib_jpeg     = 
# 26 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 
# 27 "camlimages.cppo.ml"
let lib_tiff     = 
# 27 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 
# 28 "camlimages.cppo.ml"
let lib_freetype = 
# 28 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 
# 29 "camlimages.cppo.ml"
let lib_ps       = 
# 29 "camlimages.cppo.ml"
                    match  "true"  with "true" -> true | "false" -> false | _ -> assert false 
# 30 "camlimages.cppo.ml"
let lib_xpm      = 
# 30 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 
# 31 "camlimages.cppo.ml"
let lib_exif     = 
# 31 "camlimages.cppo.ml"
                    match  "false"  with "true" -> true | "false" -> false | _ -> assert false 

# 33 "camlimages.cppo.ml"
(* External files *)
# 35 "camlimages.cppo.ml"
let path_rgb_txt = Some 
# 35 "camlimages.cppo.ml"
                         "/etc/X11/rgb.txt" 
# 40 "camlimages.cppo.ml"
let path_gs = Some 
# 40 "camlimages.cppo.ml"
                    "/usr/bin/gs" 

# 45 "camlimages.cppo.ml"
(* They are written in ML, so always supported *)
let lib_ppm = true
let lib_bmp = true
let lib_xvthumb = true

(* Word size, used for the bitmap swapping memory management *)
let word_size = Sys.word_size / 8
