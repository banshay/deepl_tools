open Lwt

let el_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt

let index data =
  let open Tyxml.Html in
  html
    (head
       (title @@ txt "Deepl GAN")
       [
         script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.9" ] (txt "");
         script ~a:[ a_src "https://cdn.tailwindcss.com" ] (txt "");
         (* link *)
         (*   ~rel:[ `Stylesheet ] *)
         (*   ~href: *)
         (*     "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" *)
         (*   (); *)
         (* script *)
         (*   ~a: *)
         (*     [ *)
         (*       a_src *)
         (*         "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"; *)
         (*     ] *)
         (*   (txt ""); *)
       ])
    (body ~a:[ a_class [ "h-full"; "bg-slate-800" ] ] [ data ])

let center_content content =
  let open Tyxml.Html in
  div
    ~a:
      [
        a_class [ "w-full"; "h-full"; "flex"; "items-center"; "justify-center" ];
      ]
    [
      div
        ~a:[ a_class [ "w-1/2"; "h-1/2"; "min-h-1/2"; "min-w-1/2" ] ]
        [ content ];
    ]

let col_container children =
  let open Tyxml.Html in
  div ~a:[ a_class [ "flex"; "flex-col"; "items-center" ] ] children

let image base64 =
  let open Tyxml.Html in
  img
    ~src:(Printf.sprintf "data:image/png;base64,%s" base64)
    ~alt:"Generated Image"
    ~a:[ a_id "gen_img"; a_class [ "w-[512px]"; "h-[512px]" ] ]
    ()

let slider () =
  let open Tyxml.Html in
  input
    ~a:
      [
        a_class
          [ "w-2/5"; "h-12"; "m-3"; "p-2"; "rounded-lg"; "shadow-lg"; "ring-2" ];
        a_input_type `Number;
        a_placeholder "Seed";
        a_value "15";
        a_name "seed";
        Unsafe.string_attrib "hx-get" "/img";
        Unsafe.string_attrib "hx-target" "#gen_img";
        Unsafe.string_attrib "hx-swap" "outerHTML";
      ]
    ()

let fetch_gen_img seed =
  let open Cohttp_lwt_unix in
  Client.get (Uri.of_string @@ Printf.sprintf "http://localhost:5000/%d" seed)
  >>= fun (resp, body) ->
  match resp.status with
  | `OK -> Cohttp_lwt.Body.to_string body
  | _ -> Lwt.fail_with "Failed to get image"

let gen_img seed = fetch_gen_img seed >|= image

let () =
  Dream.run ~port:9000 @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Lwt.bind (gen_img 1) (fun img ->
                 Dream.html @@ el_to_string @@ index @@ center_content
                 @@ col_container [ img; slider () ]));
         Dream.get "/img" (fun req ->
             let seed =
               match Dream.query req "seed" with
               | Some seed -> int_of_string seed
               | _ -> 1
             in
             Lwt.bind (gen_img seed) (fun img -> Dream.html @@ el_to_string img));
       ]
