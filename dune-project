(lang dune 3.16)

(name zamlbie)

(generate_opam_files true)

(source
 (github yoangau/zamlbie))

(authors "Yoan Gauthier" "David Isaac Garnier")

(maintainers "Yoan Gauthier" "David Isaac Garnier")

(license LICENSE)

(documentation https://github.com/yoangau/zamlbie)

(package
 (name zamlbie)
 (synopsis "CLI zombie infectionator game in ocaml")
 (description "CLI zombie infectionator game in ocaml")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  cmdliner
  notty
  base
  dream
  websocket
  websocket-lwt-unix
  cohttp-lwt-unix
  atdgen
  lwt
  lwt_ppx)
 (tags (gamedev)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
