open Cmdliner

let cartridge =
  let doc = "cartridge to load in emulator" in
  Arg.(required & pos 0 (some string) None  & info [] ~doc )

let info =
  let doc = "tubes comes in various forms and shapes, but they are usually tubular" in
  let man = [
    `S Manpage.s_bugs;
  ]
  in
  Term.info "Tubularcamel" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man
