open Cmdliner

let cartridge =
  let doc = "cartridge to load in emulator" in
  Arg.(required & pos 0 (some string) None  & info [] ~doc )

let bootrom =
  let doc = "use this bootrom" in
  Arg.(value & opt (some string) None & info ["bootrom"; "b"] ~docv:"BREAKPOINTS" ~doc)

let info =
  let doc = "bork bork" in
  let man = [
    `S Manpage.s_bugs;
  ]
  in
  Term.info "GoodBoy" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man
