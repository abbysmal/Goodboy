open Notty
    
let help_window =
  let keys = [
    I.string A.(fg white) "P";
    I.string A.(fg white) "R";
    I.string A.(fg white) "S";
    I.string A.(fg white) "F1";
    I.string A.(fg white) "F2";
  ] |> I.vcat
  in
  let descr = [
    I.string A.(fg white) "Pause emulation";
    I.string A.(fg white) "Run";
    I.string A.(fg white) "Single step";
    I.string A.(fg white) "Switch to game window";
  I.string A.(fg white) "Switch to debugger view";
  ] |> I.vcat
  in
  I.(keys |> hpad 1 4 <|> descr) |> Window.with_window ~title:"halp"
