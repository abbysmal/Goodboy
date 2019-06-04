open Goodboy
open Notty

type t = int * image

let region_str = function
  | `Boot -> "BOOT"
  | `Ech0 -> "ECH0"
  | `Ech1 -> "ECH1"
  | `Hram -> "HRAM"
  | `Io -> "I/0 "
  | `Oam -> "OAM "
  | `Rom0 -> "ROM0"
  | `Rom1 -> "ROM1"
  | `Sra0 -> "SRA0"
  | `Unk -> "****"
  | `Vra0 -> "VRAM"
  | `Wra0 -> "WRA0"
  | `Wra1 -> "WRA1"

let region_image r = I.string A.(fg red ++ st bold) (region_str r)

let make state =
  let s = Mmu.dump state in
  let l = List.map begin fun (i, r, ns) ->
    let region = region_image r |> I.hpad 0 1 in
    let i = I.string A.(fg (gray 8)) (Utils.show_hex_i8 i) |> I.hpad 0 1 in
    let ns = List.map
        (fun n ->
         Uint8.show_hex' n
         |>  I.string  A.(fg white) |> I.hpad 0 1
        ) ns |> I.hcat
    in
    I.(i <|> region <|> ns)
    end s
  in
  I.vcat l

let render (cursor, image) h =
  Window.with_window
    ~title:"hexdump"
    I.(vcrop cursor (h - 50) image)
