open Notty

let top_left = I.string A.(fg blue) "┏"
let top_right = I.string A.(fg blue) "┓"
let bottom_right = I.string A.(fg blue) "┛"
let bottom_left = I.string A.(fg blue) "┗"

let top = I.uchar A.(fg blue) (Uchar.of_int 0x2501)
let side = I.uchar A.(fg blue) (Uchar.of_int 0x2503)
let left = I.string A.(fg blue) "┫"
let right = I.string A.(fg blue) "┣"

type t = {
  title : string;
  cursor : int;
  image : image;
  refresh : Goodboy.State.t -> image;
}

let make ~cursor ~title ~refresh state =
  let image = refresh state in
  {
    cursor;
    image;
    refresh;
    title;
  }

let down_cursor t i =
  if t.cursor - i < 0 then
    { t with cursor = 0; }
  else
    { t with cursor = t.cursor - i }

let up_cursor t i =
  let image_h = I.height t.image in
  if t.cursor + i > image_h then
    { t with cursor = image_h }
  else
    { t with cursor = t.cursor + i }

let set_cursor t i = { t with cursor = i; }

let refresh t state = { t with image = t.refresh state; }

let with_window ~title i =
  let w = I.width i in
  let h = I.height i in
  let title =
    let title = I.string A.(fg yellow) title in
    I.(
      (hsnap ~align:`Middle w (left <|> title <|> right))
      </>
      (top_left <|> top w 1 <|> top_right)
    )
  in
  let bottom =
    I.(
      (bottom_left <|> top w 1 <|> bottom_right)
    )
  in
  let left = side 1 (h + 1) |> I.hpad 0 w in
  let right = side 1 (h + 1) in
  let win = I.(
      (title |> vpad 0 h <-> bottom) </> (left <|> right)
    )
  in
  I.(
    (hpad 1 0 i |> vpad 1 2)
    </>
    win
  )

let render ~window_h { image; cursor; title; _ } =
  let image_h = I.height image in
  let crop_top =
    if cursor < window_h then
      0
    else
      cursor - window_h
  in
  let crop_bottom =
    if cursor + window_h > image_h then
      0
    else
      image_h - crop_top - window_h
  in
  let i = I.(vcrop crop_top crop_bottom image |> vsnap window_h) in
  with_window ~title i
