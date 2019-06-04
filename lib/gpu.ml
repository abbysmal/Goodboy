open Uint8

type uint8 = Uint8.t
type color = [ `White | `Black | `Light_gray | `Dark_gray ]

type t = {
  mutable clock : int;
  mutable lcd_register : uint8;
  mutable control_register : uint8;
  mutable scr_y : uint8;
  mutable scr_x : uint8;
  mutable ly : uint8;
  mutable lyc : uint8;
  mutable win_x : uint8;
  mutable win_y : uint8;
  mutable bg_pal  : uint8;
  mutable obj_pal0 : uint8;
  mutable obj_pal1 : uint8;
  mutable dma_transfer : uint8;
  mutable dma_transfer_request : bool;
  mutable retrace_ly : int;
  mutable redraw : bool;
  mutable dma_clock : int;
  framebuffer : color array array;
}

let make () = {
  clock = 0;
  dma_clock = 0;
  dma_transfer_request = false;
  redraw = false;
  lcd_register = Uint8.chr (0x80);
  control_register = Uint8.zero;
  ly = Uint8.zero;
  lyc = Uint8.zero;
  scr_y = Uint8.zero;
  scr_x = Uint8.zero;
  win_x = Uint8.zero;
  win_y = Uint8.zero;
  bg_pal = Uint8.zero;
  obj_pal0 = Uint8.zero;
  obj_pal1 = Uint8.zero;
  dma_transfer = Uint8.zero;
  framebuffer = Array.make_matrix 160 144 `Black;
  retrace_ly = 0;
}

let put_pixel ~x ~y ~color { framebuffer; _ } =
  if y >= 144 || x >= 160 then
    ()
  else
    framebuffer.(x).(y) <- color

module Control = struct

  let lcd_on = 7
  let win_tile_map_display = 6
  let window_display_on = 5
  let bg_window_tile_data_select = 4
  let bg_tile_map_display_select = 3
  let obj_size = 2
  let obj_on = 1
  let display_on = 0

  let is_flag_set f { control_register; _ } = is_bit_set control_register f
  let set_flag f { control_register; _ } = set_bit control_register f

  let get_window_tile_map_display { control_register; _ } =
    match is_bit_set control_register win_tile_map_display with
    | false -> 0x9800, 0x9BFF
    | true -> 0x9C00, 0x9FFF

  let bg_tile_data_select { control_register; _ } =
    match is_bit_set control_register bg_window_tile_data_select with
    | false -> 0x9000
    | true -> 0x8000

  let bg_layout_select { control_register; _ } =
    match  is_bit_set control_register bg_tile_map_display_select with
    | false -> 0x9800
    | true -> 0x9C00

  let get_obj_size { control_register; _ } =
    match is_bit_set control_register obj_size with
    | false -> `Eight
    | true -> `Sixteen

end

module Lcd = struct

  let lyc_ly_irq = 6
  let oam_irq  = 5
  let vblank_irq = 4
  let hblank_irq = 3
  let lyc_ly_flag = 2

  let is_flag_set f { lcd_register; _ } = is_bit_set lcd_register f
  let set_flag f { lcd_register; _ } = set_bit lcd_register f
  let unset_flag f { lcd_register; _ } = unset_bit lcd_register f

  let get_mode_flag { lcd_register; _ } =
    let mode = Uint8.(lcd_register land (chr 0b11)) in
    match Uint8.code mode with
    | 0b00 -> `Hblank
    | 0b01 -> `Vblank
    | 0b10 -> `OAM_ram
    | 0b11 -> `Lcd_transfer
    | _ -> assert false

  let set_mode_flag t mode =
    let open Uint8 in
    let lcd_register =
      match mode with
      | `Hblank -> unset_bit' 0 t.lcd_register |> unset_bit' 1
      | `Vblank -> unset_bit' 1 t.lcd_register |> set_bit' 0
      | `OAM_ram -> set_bit' 1 t.lcd_register |> unset_bit' 0 
      | `Lcd_transfer -> set_bit' 0 t.lcd_register |> set_bit' 1
    in
    let r = { t with lcd_register; } in
    r

end


let map_color c =
  match Char.code c with
  | 0 -> `White
  | 1 -> `Light_gray
  | 2 -> `Dark_gray
  | 3 -> `Black
  | _ -> assert false

let get_palette palette =
  let color_4 = get_n_bits palette (Char.chr 2) |> map_color in
  let color_3 = get_n_bits (palette lsr Char.chr 2) (Char.chr 2) |> map_color in
  let color_2 = get_n_bits (palette lsr Char.chr 4) (Char.chr 2) |> map_color in
  let color_1 = get_n_bits (palette lsr Char.chr 6) (Char.chr 2) |> map_color in
  color_1, color_2, color_3, color_4

type tile = color list
