open Gpu

open State

let clear_bg gpu =
  for pixel = 0 to 159 do
    Gpu.put_pixel ~x:pixel ~y:(Uint8.code gpu.ly) ~color:`White gpu
  done

let render_line_bg ({ gpu; _ } as t) =
  match Control.(is_flag_set display_on gpu) with
  | false -> clear_bg gpu
  | true  ->

  let c1, c2, c3, c4 =
    let open Uint8 in
      (gpu.bg_pal) land (chr 0b11),
      (gpu.bg_pal lsr Char.chr 2) land (chr 0b11),
      (gpu.bg_pal lsr Char.chr 4) land (chr 0b11),
      (gpu.bg_pal lsr Char.chr 6) land (chr 0b11)
  in
  let tile_data = Control.bg_tile_data_select gpu in
  let tile_number_map = Control.bg_layout_select gpu in
  let ly = Uint8.code gpu.ly in
  let scr_y = Uint8.code gpu.scr_y in
  let scr_x = Uint8.code gpu.scr_x in

  let tile_y = ((scr_y + ly) / 8) mod 32 in
  let tile_y_off = (scr_y + ly) mod 8 in

  for pixel=0 to 159 do

    let tile_x = (((scr_x + pixel) / 8) mod 32) land Uint16.max_int in
    let tile_num = Mmu.get_n t ((tile_number_map + (tile_y * 32) + tile_x) land Uint16.max_int) in

    let tile_data_ptr =
      if tile_data != 0x9000 then
        (tile_data + (Char.code tile_num) * 0x10) land Uint16.max_int
      else
        (tile_data + (Uint8.to_signed tile_num) * 0x10) land Uint16.max_int
    in
    let tile_data_ptr = tile_data_ptr + (tile_y_off * 2) land Uint16.max_int in
    let b1 = Mmu.get_n t (tile_data_ptr) in
    let b2 = Mmu.get_n t (tile_data_ptr + 1) in
    let col_bit = (pixel + scr_x) mod 8 in
    let col_bit = col_bit - 7 in
    let col_bit = col_bit * (-1) in
    let col_num = if Uint8.is_bit_set b2 col_bit then Uint8.one else Uint8.zero in
    let col_num = Uint8.(col_num lsl one) in
    let col_num = Uint8.(col_num lor (if Uint8.is_bit_set b1 col_bit then one else zero)) in
    let color = match Uint8.code col_num with
      | 0x0 -> c1
      | 0x1 -> c2
      | 0x2 -> c3
      | 0x3 -> c4
      | _ -> assert false
    in
    let color = Gpu.map_color color in
    Gpu.put_pixel ~x:pixel ~y:(Char.code gpu.ly) ~color gpu

  done

let render_line_window ({ gpu; _ } as t) =
  let open Uint8 in

  match Control.(is_flag_set window_display_on gpu) with
  | false -> ()
  | true  ->

    let win_y = code gpu.ly - code gpu.win_y in
  if win_y < 0 then
    ()
  else begin
    let c1, c2, c3, c4 =
      let open Uint8 in
      (gpu.bg_pal) land (chr 0b11),
      (gpu.bg_pal lsr Char.chr 2) land (chr 0b11),
      (gpu.bg_pal lsr Char.chr 4) land (chr 0b11),
      (gpu.bg_pal lsr Char.chr 6) land (chr 0b11)
    in
    let tile_number_map, _ = Control.get_window_tile_map_display gpu in
    let tile_data = Control.bg_tile_data_select gpu in
    let tile_y = div (chr win_y) (chr 8) in
    let tile_y_offset = win_y mod 8 |> chr in
    let win_x = (code gpu.win_x) - 7 in
    for x = 0 to 159 do
      if x < win_x then begin
        ()
      end
      else begin
        let tile_x = Infix.((chr x - chr win_x) / chr 8) in
        let tile_number =
          Mmu.get_n t (tile_number_map + (code tile_y * 32) + code tile_x)
        in
        let tile_data_ptr =
          if tile_data != 0x9000 then
            (tile_data + (Char.code tile_number) * 0x10)
          else
            (tile_data + (Uint8.to_signed tile_number) * 0x10)
        in
        let tile_data_ptr = tile_data_ptr + (code tile_y_offset * 2) in
        let b1 = Mmu.get_n t (tile_data_ptr) in
        let b2 = Mmu.get_n t (tile_data_ptr + 1) in
        let bit = 7 - x mod 8 in
        let pLo = if is_bit_set b1 bit then 0x01 else 0x00 in
        let pHo = if is_bit_set b2 bit then 0x02 else 0x00 in
        let col_num = pLo + pHo in
        let color = match col_num with
          | 0x0 -> c1
          | 0x1 -> c2
          | 0x2 -> c3
          | 0x3 -> c4
          | _ -> assert false
        in
        let color = Gpu.map_color color in
        Gpu.put_pixel ~x ~y:(Char.code gpu.ly) ~color gpu
      end
    done
  end

let render_line_obj ({ gpu; _ } as t) =
  let open Uint8 in
  let get_n_oam t i = try Mmu.get_n t (0xFE00 + i) with _ -> failwith "get_n_oam" in

  match Control.(is_flag_set obj_on gpu) with
  | false -> () 
  | true  ->

    let rec loop i =
    if i >= 0 then begin
      let obj_y = get_n_oam t i in
      let obj_size = Control.get_obj_size gpu in
      let height = match obj_size with `Eight -> 8 | `Sixteen -> 16 in

      let y = sub obj_y (chr 16) in
      (* sprite is on current scanline *)
      if (y <= gpu.ly) && (add y (chr height)) > gpu.ly then begin

        let obj_x = get_n_oam t (i + 1) in
        let sprite_tile_number = get_n_oam t (i + 2) in
        let sprite_flags = get_n_oam t (i + 3) in
        let sprite_tile_number =
          match obj_size with
          | `Sixteen -> sprite_tile_number land (chr 0xFE)
          | _ -> sprite_tile_number
        in
        let palette_number =
          if is_bit_set sprite_flags 4 then 0x01 else 0x00
        in
        let c1, c2, c3, c4 =
          let open Uint8 in
          match palette_number with
          | 0x00 ->
            chr 0x00,
            (gpu.obj_pal0 lsr Char.chr 2) land (chr 0b11),
            (gpu.obj_pal0 lsr Char.chr 4) land (chr 0b11),
            (gpu.obj_pal0 lsr Char.chr 6) land (chr 0b11)
          | _ ->
            chr 0x00,
            (gpu.obj_pal1 lsr Char.chr 2) land (chr 0b11),
            (gpu.obj_pal1 lsr Char.chr 4) land (chr 0b11),
            (gpu.obj_pal1 lsr Char.chr 6) land (chr 0b11)
        in
        let x = sub obj_x (chr 8) in

        let sprite_size_in_bytes = chr 16 in
        let tile_data = 0x0000 in
        let tile_pointer = (tile_data + (code sprite_tile_number * code sprite_size_in_bytes)) in
        let tile_y_offset =
          if is_bit_set sprite_flags 6 then
            Infix.(((chr height) - one) - (gpu.ly - y))
          else
            Infix.(gpu.ly - y)
        in
        let tile_pointer = (tile_pointer + (code tile_y_offset * (2))) in
        let vram = 0x8000 in
        let low = Mmu.get_n t (vram + (tile_pointer)) in
        let high = Mmu.get_n t (vram + (tile_pointer) + 1) in

        for index_x = 0 to 7 do
          let pixel_x = (code x) + index_x in
          if (pixel_x >= 0) && (pixel_x <= 160) then begin
            let bit = if is_bit_set sprite_flags 5 then index_x else 7 - index_x in
            let pixel_val = if is_bit_set high bit then zero lor (chr 0x02) else zero in
            let pixel_val = if is_bit_set low bit then pixel_val lor (chr 0x01) else pixel_val in
            let color =
              match code pixel_val with
              | 0x00 -> c1
              | 0x01 -> c2
              | 0x02 -> c3
              | 0x03 -> c4
              | _ -> assert false
            in
            let color' = Gpu.map_color color in
            let index = pixel_x in
            if pixel_val != Uint8.zero then
            if not (is_bit_set sprite_flags 7) || gpu.framebuffer.(index).(Char.code gpu.ly) = `White then begin
              try
                Gpu.put_pixel ~x:(index) ~y:(Char.code gpu.ly) ~color:color' gpu
              with
              | _ -> failwith (Printf.sprintf "dang %02X %02X" index (code gpu.ly))
            end
            else
              ()
          end
        done
      end;
      loop (i - 4)
    end
  in
  loop 156

let oam_ram_cycles = 80
let lcd_transfer_cycles = 172
let hblank_cycles = 204
let vblank_cycles = 456

let dma_transfer t addr =
  t.gpu.dma_transfer_request <- false;
  t.gpu.dma_clock <- 752;
  let src = (Uint8.code addr) * 0x100 in
  for offset = 0x00 to 0x9F do
    let source = src lor offset in
    Mmu.put_n t (0xFE00 + offset) (Mmu.get_n t source)
  done

let trigger_interrupt t int =
  let isf = Mmu.get_n t 0xFF0F in
  Mmu.put_n t 0xFF0F (Uint8.set_bit isf int)

let step ({ gpu; _ } as t) cycles =
  let current_mode = Lcd.get_mode_flag gpu in
  if gpu.dma_transfer_request then
    dma_transfer t gpu.dma_transfer;
  if gpu.dma_clock > 0 then begin
    gpu.dma_clock <- gpu.dma_clock - cycles;
  end;
  let clock = gpu.clock + cycles in
  let gpu' =
    match current_mode with
    | `OAM_ram when clock >= oam_ram_cycles -> begin
      let clock = clock - oam_ram_cycles in
      Lcd.set_mode_flag { gpu with clock } `Lcd_transfer
    end
    | `Lcd_transfer when clock >= lcd_transfer_cycles ->
      let clock = clock - lcd_transfer_cycles in
      render_line_bg t;
      render_line_window t;
      render_line_obj t;
      trigger_interrupt t Interrupts.lcd_stats;
      Lcd.set_mode_flag { gpu with clock } `Hblank
    | `Hblank when clock >= hblank_cycles ->
      let clock = clock - hblank_cycles in
      let ly = Uint8.succ gpu.ly in
      if (Char.code ly == 144) then begin
        gpu.redraw <- true;
        trigger_interrupt t Interrupts.vblank;
        trigger_interrupt t Interrupts.lcd_stats;
        Lcd.set_mode_flag {gpu with clock; ly; } `Vblank
      end
      else begin
        let gpu = Lcd.set_mode_flag { gpu with clock; ly; } `OAM_ram in
          trigger_interrupt t Interrupts.lcd_stats;
        gpu
      end
    | `Vblank when clock >= vblank_cycles ->
      let clock = clock - vblank_cycles in
      let ly = Uint8.succ gpu.ly in
      if Char.code ly = 154 then begin
        let ly = Uint8.zero in
          trigger_interrupt t Interrupts.lcd_stats;
        Lcd.set_mode_flag { gpu with clock; ly; } `OAM_ram
      end
      else { gpu with clock; ly; }
    | _ -> { gpu with clock; }
  in
  let lcd_register =
    if gpu'.ly = gpu'.lyc then begin
      let lcd = Lcd.set_flag Lcd.lyc_ly_flag gpu' in
        trigger_interrupt { t with gpu = gpu'; } Interrupts.lcd_stats;
      lcd
    end else 
      Lcd.unset_flag Lcd.lyc_ly_flag gpu'
  in
  {
    t with
    gpu = { gpu' with lcd_register };
  }
