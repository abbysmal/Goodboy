type t = char

let vblank= 0
let lcd_stats = 1
let timer = 2
let serial = 3
let joypad = 4

let vblank_handler = 0x40
let lcd_stats_handler = 0x48
let timer_handler = 0x50
let joypad_handler = 0x60
