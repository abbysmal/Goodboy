open Notty

let () = Random.self_init ()

let cat1 = [
{|                   _        |};
{|                   \ \      |};
{|                   \ \      |};
{|                    \ \     |};
{|  |\                 \ \    |};
{| /, ~\                ) )   |};
{|X     `-.....-------./ /    |};
{| ~-. ~  ~              |    |};
{|    \             /    |    |};
{|     \  /_     ___\   /     |};
{|     / /  ~~~~~    \ \\     |};
{|    / /|           |\ \\    |};
{|   / / |           | \ |    |};
{|  (_/(_/          (_/(_/    |};
] |> List.map (fun s -> I.string A.(fg white) s) |> I.vcat

let cat2 = [
{|                          _ |};
{|                         / /|};
{|                        / / |};
{|                       / /  |};
{|  |\                   | |  |};
{| /, ~\                / /   |};
{|X     `-.....-------./ /    |};
{| ~-. ~  ~              |    |};
{|    \             /    |    |};
{|     \  /_     ___\   /     |};
{|     | /\ ~~~~~    \ |\     |};
{|     | | \         | | \    |};
{|     | |\ \        | )\ \   |};
{|    (_/ (_/       (_/ (_/   |};
] |> List.map (fun s -> I.string A.(fg white) s) |> I.vcat

let cat_length = 14

let cat_says = [
  "meow";
  "meow meow";
  "nyan";
  "miaouh";
  "i'm a kitty cat";
  "meow meow meow";
]

let cat_says_length = 6

let cat_image pc =
  let sentence =
    pc mod 6 |> List.nth cat_says |> I.string A.(fg black ++ bg white)
  in
  let sentence_image = I.(sentence |> vpad (pc mod 14) 0 |> hpad (pc mod 8) 0) in
  let cat =
    if pc = 0 then
      sentence_image
    else if pc mod 2 = 0 then
      cat1
    else
      cat2
  in
  let image = I.(sentence_image </> hpad 1 0 cat) in
  Window.with_window ~title:"meow" image
