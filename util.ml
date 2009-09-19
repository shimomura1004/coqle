
exception Found
exception Empty
exception Exit
exception Retry_exceed of exn

let ( @@ ) f x = f x
let ( $ ) g f x = g (f x)
let ( +> ) x f = f x
let id x = x
let const c = fun _ -> c
let ( !% ) s = Printf.sprintf s
let tee f x = let () = try f x with _ -> () in x
