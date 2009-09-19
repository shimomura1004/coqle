open Util

(* url decoding : string -> string *)
let decode text =
  let parce_char c = match c with
    | '0'..'9' -> int_of_char c - int_of_char '0'
    | 'a'..'f' -> 10 + int_of_char c - int_of_char 'a'
    | 'A'..'F' -> 10 + int_of_char c - int_of_char 'A'
    | _ -> failwith ("decode parce_char: " ^ String.make 1 c)
  in
  let rec aux i s =
    if i < String.length s then
      match s.[i] with
      | '+' -> (s.[i] <- ' '; aux (i+1) s)
      | '%' -> String.sub s 0 i
          ^ String.make 1 (char_of_int (16 * parce_char s.[i+1] + parce_char s.[i+2]))
          ^ aux 0 (String.sub s (i+3) (String.length s - i - 3))
      | _ -> aux (i+1) s
    else s
  in aux 0 text


(* GET method parameter *)
let get_param pname : string =
  try
    let qstr = Sys.getenv "QUERY_STRING" in
    let qs = Str.split (Str.regexp "&") qstr in
    let qs = List.map
	(fun row ->
	  match Str.split (Str.regexp "=") row with
	  | [k; v] -> (k,v)
	  | _ -> failwith @@ "CgiUtil.get_param: invalid query string: "^qstr)
	qs
    in
    try decode @@ List.assoc pname qs with
    | Not_found -> failwith @@ "CgiUtil.get_param: not found param: " ^ pname
  with
  | Not_found -> failwith "CgiUtil.get_param: query_string is not found"
  | Match_failure _ -> failwith "CgiUtil.get_param: match failure"

      
let print_header () =
  print_endline "Content-Type: text/plain;charset=utf-8";
  print_newline ();

