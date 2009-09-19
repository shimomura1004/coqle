open Util
open CgiUtil

let _ =
  print_header ();
  try 
    let query = get_param "q" in
    ignore @@ Sys.command @@ "echo 'SearchPattern(" ^query^ ").' | /opt/local/bin/coqtop"
  with
  | err -> print_string @@ Printexc.to_string err
  
