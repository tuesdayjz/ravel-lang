let () =
  try Cli.main ()
  with
  | Ravel_error.Parse_error (pos, message) ->
      prerr_endline
        (Printf.sprintf "Parse error at %s: %s"
           (Ravel_error.string_of_position pos)
           message);
      exit 1
  | Ravel_error.Compile_error message ->
      prerr_endline ("Compile error: " ^ message);
      exit 1
  | Sys_error message ->
      prerr_endline ("I/O error: " ^ message);
      exit 1
  | Failure message ->
      prerr_endline ("Runtime failure: " ^ message);
      exit 1
