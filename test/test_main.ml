let () =
  Test_parse.run ();
  Test_eval.run ();
  Test_error.run ();
  Printf.printf "All tests passed.\n"
