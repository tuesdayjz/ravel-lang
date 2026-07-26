let failf fmt = Printf.ksprintf failwith fmt

let read_case path = Driver.read_file path

let assert_int_equal label expected actual =
  if expected <> actual then
    failf "%s: expected %d, got %d" label expected actual

let assert_nat_value_equal label expected actual =
  match Interaction_net.int_of_value_opt actual with
  | Some value -> assert_int_equal label expected value
  | None ->
      failf "%s: expected nat %d, got %s" label expected
        (Interaction_net.value_to_string actual)

let assert_value_string_equal label expected actual =
  let rendered = Interaction_net.value_to_string actual in
  if expected <> rendered then
    failf "%s: expected %s, got %s" label expected rendered

let assert_program_equal label expected actual =
  if expected <> actual then
    failf "%s: expected %s, got %s" label (Ast.to_string expected)
      (Ast.to_string actual)

let expect_parse_error label thunk =
  try
    let _ = thunk () in
    failf "%s: expected parse error" label
  with
  | Ravel_error.Parse_error _ -> ()

let expect_compile_error label expected_message thunk =
  try
    let _ = thunk () in
    failf "%s: expected compile error" label
  with
  | Ravel_error.Compile_error message ->
      if message <> expected_message then
        failf "%s: expected compile error %S, got %S" label
          expected_message message

let run_test name thunk =
  try
    thunk ();
    Printf.printf "PASS %s\n" name
  with exn ->
    Printf.eprintf "FAIL %s: %s\n" name (Printexc.to_string exn);
    raise exn
