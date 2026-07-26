let cases_dir = "test/cases/error"

let run () =
  Test_support.run_test "unused_variable_error" (fun () ->
      Test_support.expect_compile_error "unused_variable_error"
        "linearity error: variable 'x' was not used; erase it explicitly with 'drop x in ...'"
        (fun () -> Driver.run_file (cases_dir ^ "/unused_variable.rvl")));

  Test_support.run_test "reused_variable_error" (fun () ->
      Test_support.expect_compile_error "reused_variable_error"
        "variable 'x' is undefined or has already been consumed; duplicate explicitly with 'dup' if you need multiple uses"
        (fun () -> Driver.run_file (cases_dir ^ "/reused_variable.rvl")));

  Test_support.run_test "dup_binders_must_differ" (fun () ->
      Test_support.expect_compile_error "dup_binders_must_differ"
        "dup binders must be distinct"
        (fun () -> Driver.run_file (cases_dir ^ "/dup_binders_must_differ.rvl")));

  Test_support.run_test "undefined_function_error" (fun () ->
      Test_support.expect_compile_error "undefined_function_error"
        "undefined function 'foo'"
        (fun () -> Driver.run_file (cases_dir ^ "/undefined_function.rvl")));

  Test_support.run_test "non_exhaustive_function_error" (fun () ->
      Test_support.expect_compile_error "non_exhaustive_function_error"
        "function 'only_zero' is non-exhaustive; natural-number matches must cover both 0 and succ(...) or include a variable or '_' fallback"
        (fun () -> Driver.run_file (cases_dir ^ "/non_exhaustive_function.rvl")))
