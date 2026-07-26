let cases_dir = "test/cases/error"

let run () =


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
        "function 'only_zero' is non-exhaustive for data type 'Nat'; missing constructor(s): S, or include a variable or '_' fallback"
        (fun () -> Driver.run_file (cases_dir ^ "/non_exhaustive_function.rvl")));

  Test_support.run_test "non_exhaustive_declared_data_error" (fun () ->
      Test_support.expect_compile_error "non_exhaustive_declared_data_error"
        "function 'is_empty' is non-exhaustive for data type 'List'; missing constructor(s): Cons, or include a variable or '_' fallback"
        (fun () ->
          Driver.run_file (cases_dir ^ "/non_exhaustive_declared_data.rvl")));

  Test_support.run_test "mixed_data_families_error" (fun () ->
      Test_support.expect_compile_error "mixed_data_families_error"
        "function 'classify' mixes constructors from different data types in one pattern column: List, Option"
        (fun () -> Driver.run_file (cases_dir ^ "/mixed_data_families.rvl")))
