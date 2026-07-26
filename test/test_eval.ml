let cases_dir = "test/cases/eval"

let run () =
  Test_support.run_test "eval_builtin_add" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/builtin_add.rvl") in
      Test_support.assert_int_equal "eval_builtin_add" 5 result.value);

  Test_support.run_test "eval_recursive_plus" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/recursive_plus.rvl") in
      Test_support.assert_int_equal "eval_recursive_plus" 5 result.value);

  Test_support.run_test "eval_double_with_dup" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/double_with_dup.rvl") in
      Test_support.assert_int_equal "eval_double_with_dup" 6 result.value);

  Test_support.run_test "eval_catchall_clause" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/catchall_clause.rvl") in
      Test_support.assert_int_equal "eval_catchall_clause" 4 result.value);

  Test_support.run_test "eval_mutual_recursion" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/mutual_recursion.rvl") in
      Test_support.assert_int_equal "eval_mutual_recursion" 1 result.value);

  Test_support.run_test "eval_recursive_sum_to" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/recursive_sum_to.rvl") in
      Test_support.assert_int_equal "eval_recursive_sum_to" 10 result.value);

  Test_support.run_test "eval_factorial" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/factorial.rvl") in
      Test_support.assert_int_equal "eval_factorial" 120 result.value);

  Test_support.run_test "eval_logic_arith" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/logic_arith.rvl") in
      Test_support.assert_int_equal "eval_logic_arith" 1 result.value);

  Test_support.run_test "eval_work_pool" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/work_pool.rvl") in
      Test_support.assert_int_equal "eval_work_pool" 86 result.value);

  Test_support.run_test "run_file_fixture" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/builtin_add.rvl") in
      Test_support.assert_int_equal "run_file_fixture" 5 result.value);

  Test_support.run_test "strategy_invariance" (fun () ->
      let source = Test_support.read_case (cases_dir ^ "/double_with_dup.rvl") in
      let first = Driver.run_string ~strategy:Driver.First source in
      let random = Driver.run_string ~strategy:Driver.Random ~seed:42 source in
      Test_support.assert_int_equal "strategy_invariance value" first.value random.value;
      Test_support.assert_int_equal "strategy_invariance steps"
        (Driver.step_count first) (Driver.step_count random))
