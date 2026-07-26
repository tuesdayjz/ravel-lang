let cases_dir = "test/cases/eval"

let run () =
  Test_support.run_test "eval_builtin_add" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/builtin_add.rvl") in
      Test_support.assert_nat_value_equal "eval_builtin_add" 5 result.value);

  Test_support.run_test "eval_recursive_plus" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/recursive_plus.rvl") in
      Test_support.assert_nat_value_equal "eval_recursive_plus" 5 result.value);

  Test_support.run_test "eval_double_with_dup" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/double_with_dup.rvl") in
      Test_support.assert_nat_value_equal "eval_double_with_dup" 6 result.value);

  Test_support.run_test "eval_automatic_dup_drop" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/automatic_dup_drop.rvl") in
      Test_support.assert_nat_value_equal "eval_automatic_dup_drop" 10 result.value);

  Test_support.run_test "eval_closures" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/closures.rvl") in
      Test_support.assert_nat_value_equal "eval_closures" 13 result.value);

  Test_support.run_test "eval_duplicated_closure_capture" (fun () ->
      let result =
        Driver.run_file (cases_dir ^ "/duplicated_closure_capture.rvl")
      in
      Test_support.assert_nat_value_equal "eval_duplicated_closure_capture" 11
        result.value);

  Test_support.run_test "eval_catchall_clause" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/catchall_clause.rvl") in
      Test_support.assert_nat_value_equal "eval_catchall_clause" 4 result.value);

  Test_support.run_test "eval_wildcard_argument" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/wildcard_argument.rvl") in
      Test_support.assert_nat_value_equal "eval_wildcard_argument" 4 result.value);

  Test_support.run_test "eval_constructor_value" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/constructor_value.rvl") in
      Test_support.assert_value_string_equal "eval_constructor_value"
        "Cons(1, Nil)" result.value);

  Test_support.run_test "eval_declared_option" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/declared_option.rvl") in
      Test_support.assert_nat_value_equal "eval_declared_option" 5 result.value);

  Test_support.run_test "eval_deep_pattern_match" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/deep_pattern_match.rvl") in
      Test_support.assert_nat_value_equal "eval_deep_pattern_match" 4 result.value);

  Test_support.run_test "eval_multi_argument_pattern_match" (fun () ->
      let result =
        Driver.run_file (cases_dir ^ "/multi_argument_pattern_match.rvl")
      in
      Test_support.assert_nat_value_equal "eval_multi_argument_pattern_match" 3
        result.value);

  Test_support.run_test "eval_pattern_fallback" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/pattern_fallback.rvl") in
      Test_support.assert_value_string_equal "eval_pattern_fallback"
        "Cons(1, Nil)" result.value);

  Test_support.run_test "eval_mutual_recursion" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/mutual_recursion.rvl") in
      Test_support.assert_nat_value_equal "eval_mutual_recursion" 1 result.value);

  Test_support.run_test "eval_recursive_sum_to" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/recursive_sum_to.rvl") in
      Test_support.assert_nat_value_equal "eval_recursive_sum_to" 10 result.value);

  Test_support.run_test "eval_factorial" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/factorial.rvl") in
      Test_support.assert_nat_value_equal "eval_factorial" 120 result.value);

  Test_support.run_test "eval_logic_arith" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/logic_arith.rvl") in
      Test_support.assert_nat_value_equal "eval_logic_arith" 1 result.value);

  Test_support.run_test "eval_work_pool" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/work_pool.rvl") in
      Test_support.assert_nat_value_equal "eval_work_pool" 86 result.value);

  Test_support.run_test "run_file_fixture" (fun () ->
      let result = Driver.run_file (cases_dir ^ "/builtin_add.rvl") in
      Test_support.assert_nat_value_equal "run_file_fixture" 5 result.value);

  Test_support.run_test "closure_strategy_invariance" (fun () ->
      let source =
        Test_support.read_case (cases_dir ^ "/duplicated_closure_capture.rvl")
      in
      let first = Driver.run_string ~strategy:Driver.First source in
      let random = Driver.run_string ~strategy:Driver.Random ~seed:42 source in
      Test_support.assert_value_string_equal "closure_strategy_invariance"
        (Interaction_net.value_to_string first.value) random.value);

  Test_support.run_test "strategy_invariance" (fun () ->
      let source = Test_support.read_case (cases_dir ^ "/double_with_dup.rvl") in
      let first = Driver.run_string ~strategy:Driver.First source in
      let random = Driver.run_string ~strategy:Driver.Random ~seed:42 source in
      Test_support.assert_value_string_equal "strategy_invariance value"
        (Interaction_net.value_to_string first.value) random.value;
      Test_support.assert_int_equal "strategy_invariance steps"
        (Driver.step_count first) (Driver.step_count random))
