let cases_dir = "test/cases/parse"

let run () =
  Test_support.run_test "parse_expression_only_program" (fun () ->
      let actual = Parser.parse (Test_support.read_case (cases_dir ^ "/expression_only_program.rvl")) in
      let expected =
        {
          Ast.definitions = [];
          main = Ast.Call ("add", [ Ast.Int 2; Ast.Int 3 ]);
        }
      in
      Test_support.assert_program_equal "parse_expression_only_program" expected actual);

  Test_support.run_test "parse_function_definition_program" (fun () ->
      let actual =
        Parser.parse (Test_support.read_case (cases_dir ^ "/function_definition_program.rvl"))
      in
      let expected =
        {
          Ast.definitions =
            [
              {
                Ast.name = "plus";
                pattern = Ast.PZero;
                params = [ "y" ];
                body = Ast.Var "y";
              };
              {
                Ast.name = "plus";
                pattern = Ast.PSucc "x";
                params = [ "y" ];
                body =
                  Ast.Succ (Ast.Call ("plus", [ Ast.Var "x"; Ast.Var "y" ]));
              };
            ];
          main = Ast.Call ("plus", [ Ast.Int 2; Ast.Int 3 ]);
        }
      in
      Test_support.assert_program_equal "parse_function_definition_program" expected actual);

  Test_support.run_test "parse_comments_and_whitespace" (fun () ->
      let actual =
        Parser.parse (Test_support.read_case (cases_dir ^ "/comments_and_whitespace.rvl"))
      in
      let expected =
        {
          Ast.definitions = [];
          main = Ast.Let ("x", Ast.Int 2, Ast.Drop (Ast.Var "x", Ast.Int 0));
        }
      in
      Test_support.assert_program_equal "parse_comments_and_whitespace" expected actual);

  Test_support.run_test "parse_error" (fun () ->
      Test_support.expect_parse_error "parse_error" (fun () ->
          Parser.parse (Test_support.read_case (cases_dir ^ "/parse_error_missing_paren.rvl"))))
