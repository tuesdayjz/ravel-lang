let cases_dir = "test/cases/parse"

let run () =
  Test_support.run_test "parse_expression_only_program" (fun () ->
      let actual = Parser.parse (Test_support.read_case (cases_dir ^ "/expression_only_program.rvl")) in
      let expected =
        {
          Ast.type_definitions = [];
          definitions = [];
          main = Ast.Apply (Ast.Var "add", [ Ast.Int 2; Ast.Int 3 ]);
        }
      in
      Test_support.assert_program_equal "parse_expression_only_program" expected actual);

  Test_support.run_test "parse_function_definition_program" (fun () ->
      let actual =
        Parser.parse (Test_support.read_case (cases_dir ^ "/function_definition_program.rvl"))
      in
      let expected =
        {
          Ast.type_definitions = [];
          definitions =
            [
              {
                Ast.name = "plus";
                patterns = [ Ast.PZero; Ast.PVar "y" ];
                body = Ast.Var "y";
              };
              {
                Ast.name = "plus";
                patterns = [ Ast.PSucc "x"; Ast.PVar "y" ];
                body =
                  Ast.Succ
                    (Ast.Apply (Ast.Var "plus", [ Ast.Var "x"; Ast.Var "y" ]));
              };
            ];
          main = Ast.Apply (Ast.Var "plus", [ Ast.Int 2; Ast.Int 3 ]);
        }
      in
      Test_support.assert_program_equal "parse_function_definition_program" expected actual);

  Test_support.run_test "parse_comments_and_whitespace" (fun () ->
      let actual =
        Parser.parse (Test_support.read_case (cases_dir ^ "/comments_and_whitespace.rvl"))
      in
      let expected =
        {
          Ast.type_definitions = [];
          definitions = [];
          main = Ast.Let ("x", Ast.Int 2, Ast.Drop (Ast.Var "x", Ast.Int 0));
        }
      in
      Test_support.assert_program_equal "parse_comments_and_whitespace" expected actual);

  Test_support.run_test "parse_nested_constructor_patterns" (fun () ->
      let actual =
        Parser.parse
          (Test_support.read_case (cases_dir ^ "/nested_constructor_patterns.rvl"))
      in
      let expected =
        {
          Ast.type_definitions = [];
          definitions =
            [
              {
                Ast.name = "head_or";
                patterns =
                  [
                    Ast.PConstr ("Cons", [ Ast.PVar "x"; Ast.PWildcard ]);
                    Ast.PVar "fallback";
                  ];
                body = Ast.Var "x";
              };
              {
                Ast.name = "head_or";
                patterns = [ Ast.PConstr ("Nil", []); Ast.PVar "fallback" ];
                body = Ast.Var "fallback";
              };
            ];
          main =
            Ast.Apply
              ( Ast.Var "head_or",
                [
                  Ast.Constr
                    ("Cons", [ Ast.Succ (Ast.Int 0); Ast.Constr ("Nil", []) ]);
                  Ast.Int 0;
                ] );
        }
      in
      Test_support.assert_program_equal "parse_nested_constructor_patterns"
        expected actual);

  Test_support.run_test "parse_data_declarations" (fun () ->
      let actual =
        Parser.parse (Test_support.read_case (cases_dir ^ "/data_declarations.rvl"))
      in
      let expected =
        {
          Ast.type_definitions =
            [
              { Ast.type_name = "List"; constructors = [ "Nil"; "Cons" ] };
              { Ast.type_name = "Option"; constructors = [ "None"; "Some" ] };
            ];
          definitions = [];
          main = Ast.Constr ("Some", [ Ast.Constr ("Nil", []) ]);
        }
      in
      Test_support.assert_program_equal "parse_data_declarations" expected actual);

  Test_support.run_test "parse_lambda_application" (fun () ->
      let actual =
        Parser.parse (Test_support.read_case (cases_dir ^ "/lambda_application.rvl"))
      in
      let expected =
        {
          Ast.type_definitions = [];
          definitions = [];
          main =
            Ast.Let
              ( "apply",
                Ast.Lambda
                  ([ "f"; "x" ], Ast.Apply (Ast.Var "f", [ Ast.Var "x" ])),
                Ast.Apply
                  ( Ast.Var "apply",
                    [
                      Ast.Lambda ([ "n" ], Ast.Succ (Ast.Var "n"));
                      Ast.Int 4;
                    ] ) );
        }
      in
      Test_support.assert_program_equal "parse_lambda_application" expected actual);

  Test_support.run_test "parse_error" (fun () ->
      Test_support.expect_parse_error "parse_error" (fun () ->
          Parser.parse (Test_support.read_case (cases_dir ^ "/parse_error_missing_paren.rvl"))))
