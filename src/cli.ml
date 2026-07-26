type config = {
  expr_source : string option;
  file_path : string option;
  trace : bool;
  dump_ast : bool;
  dump_net : bool;
  strategy : Driver.strategy;
  seed : int option;
}

let default_config =
  {
    expr_source = None;
    file_path = None;
    trace = false;
    dump_ast = false;
    dump_net = false;
    strategy = Driver.First;
    seed = None;
  }

let usage () =
  let text =
    "Usage:\n\
     \  ravel -e \"PROGRAM\" [--trace] [--dump-ast] [--dump-net] [--strategy first|random] [--seed N]\n\
     \  ravel FILE.rvl [--trace] [--dump-ast] [--dump-net] [--strategy first|random] [--seed N]\n\n\
     Program syntax:\n\
     \  program ::= definition* expr\n\
     \  definition ::= def NAME(PATTERN[, NAME ...]) = expr\n\
     \  PATTERN ::= 0 | succ(NAME) | NAME\n\
     \  expr ::= INT | NAME | succ(expr) | NAME(expr, ...)\n\
     \         | let NAME = expr in expr\n\
     \         | dup expr as NAME, NAME in expr\n\
     \         | drop expr in expr\n\
     \         | (expr)\n"
  in
  print_string text

let parse_strategy = function
  | "first" -> Driver.First
  | "random" -> Driver.Random
  | s ->
      Ravel_error.compile_error
        (Printf.sprintf
           "unknown strategy '%s' (expected 'first' or 'random')" s)

let rec parse_args cfg i =
  if i >= Array.length Sys.argv then cfg
  else
    match Sys.argv.(i) with
    | "-e" ->
        if i + 1 >= Array.length Sys.argv then
          Ravel_error.compile_error "missing program after '-e'";
        if cfg.expr_source <> None || cfg.file_path <> None then
          Ravel_error.compile_error
            "provide either '-e PROGRAM' or a file path, not both";
        parse_args { cfg with expr_source = Some Sys.argv.(i + 1) } (i + 2)
    | "--trace" -> parse_args { cfg with trace = true } (i + 1)
    | "--dump-ast" -> parse_args { cfg with dump_ast = true } (i + 1)
    | "--dump-net" -> parse_args { cfg with dump_net = true } (i + 1)
    | "--strategy" ->
        if i + 1 >= Array.length Sys.argv then
          Ravel_error.compile_error "missing value after '--strategy'";
        parse_args
          { cfg with strategy = parse_strategy Sys.argv.(i + 1) }
          (i + 2)
    | "--seed" ->
        if i + 1 >= Array.length Sys.argv then
          Ravel_error.compile_error "missing integer after '--seed'";
        let seed =
          try int_of_string Sys.argv.(i + 1)
          with Failure _ ->
            Ravel_error.compile_error "'--seed' expects an integer"
        in
        parse_args { cfg with seed = Some seed } (i + 2)
    | "-h" | "--help" ->
        usage ();
        exit 0
    | arg when String.length arg > 0 && arg.[0] = '-' ->
        Ravel_error.compile_error (Printf.sprintf "unknown option '%s'" arg)
    | path ->
        if cfg.expr_source <> None || cfg.file_path <> None then
          Ravel_error.compile_error "provide exactly one input program"
        else parse_args { cfg with file_path = Some path } (i + 1)

let read_source cfg =
  match (cfg.expr_source, cfg.file_path) with
  | Some src, None -> src
  | None, Some path -> Driver.read_file path
  | _ ->
      usage ();
      exit 1

let main () =
  let cfg = parse_args default_config 1 in
  let source = read_source cfg in
  let result = Driver.run_string ~strategy:cfg.strategy ?seed:cfg.seed source in
  if cfg.dump_ast then Printf.printf "AST: %s\n" (Ast.to_string result.program);
  if cfg.dump_net then Printf.printf "%s\n" (Interaction_net.dump_net result.initial_net);
  if cfg.trace then (
    Printf.printf "result: %d\n" result.value;
    Printf.printf "steps: %d\n" (Driver.step_count result);
    Printf.printf "trace: %s\n" (Driver.trace result))
  else Printf.printf "%d\n" result.value
