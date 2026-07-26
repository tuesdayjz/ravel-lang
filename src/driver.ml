type strategy = First | Random

type run_result = {
  program : Ast.program;
  initial_net : Interaction_net.net;
  final_net : Interaction_net.net;
  root : int;
  log : (string * string) list;
  value : Interaction_net.value;
}

let parse_string source = Parser.parse source

let read_file path =
  let ch = open_in path in
  try
    let len = in_channel_length ch in
    let source = really_input_string ch len in
    close_in ch;
    source
  with exn ->
    close_in_noerr ch;
    raise exn

let parse_file path = read_file path |> parse_string

let pick_for_strategy = function
  | First -> Interaction_net.pick_first
  | Random -> Interaction_net.pick_random

let initialize_random = function
  | Some seed -> Random.init seed
  | None -> Random.self_init ()

let run_program ?(strategy = First) ?seed program =
  initialize_random seed;
  let compiled = Compiler.compile program in
  let initial_net = compiled.net in
  let final_net, log =
    Interaction_net.run ~rules:compiled.rules
      ~pick:(pick_for_strategy strategy) initial_net
  in
  let value =
    Interaction_net.readback_value final_net
      (Interaction_net.observe_root compiled.root final_net)
  in
  { program; initial_net; final_net; root = compiled.root; log; value }

let run_string ?strategy ?seed source =
  let program = parse_string source in
  run_program ?strategy ?seed program

let run_file ?strategy ?seed path =
  let program = parse_file path in
  run_program ?strategy ?seed program

let step_count result = List.length result.log
let trace result = Interaction_net.show_log result.log
