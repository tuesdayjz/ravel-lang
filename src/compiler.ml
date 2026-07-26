module StringMap = Map.Make (String)

module FunctionMap = Map.Make (String)

type env = Interaction_net.port StringMap.t

type clause =
  | ZeroClause of { params : string list; body : Ast.expr }
  | SuccClause of { binder : string; params : string list; body : Ast.expr }
  | VarClause of { binder : string; params : string list; body : Ast.expr }

type function_spec = {
  name : string;
  arity : int;
  clauses : clause list;
}

type compile_context = {
  rulebook : Interaction_net.rulebook;
  functions : function_spec FunctionMap.t;
}

type compiled_program = {
  root : int;
  net : Interaction_net.net;
  rules : Interaction_net.rulebook;
}

let reserved_function_names =
  [ "add"; "succ"; "Z"; "S"; "Add"; "Dup"; "Era"; "Root" ]

let ensure_fresh name env =
  if StringMap.mem name env then
    Ravel_error.compile_error
      (Printf.sprintf "variable '%s' is already bound in this scope" name)

let consume name env =
  match StringMap.find_opt name env with
  | None ->
      Ravel_error.compile_error
        (Printf.sprintf
           "variable '%s' is undefined or has already been consumed; duplicate explicitly with 'dup' if you need multiple uses"
           name)
  | Some port -> (port, StringMap.remove name env)

let require_consumed names env =
  List.iter
    (fun name ->
      if StringMap.mem name env then
        Ravel_error.compile_error
          (Printf.sprintf
             "linearity error: variable '%s' was not used; erase it explicitly with 'drop %s in ...'"
             name name))
    names

let ensure_distinct names what =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun name ->
      if Hashtbl.mem seen name then
        Ravel_error.compile_error
          (Printf.sprintf "%s '%s' is duplicated" what name)
      else Hashtbl.add seen name ())
    names

let bind_names names ports env =
  if List.length names <> List.length ports then invalid_arg "bind_names";
  List.fold_left2
    (fun env name port ->
      ensure_fresh name env;
      StringMap.add name port env)
    env names ports

let rec compile_expr ctx env net = function
  | Ast.Int n ->
      let id, net = Interaction_net.build_num n net in
      ((id, 0), env, net)
  | Ast.Var name ->
      let port, env = consume name env in
      (port, env, net)
  | Ast.Succ expr ->
      let inner_port, env, net = compile_expr ctx env net expr in
      let s_id, net = Interaction_net.new_agent "S" 1 net in
      let net = Interaction_net.connect (s_id, 1) inner_port net in
      ((s_id, 0), env, net)
  | Ast.Let (name, value, body) ->
      ensure_fresh name env;
      let value_port, env, net = compile_expr ctx env net value in
      let env_with_binding = StringMap.add name value_port env in
      let result_port, env_after, net = compile_expr ctx env_with_binding net body in
      require_consumed [ name ] env_after;
      (result_port, StringMap.remove name env_after, net)
  | Ast.Dup (value, left, right, body) ->
      ensure_fresh left env;
      ensure_fresh right env;
      if left = right then
        Ravel_error.compile_error "dup binders must be distinct";
      let value_port, env, net = compile_expr ctx env net value in
      let dup_id, net = Interaction_net.new_agent "Dup" 2 net in
      let net = Interaction_net.connect (dup_id, 0) value_port net in
      let env_with_dups =
        env |> StringMap.add left (dup_id, 1) |> StringMap.add right (dup_id, 2)
      in
      let result_port, env_after, net = compile_expr ctx env_with_dups net body in
      require_consumed [ left; right ] env_after;
      let env_after = env_after |> StringMap.remove left |> StringMap.remove right in
      (result_port, env_after, net)
  | Ast.Drop (value, body) ->
      let value_port, env, net = compile_expr ctx env net value in
      let era_id, net = Interaction_net.new_agent "Era" 0 net in
      let net = Interaction_net.connect (era_id, 0) value_port net in
      compile_expr ctx env net body
  | Ast.Call ("add", args) ->
      compile_add ctx env net args
  | Ast.Call (name, args) ->
      compile_call ctx env net name args

and compile_add ctx env net = function
  | [ lhs; rhs ] ->
      let lhs_port, env, net = compile_expr ctx env net lhs in
      let rhs_port, env, net = compile_expr ctx env net rhs in
      let add_id, net = Interaction_net.new_agent "Add" 2 net in
      let net =
        net
        |> Interaction_net.connect (add_id, 0) lhs_port
        |> Interaction_net.connect (add_id, 1) rhs_port
      in
      ((add_id, 2), env, net)
  | _ -> Ravel_error.compile_error "add expects exactly 2 arguments"

and compile_call ctx env net name args =
  let spec =
    match FunctionMap.find_opt name ctx.functions with
    | Some spec -> spec
    | None ->
        Ravel_error.compile_error
          (Printf.sprintf "undefined function '%s'" name)
  in
  if List.length args <> spec.arity then
    Ravel_error.compile_error
      (Printf.sprintf "function '%s' expects %d arguments, got %d" name spec.arity
         (List.length args));
  let arg_ports, env, net =
    List.fold_left
      (fun (ports, env, net) arg ->
        let port, env, net = compile_expr ctx env net arg in
        (port :: ports, env, net))
      ([], env, net) args
  in
  let arg_ports = List.rev arg_ports in
  let fn_id, net = Interaction_net.new_agent name spec.arity net in
  let first_arg, rest_args =
    match arg_ports with
    | first :: rest -> (first, rest)
    | [] -> assert false
  in
  let net = Interaction_net.connect (fn_id, 0) first_arg net in
  let rec connect_aux slot net = function
    | [] -> net
    | arg_port :: rest ->
        let net = Interaction_net.connect (fn_id, slot) arg_port net in
        connect_aux (slot + 1) net rest
  in
  let net = connect_aux 1 net rest_args in
  ((fn_id, spec.arity), env, net)

let names_bound_by_pattern = function
  | Ast.PZero -> []
  | Ast.PSucc name -> [ name ]
  | Ast.PVar name -> [ name ]

let validate_definition_shape def =
  if List.mem def.Ast.name reserved_function_names then
    Ravel_error.compile_error
      (Printf.sprintf "function name '%s' is reserved" def.Ast.name);
  ensure_distinct (names_bound_by_pattern def.Ast.pattern @ def.Ast.params)
    "parameter";
  if def.Ast.params = [] then ()

let clause_of_definition def =
  validate_definition_shape def;
  match def.Ast.pattern with
  | Ast.PZero -> ZeroClause { params = def.Ast.params; body = def.Ast.body }
  | Ast.PSucc binder ->
      SuccClause { binder; params = def.Ast.params; body = def.Ast.body }
  | Ast.PVar binder ->
      VarClause { binder; params = def.Ast.params; body = def.Ast.body }

let clause_arity def = 1 + List.length def.Ast.params

let validate_clauses name arity clauses =
  match clauses with
  | [ VarClause _ ] -> { name; arity; clauses }
  | [ ZeroClause _; SuccClause _ ] | [ SuccClause _; ZeroClause _ ] ->
      { name; arity; clauses }
  | [ ZeroClause _ ] | [ SuccClause _ ] ->
      Ravel_error.compile_error
        (Printf.sprintf
           "function '%s' is non-exhaustive; provide both 0 and succ(x) clauses or a single catch-all clause"
           name)
  | [ VarClause _; _ ] | [ _; VarClause _ ] ->
      Ravel_error.compile_error
        (Printf.sprintf
           "function '%s' cannot mix a catch-all clause with constructor clauses"
           name)
  | _ ->
      Ravel_error.compile_error
        (Printf.sprintf
           "function '%s' must have either one catch-all clause or exactly two clauses for 0 and succ(x)"
           name)

let build_function_specs definitions =
  let grouped = Hashtbl.create 16 in
  List.iter
    (fun def ->
      let name = def.Ast.name in
      let entry =
        match Hashtbl.find_opt grouped name with
        | None -> (clause_arity def, [ clause_of_definition def ])
        | Some (arity, clauses) ->
            let def_arity = clause_arity def in
            if arity <> def_arity then
              Ravel_error.compile_error
                (Printf.sprintf
                   "function '%s' is defined with inconsistent arities (%d vs %d)"
                   name arity def_arity);
            (arity, clause_of_definition def :: clauses)
      in
      Hashtbl.replace grouped name entry)
    definitions;
  Hashtbl.fold
    (fun name (arity, clauses) acc ->
      let spec = validate_clauses name arity (List.rev clauses) in
      FunctionMap.add name spec acc)
    grouped FunctionMap.empty

let connect_result result_port output_port net =
  Interaction_net.connect result_port output_port net

let compile_case_body ctx env names body output_port net =
  let result_port, env_after, net = compile_expr ctx env net body in
  require_consumed names env_after;
  connect_result result_port output_port net

let compile_zero_clause ctx params body ifcF net =
  let args, output =
    match List.rev ifcF with
    | output :: rev_args -> (List.rev rev_args, output)
    | [] -> assert false
  in
  let env = bind_names params args StringMap.empty in
  compile_case_body ctx env params body output net

let compile_succ_clause ctx binder params body ifcS ifcF net =
  let predecessor =
    match ifcS with
    | [ p ] -> p
    | _ -> assert false
  in
  let args, output =
    match List.rev ifcF with
    | output :: rev_args -> (List.rev rev_args, output)
    | [] -> assert false
  in
  let env = StringMap.empty |> bind_names [ binder ] [ predecessor ] |> bind_names params args in
  compile_case_body ctx env (binder :: params) body output net

let rebuild_zero net =
  let z, net = Interaction_net.new_agent "Z" 0 net in
  ((z, 0), net)

let rebuild_succ predecessor net =
  let s, net = Interaction_net.new_agent "S" 1 net in
  let net = Interaction_net.connect (s, 1) predecessor net in
  ((s, 0), net)

let compile_var_clause_zero ctx binder params body ifcF net =
  let whole, net = rebuild_zero net in
  let args, output =
    match List.rev ifcF with
    | output :: rev_args -> (List.rev rev_args, output)
    | [] -> assert false
  in
  let env =
    StringMap.empty |> bind_names [ binder ] [ whole ] |> bind_names params args
  in
  compile_case_body ctx env (binder :: params) body output net

let compile_var_clause_succ ctx binder params body ifcS ifcF net =
  let predecessor =
    match ifcS with
    | [ p ] -> p
    | _ -> assert false
  in
  let whole, net = rebuild_succ predecessor net in
  let args, output =
    match List.rev ifcF with
    | output :: rev_args -> (List.rev rev_args, output)
    | [] -> assert false
  in
  let env =
    StringMap.empty |> bind_names [ binder ] [ whole ] |> bind_names params args
  in
  compile_case_body ctx env (binder :: params) body output net

let install_function_rule ctx spec =
  match spec.clauses with
  | [ ZeroClause { params; body }; SuccClause { binder; params = s_params; body = s_body } ]
  | [ SuccClause { binder; params = s_params; body = s_body }; ZeroClause { params; body } ] ->
      Interaction_net.def_rule ctx.rulebook "Z" spec.name
        (fun _ifcZ ifcF net -> compile_zero_clause ctx params body ifcF net);
      Interaction_net.def_rule ctx.rulebook "S" spec.name
        (fun ifcS ifcF net -> compile_succ_clause ctx binder s_params s_body ifcS ifcF net)
  | [ VarClause { binder; params; body } ] ->
      Interaction_net.def_rule ctx.rulebook "Z" spec.name
        (fun _ifcZ ifcF net -> compile_var_clause_zero ctx binder params body ifcF net);
      Interaction_net.def_rule ctx.rulebook "S" spec.name
        (fun ifcS ifcF net -> compile_var_clause_succ ctx binder params body ifcS ifcF net)
  | _ -> assert false

let build_context definitions =
  let functions = build_function_specs definitions in
  let rulebook = Interaction_net.base_rulebook () in
  let ctx = { rulebook; functions } in
  functions |> FunctionMap.iter (fun _ spec -> install_function_rule ctx spec);
  ctx

let compile program =
  let ctx = build_context program.Ast.definitions in
  let result_port, env, net = compile_expr ctx StringMap.empty Interaction_net.empty_net program.main in
  if not (StringMap.is_empty env) then (
    let names = StringMap.bindings env |> List.map fst |> String.concat ", " in
    Ravel_error.compile_error
      (Printf.sprintf
         "internal linearity error: unconsumed bindings remain at top level: %s"
         names));
  let root, net = Interaction_net.attach_root result_port net in
  { root; net; rules = ctx.rulebook }
