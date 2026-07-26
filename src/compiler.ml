module StringMap = Map.Make (String)
module FunctionMap = Map.Make (String)
module ConstructorMap = Map.Make (String)

type env = Interaction_net.port StringMap.t

type match_row = {
  patterns : Ast.pattern list;
  body : Ast.expr;
}

type decision_tree =
  | Leaf of match_row
  | Switch of {
      symbol : string;
      column : int;
      preserve : bool;
      cases : (string * int * decision_tree) list;
    }

type function_spec = {
  name : string;
  arity : int;
  tree : decision_tree;
}

type compile_context = {
  rulebook : Interaction_net.rulebook;
  functions : function_spec FunctionMap.t;
  constructors : int ConstructorMap.t;
}

type compiled_program = {
  root : int;
  net : Interaction_net.net;
  rules : Interaction_net.rulebook;
}

let reserved_function_names =
  [ "add"; "succ"; "Z"; "S"; "Add"; "Dup"; "Era"; "Root" ]

let reserved_constructor_names = [ "Z"; "S"; "Add"; "Dup"; "Era"; "Root" ]

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

let erase_port port net =
  let era_id, net = Interaction_net.new_agent "Era" 0 net in
  Interaction_net.connect (era_id, 0) port net

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
  | Ast.Constr (name, args) ->
      let arg_ports, env, net =
        List.fold_left
          (fun (ports, env, net) arg ->
            let port, env, net = compile_expr ctx env net arg in
            (port :: ports, env, net))
          ([], env, net) args
      in
      let arg_ports = List.rev arg_ports in
      let ctor_id, net = Interaction_net.new_agent name (List.length arg_ports) net in
      let net =
        List.fold_left2
          (fun net slot port -> Interaction_net.connect (ctor_id, slot) port net)
          net
          (List.init (List.length arg_ports) (fun index -> index + 1))
          arg_ports
      in
      ((ctor_id, 0), env, net)
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
      let net = erase_port value_port net in
      compile_expr ctx env net body
  | Ast.Call ("add", args) -> compile_add ctx env net args
  | Ast.Call (name, args) -> compile_call ctx env net name args

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
        Ravel_error.compile_error (Printf.sprintf "undefined function '%s'" name)
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
    match arg_ports with first :: rest -> (first, rest) | [] -> assert false
  in
  let net = Interaction_net.connect (fn_id, 0) first_arg net in
  let net =
    List.fold_left2
      (fun net slot port -> Interaction_net.connect (fn_id, slot) port net)
      net
      (List.init (List.length rest_args) (fun index -> index + 1))
      rest_args
  in
  ((fn_id, spec.arity), env, net)

and compile_leaf ctx row values output net =
  if List.length row.patterns <> List.length values then
    invalid_arg "compile_leaf";
  let env, names, net =
    List.fold_left2
      (fun (env, names, net) pattern port ->
        match pattern with
        | Ast.PVar name ->
            ensure_fresh name env;
            (StringMap.add name port env, name :: names, net)
        | Ast.PWildcard -> (env, names, erase_port port net)
        | _ -> invalid_arg "compile_leaf: refutable pattern remains")
      (StringMap.empty, [], net) row.patterns values
  in
  let result, env_after, net = compile_expr ctx env net row.body in
  require_consumed names env_after;
  Interaction_net.connect result output net

and instantiate_tree ctx tree values output net =
  match tree with
  | Leaf row -> compile_leaf ctx row values output net
  | Switch { symbol; column; preserve; _ } ->
      let selected = List.nth values column in
      let others = List.filteri (fun index _ -> index <> column) values in
      let inspected, auxiliaries, net =
        if preserve then
          let dup, net = Interaction_net.new_agent "Dup" 2 net in
          let net = Interaction_net.connect (dup, 0) selected net in
          ((dup, 1), others @ [ (dup, 2) ], net)
        else (selected, others, net)
      in
      let agent, net =
        Interaction_net.new_agent symbol (List.length auxiliaries + 1) net
      in
      let net = Interaction_net.connect (agent, 0) inspected net in
      let ports = auxiliaries @ [ output ] in
      List.fold_left2
        (fun net slot port -> Interaction_net.connect (agent, slot) port net)
        net
        (List.init (List.length ports) (fun index -> index + 1))
        ports

let rec names_bound_by_pattern = function
  | Ast.PZero | Ast.PWildcard -> []
  | Ast.PSucc name | Ast.PVar name -> [ name ]
  | Ast.PConstr (_, patterns) ->
      List.concat (List.map names_bound_by_pattern patterns)

let normalize_pattern = function
  | Ast.PZero -> Ast.PConstr ("Z", [])
  | Ast.PSucc name -> Ast.PConstr ("S", [ Ast.PVar name ])
  | Ast.PConstr ("succ", patterns) -> Ast.PConstr ("S", patterns)
  | pattern -> pattern

let rec normalize_pattern_deep pattern =
  match normalize_pattern pattern with
  | Ast.PConstr (name, patterns) ->
      Ast.PConstr (name, List.map normalize_pattern_deep patterns)
  | pattern -> pattern

let validate_definition_shape def =
  if List.mem def.Ast.name reserved_function_names then
    Ravel_error.compile_error
      (Printf.sprintf "function name '%s' is reserved" def.Ast.name);
  if def.Ast.patterns = [] then
    Ravel_error.compile_error "function definitions must have at least one pattern";
  let bound_names = List.concat (List.map names_bound_by_pattern def.Ast.patterns) in
  ensure_distinct bound_names "parameter"

let replace_column column replacements items =
  let rec loop index = function
    | [] -> if index = column then replacements else []
    | item :: rest ->
        if index = column then replacements @ rest
        else item :: loop (index + 1) rest
  in
  loop 0 items

let first_refutable_column rows =
  match rows with
  | [] -> None
  | first :: _ ->
      let width = List.length first.patterns in
      let rec search column =
        if column = width then None
        else if
          List.exists
            (fun row ->
              match List.nth row.patterns column with
              | Ast.PConstr _ -> true
              | _ -> false)
            rows
        then Some column
        else search (column + 1)
      in
      search 0

let constructor_at pattern =
  match pattern with Ast.PConstr (name, fields) -> Some (name, List.length fields) | _ -> None

let unique_constructors patterns =
  List.fold_left
    (fun constructors pattern ->
      match constructor_at pattern with
      | None -> constructors
      | Some (name, arity) ->
          if List.exists (fun (existing, _) -> existing = name) constructors then constructors
          else constructors @ [ (name, arity) ])
    [] patterns

let validate_nat_exhaustiveness function_name patterns has_fallback =
  if not has_fallback then
    let names =
      List.filter_map (fun pattern -> Option.map fst (constructor_at pattern)) patterns
    in
    let has_z = List.mem "Z" names in
    let has_s = List.mem "S" names in
    if has_z <> has_s then
      Ravel_error.compile_error
        (Printf.sprintf
           "function '%s' is non-exhaustive; natural-number matches must cover both 0 and succ(...) or include a variable or '_' fallback"
           function_name)

let fresh_match_symbol =
  let next = ref 0 in
  fun function_name ->
    let id = !next in
    incr next;
    Printf.sprintf "$match:%s:%d" function_name id

let rec build_decision_tree constructors function_name rows =
  match first_refutable_column rows with
  | None -> (
      match rows with
      | row :: _ -> Leaf row
      | [] -> invalid_arg "build_decision_tree")
  | Some column ->
      let column_patterns = List.map (fun row -> List.nth row.patterns column) rows in
      let has_fallback =
        List.exists
          (function Ast.PVar _ | Ast.PWildcard -> true | _ -> false)
          column_patterns
      in
      let preserve =
        List.exists (function Ast.PVar _ -> true | _ -> false) column_patterns
      in
      validate_nat_exhaustiveness function_name column_patterns has_fallback;
      let constructors_to_branch =
        if has_fallback then ConstructorMap.bindings constructors
        else unique_constructors column_patterns
      in
      let specialize constructor arity row =
        let selected = List.nth row.patterns column in
        let saved_pattern =
          if preserve then
            match selected with Ast.PVar name -> Ast.PVar name | _ -> Ast.PWildcard
          else Ast.PWildcard
        in
        let row =
          if preserve then { row with patterns = row.patterns @ [ saved_pattern ] }
          else row
        in
        match selected with
        | Ast.PConstr (name, fields) when name = constructor ->
            Some { row with patterns = replace_column column fields row.patterns }
        | Ast.PConstr _ -> None
        | Ast.PVar _ | Ast.PWildcard ->
            Some
              {
                row with
                patterns =
                  replace_column column (List.init arity (fun _ -> Ast.PWildcard))
                    row.patterns;
              }
        | Ast.PZero | Ast.PSucc _ -> assert false
      in
      let cases =
        List.filter_map
          (fun (constructor, arity) ->
            let specialized = List.filter_map (specialize constructor arity) rows in
            match specialized with
            | [] -> None
            | _ ->
                Some
                  ( constructor,
                    arity,
                    build_decision_tree constructors function_name specialized ))
          constructors_to_branch
      in
      Switch
        {
          symbol = fresh_match_symbol function_name;
          column;
          preserve;
          cases;
        }

let clause_arity def = List.length def.Ast.patterns

let build_function_specs definitions constructors =
  let grouped = Hashtbl.create 16 in
  List.iter
    (fun def ->
      validate_definition_shape def;
      let name = def.Ast.name in
      let arity = clause_arity def in
      let row =
        {
          patterns = List.map normalize_pattern_deep def.Ast.patterns;
          body = def.Ast.body;
        }
      in
      match Hashtbl.find_opt grouped name with
      | None -> Hashtbl.add grouped name (arity, [ row ])
      | Some (existing_arity, rows) ->
          if arity <> existing_arity then
            Ravel_error.compile_error
              (Printf.sprintf
                 "function '%s' is defined with inconsistent arities (%d vs %d)"
                 name existing_arity arity);
          Hashtbl.replace grouped name (existing_arity, rows @ [ row ]))
    definitions;
  Hashtbl.fold
    (fun name (arity, rows) specs ->
      let tree = build_decision_tree constructors name rows in
      FunctionMap.add name { name; arity; tree } specs)
    grouped FunctionMap.empty

let rebuild_constructor name fields net =
  let constructor, net = Interaction_net.new_agent name (List.length fields) net in
  let net =
    List.fold_left2
      (fun net slot field -> Interaction_net.connect (constructor, slot) field net)
      net
      (List.init (List.length fields) (fun index -> index + 1))
      fields
  in
  ((constructor, 0), net)

let split_last items =
  match List.rev items with
  | last :: reversed_rest -> (List.rev reversed_rest, last)
  | [] -> invalid_arg "split_last"

let insert_at index inserted items =
  let rec loop current = function
    | rest when current = index -> inserted @ rest
    | item :: rest -> item :: loop (current + 1) rest
    | [] -> invalid_arg "insert_at"
  in
  loop 0 items

let rec install_tree_rules ctx function_name tree =
  match tree with
  | Leaf _ -> ()
  | Switch { symbol; column; preserve; cases } ->
      List.iter
        (fun (constructor, _arity, child) ->
          Interaction_net.def_rule ctx.rulebook constructor symbol
            (fun fields interface net ->
              let auxiliary_values, output = split_last interface in
              let other_values, saved =
                if preserve then
                  let others, saved = split_last auxiliary_values in
                  (others, Some saved)
                else (auxiliary_values, None)
              in
              let values = insert_at column fields other_values in
              let values =
                match saved with Some port -> values @ [ port ] | None -> values
              in
              instantiate_tree ctx child values output net);
          install_tree_rules ctx function_name child)
        cases

let install_function_rules ctx spec =
  ConstructorMap.iter
    (fun constructor _arity ->
      Interaction_net.def_rule ctx.rulebook constructor spec.name
        (fun fields interface net ->
          let rest_args, output = split_last interface in
          let first_arg, net = rebuild_constructor constructor fields net in
          instantiate_tree ctx spec.tree (first_arg :: rest_args) output net))
    ctx.constructors;
  install_tree_rules ctx spec.name spec.tree

let collect_constructor name arity constructors =
  if List.mem name reserved_constructor_names then
    Ravel_error.compile_error
      (Printf.sprintf "constructor name '%s' is reserved" name);
  match ConstructorMap.find_opt name constructors with
  | None -> ConstructorMap.add name arity constructors
  | Some existing_arity ->
      if existing_arity <> arity then
        Ravel_error.compile_error
          (Printf.sprintf
             "constructor '%s' is used with inconsistent arities (%d vs %d)"
             name existing_arity arity)
      else constructors

let rec collect_expr_constructors constructors = function
  | Ast.Int _ | Ast.Var _ -> constructors
  | Ast.Succ expr -> collect_expr_constructors constructors expr
  | Ast.Constr (name, args) ->
      List.fold_left collect_expr_constructors
        (collect_constructor name (List.length args) constructors)
        args
  | Ast.Let (_, value, body) ->
      collect_expr_constructors
        (collect_expr_constructors constructors value)
        body
  | Ast.Dup (value, _, _, body) | Ast.Drop (value, body) ->
      collect_expr_constructors
        (collect_expr_constructors constructors value)
        body
  | Ast.Call (_, args) -> List.fold_left collect_expr_constructors constructors args

let rec collect_pattern_constructors constructors = function
  | Ast.PZero | Ast.PSucc _ | Ast.PVar _ | Ast.PWildcard -> constructors
  | Ast.PConstr ("succ", patterns) ->
      List.fold_left collect_pattern_constructors constructors patterns
  | Ast.PConstr (name, patterns) ->
      List.fold_left collect_pattern_constructors
        (collect_constructor name (List.length patterns) constructors)
        patterns

let collect_program_constructors program =
  let constructors =
    ConstructorMap.empty |> ConstructorMap.add "Z" 0 |> ConstructorMap.add "S" 1
  in
  let constructors =
    List.fold_left
      (fun constructors def ->
        let constructors =
          List.fold_left collect_pattern_constructors constructors def.Ast.patterns
        in
        collect_expr_constructors constructors def.Ast.body)
      constructors program.Ast.definitions
  in
  collect_expr_constructors constructors program.Ast.main

let build_context definitions constructors =
  let functions = build_function_specs definitions constructors in
  let rulebook = Interaction_net.base_rulebook () in
  ConstructorMap.iter
    (fun name arity ->
      if name <> "Z" && name <> "S" then
        Interaction_net.install_constructor_rules rulebook name arity)
    constructors;
  let ctx = { rulebook; functions; constructors } in
  FunctionMap.iter (fun _ spec -> install_function_rules ctx spec) functions;
  ctx

let compile program =
  let constructors = collect_program_constructors program in
  let ctx = build_context program.Ast.definitions constructors in
  let result_port, env, net =
    compile_expr ctx StringMap.empty Interaction_net.empty_net program.main
  in
  if not (StringMap.is_empty env) then (
    let names = StringMap.bindings env |> List.map fst |> String.concat ", " in
    Ravel_error.compile_error
      (Printf.sprintf
         "internal linearity error: unconsumed bindings remain at top level: %s"
         names));
  let root, net = Interaction_net.attach_root result_port net in
  { root; net; rules = ctx.rulebook }
