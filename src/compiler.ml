module StringMap = Map.Make (String)
module FunctionMap = Map.Make (String)
module ConstructorMap = Map.Make (String)
module StringSet = Set.Make (String)

type env = Interaction_net.port list StringMap.t

type closure_spec = {
  closure_id : int;
  params : string list;
  captures : string list;
  closure_body : Ast.expr;
}

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

type constructor_family = {
  family_name : string;
  family_constructors : string list;
}

type compile_context = {
  rulebook : Interaction_net.rulebook;
  functions : function_spec FunctionMap.t;
  constructors : int ConstructorMap.t;
  closures : closure_spec list;
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
  | Some (port :: rest) ->
      let env =
        if rest = [] then StringMap.remove name env
        else StringMap.add name rest env
      in
      (port, env)
  | Some [] -> assert false
  | None ->
      Ravel_error.compile_error (Printf.sprintf "undefined variable '%s'" name)

let ensure_distinct names what =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun name ->
      if Hashtbl.mem seen name then
        Ravel_error.compile_error
          (Printf.sprintf "%s '%s' is duplicated" what name)
      else Hashtbl.add seen name ())
    names

let rec count_uses name = function
  | Ast.Int _ -> 0
  | Ast.Var variable -> if variable = name then 1 else 0
  | Ast.Succ expr -> count_uses name expr
  | Ast.Constr (_, args) ->
      List.fold_left (fun total arg -> total + count_uses name arg) 0 args
  | Ast.Apply (callee, args) ->
      count_uses name callee
      + List.fold_left (fun total arg -> total + count_uses name arg) 0 args
  | Ast.Let (binder, value, body) ->
      count_uses name value
      + if binder = name then 0 else count_uses name body
  | Ast.Dup (value, left, right, body) ->
      count_uses name value
      + if left = name || right = name then 0 else count_uses name body
  | Ast.Drop (value, body) -> count_uses name value + count_uses name body
  | Ast.Lambda (params, body) ->
      if List.mem name params then 0 else count_uses name body
  | Ast.Closure (_, _, captures, _) ->
      List.fold_left
        (fun total capture -> if capture = name then total + 1 else total)
        0 captures

let erase_port port net =
  let era_id, net = Interaction_net.new_agent "Era" 0 net in
  Interaction_net.connect (era_id, 0) port net

let distribute_port count port net =
  if count = 0 then ([], erase_port port net)
  else if count = 1 then ([ port ], net)
  else
    let label, net = Interaction_net.fresh_dup_label net in
    let rec build count port net =
      if count = 1 then ([ port ], net)
      else
        let left_count = count / 2 in
        let right_count = count - left_count in
        let dup, net = Interaction_net.new_labeled_dup label net in
        let net = Interaction_net.connect (dup, 0) port net in
        let left, net = build left_count (dup, 1) net in
        let right, net = build right_count (dup, 2) net in
        (left @ right, net)
    in
    build count port net

let bind_for_body name port body env net =
  ensure_fresh name env;
  let ports, net = distribute_port (count_uses name body) port net in
  let env = if ports = [] then env else StringMap.add name ports env in
  (env, net)

let rec free_vars bound = function
  | Ast.Int _ -> StringSet.empty
  | Ast.Var name ->
      if StringSet.mem name bound then StringSet.empty else StringSet.singleton name
  | Ast.Succ expr -> free_vars bound expr
  | Ast.Constr (_, args) ->
      List.fold_left
        (fun free arg -> StringSet.union free (free_vars bound arg))
        StringSet.empty args
  | Ast.Let (name, value, body) ->
      StringSet.union (free_vars bound value)
        (free_vars (StringSet.add name bound) body)
  | Ast.Dup (value, left, right, body) ->
      StringSet.union (free_vars bound value)
        (free_vars (bound |> StringSet.add left |> StringSet.add right) body)
  | Ast.Drop (value, body) ->
      StringSet.union (free_vars bound value) (free_vars bound body)
  | Ast.Lambda (params, body) ->
      free_vars
        (List.fold_left (fun bound param -> StringSet.add param bound) bound params)
        body
  | Ast.Closure (_, _, captures, _) ->
      List.fold_left (fun free name -> StringSet.add name free) StringSet.empty captures
  | Ast.Apply (callee, args) ->
      List.fold_left
        (fun free arg -> StringSet.union free (free_vars bound arg))
        (free_vars bound callee) args

let closure_convert_program program =
  let rec pattern_names = function
    | Ast.PZero | Ast.PWildcard -> []
    | Ast.PSucc name | Ast.PVar name -> [ name ]
    | Ast.PConstr (_, patterns) -> List.concat (List.map pattern_names patterns)
  in
  let next_id = ref 0 in
  let rec convert bound = function
    | (Ast.Int _ as expr) -> expr
    | (Ast.Var _ as expr) -> expr
    | Ast.Succ expr -> Ast.Succ (convert bound expr)
    | Ast.Constr (name, args) -> Ast.Constr (name, List.map (convert bound) args)
    | Ast.Let (name, value, body) ->
        Ast.Let (name, convert bound value, convert (StringSet.add name bound) body)
    | Ast.Dup (value, left, right, body) ->
        let body_bound = bound |> StringSet.add left |> StringSet.add right in
        Ast.Dup (convert bound value, left, right, convert body_bound body)
    | Ast.Drop (value, body) -> Ast.Drop (convert bound value, convert bound body)
    | Ast.Lambda (params, body) ->
        ensure_distinct params "lambda parameter";
        let body_bound =
          List.fold_left (fun names param -> StringSet.add param names) bound params
        in
        let body = convert body_bound body in
        let param_set = List.fold_left (fun set p -> StringSet.add p set) StringSet.empty params in
        let captures =
          StringSet.inter bound (free_vars param_set body) |> StringSet.elements
        in
        let id = !next_id in
        incr next_id;
        Ast.Closure (id, params, captures, body)
    | Ast.Closure _ -> invalid_arg "closure_convert_program"
    | Ast.Apply (callee, args) ->
        Ast.Apply (convert bound callee, List.map (convert bound) args)
  in
  let definitions =
    List.map
      (fun def ->
        let bound =
          List.fold_left
            (fun names pattern ->
              List.fold_left (fun names n -> StringSet.add n names) names
                (pattern_names pattern))
            StringSet.empty def.Ast.patterns
        in
        { def with Ast.body = convert bound def.Ast.body })
      program.Ast.definitions
  in
  { program with Ast.definitions; main = convert StringSet.empty program.Ast.main }

let closure_symbol id = Printf.sprintf "$closure:%d" id
let function_symbol name = Printf.sprintf "$function:%s" name
let apply_symbol arity = Printf.sprintf "$apply:%d" arity

let rec compile_expr ctx env net = function
  | Ast.Int n ->
      let id, net = Interaction_net.build_num n net in
      ((id, 0), env, net)
  | Ast.Var name -> (
      match StringMap.find_opt name env with
      | Some _ ->
          let port, env = consume name env in
          (port, env, net)
      | None when FunctionMap.mem name ctx.functions ->
          let closure, net = Interaction_net.new_agent (function_symbol name) 0 net in
          ((closure, 0), env, net)
      | None -> Ravel_error.compile_error (Printf.sprintf "undefined variable '%s'" name))
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
      let env_with_binding, net = bind_for_body name value_port body env net in
      let result_port, env_after, net = compile_expr ctx env_with_binding net body in
      (result_port, StringMap.remove name env_after, net)
  | Ast.Dup (value, left, right, body) ->
      ensure_fresh left env;
      ensure_fresh right env;
      if left = right then
        Ravel_error.compile_error "dup binders must be distinct";
      let value_port, env, net = compile_expr ctx env net value in
      let dup_id, net = Interaction_net.new_dup net in
      let net = Interaction_net.connect (dup_id, 0) value_port net in
      let env_with_dups, net = bind_for_body left (dup_id, 1) body env net in
      let env_with_dups, net =
        bind_for_body right (dup_id, 2) body env_with_dups net
      in
      let result_port, env_after, net = compile_expr ctx env_with_dups net body in
      let env_after = env_after |> StringMap.remove left |> StringMap.remove right in
      (result_port, env_after, net)
  | Ast.Drop (value, body) ->
      let value_port, env, net = compile_expr ctx env net value in
      let net = erase_port value_port net in
      compile_expr ctx env net body
  | Ast.Lambda _ -> invalid_arg "compile_expr: unconverted lambda"
  | Ast.Closure (id, _params, captures, _body) ->
      let capture_ports, env, net =
        List.fold_left
          (fun (ports, env, net) capture ->
            let port, env, net = compile_expr ctx env net (Ast.Var capture) in
            (port :: ports, env, net))
          ([], env, net) captures
      in
      let capture_ports = List.rev capture_ports in
      let closure, net =
        Interaction_net.new_agent (closure_symbol id) (List.length captures) net
      in
      let net =
        List.fold_left2
          (fun net slot port -> Interaction_net.connect (closure, slot) port net)
          net
          (List.init (List.length captures) (fun index -> index + 1))
          capture_ports
      in
      ((closure, 0), env, net)
  | Ast.Apply (Ast.Var "add", args) when not (StringMap.mem "add" env) ->
      compile_add ctx env net args
  | Ast.Apply (Ast.Var name, _)
    when not (StringMap.mem name env) && not (FunctionMap.mem name ctx.functions) ->
      Ravel_error.compile_error (Printf.sprintf "undefined function '%s'" name)
  | Ast.Apply (callee, args) -> compile_apply ctx env net callee args

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

and compile_apply ctx env net callee args =
  let callee_port, env, net = compile_expr ctx env net callee in
  let arg_ports, env, net =
    List.fold_left
      (fun (ports, env, net) arg ->
        let port, env, net = compile_expr ctx env net arg in
        (port :: ports, env, net))
      ([], env, net) args
  in
  let arg_ports = List.rev arg_ports in
  let apply, net =
    Interaction_net.new_agent (apply_symbol (List.length args))
      (List.length args + 1) net
  in
  let net = Interaction_net.connect (apply, 0) callee_port net in
  let net =
    List.fold_left2
      (fun net slot port -> Interaction_net.connect (apply, slot) port net)
      net
      (List.init (List.length arg_ports) (fun index -> index + 1))
      arg_ports
  in
  ((apply, List.length args + 1), env, net)

and compile_leaf ctx row values output net =
  if List.length row.patterns <> List.length values then
    invalid_arg "compile_leaf";
  let env, net =
    List.fold_left2
      (fun (env, net) pattern port ->
        match pattern with
        | Ast.PVar name -> bind_for_body name port row.body env net
        | Ast.PWildcard -> (env, erase_port port net)
        | _ -> invalid_arg "compile_leaf: refutable pattern remains")
      (StringMap.empty, net) row.patterns values
  in
  let result, _env_after, net = compile_expr ctx env net row.body in
  Interaction_net.connect result output net

and instantiate_tree ctx tree values output net =
  match tree with
  | Leaf row -> compile_leaf ctx row values output net
  | Switch { symbol; column; preserve; _ } ->
      let selected = List.nth values column in
      let others = List.filteri (fun index _ -> index <> column) values in
      let inspected, auxiliaries, net =
        if preserve then
          let dup, net = Interaction_net.new_dup net in
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

let validate_exhaustiveness families function_name patterns has_fallback =
  if not has_fallback then
    let constructor_names =
      List.filter_map (fun pattern -> Option.map fst (constructor_at pattern)) patterns
    in
    let referenced_families =
      List.fold_left
        (fun referenced constructor ->
          match ConstructorMap.find_opt constructor families with
          | None -> referenced
          | Some family ->
              if
                List.exists
                  (fun existing -> existing.family_name = family.family_name)
                  referenced
              then referenced
              else family :: referenced)
        [] constructor_names
    in
    match referenced_families with
    | [] -> ()
    | [ family ] ->
        let missing =
          List.filter
            (fun constructor -> not (List.mem constructor constructor_names))
            family.family_constructors
        in
        if missing <> [] then
          Ravel_error.compile_error
            (Printf.sprintf
               "function '%s' is non-exhaustive for data type '%s'; missing constructor(s): %s, or include a variable or '_' fallback"
               function_name family.family_name (String.concat ", " missing))
    | families ->
        let names =
          families |> List.map (fun family -> family.family_name) |> List.sort_uniq compare
        in
        Ravel_error.compile_error
          (Printf.sprintf
             "function '%s' mixes constructors from different data types in one pattern column: %s"
             function_name (String.concat ", " names))

let fresh_match_symbol =
  let next = ref 0 in
  fun function_name ->
    let id = !next in
    incr next;
    Printf.sprintf "$match:%s:%d" function_name id

let rec build_decision_tree constructors families function_name rows =
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
      validate_exhaustiveness families function_name column_patterns has_fallback;
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
                    build_decision_tree constructors families function_name specialized ))
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

let build_function_specs definitions constructors families =
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
      let tree = build_decision_tree constructors families name rows in
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
  | Ast.Lambda (_, body) | Ast.Closure (_, _, _, body) ->
      collect_expr_constructors constructors body
  | Ast.Apply (callee, args) ->
      List.fold_left collect_expr_constructors
        (collect_expr_constructors constructors callee) args

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

let build_constructor_families type_definitions =
  let nat = { family_name = "Nat"; family_constructors = [ "Z"; "S" ] } in
  let initial_families =
    ConstructorMap.empty |> ConstructorMap.add "Z" nat
    |> ConstructorMap.add "S" nat
  in
  let initial_type_names = StringMap.singleton "Nat" () in
  let _, families =
    List.fold_left
      (fun (type_names, families) def ->
        if StringMap.mem def.Ast.type_name type_names then
          Ravel_error.compile_error
            (Printf.sprintf "data type '%s' is declared more than once"
               def.Ast.type_name);
        let family =
          {
            family_name = def.Ast.type_name;
            family_constructors = def.Ast.constructors;
          }
        in
        let families =
          List.fold_left
            (fun families constructor ->
              if List.mem constructor reserved_constructor_names then
                Ravel_error.compile_error
                  (Printf.sprintf "constructor name '%s' is reserved" constructor);
              match ConstructorMap.find_opt constructor families with
              | Some existing ->
                  Ravel_error.compile_error
                    (Printf.sprintf
                       "constructor '%s' already belongs to data type '%s'"
                       constructor existing.family_name)
              | None -> ConstructorMap.add constructor family families)
            families def.Ast.constructors
        in
        (StringMap.add def.Ast.type_name () type_names, families))
      (initial_type_names, initial_families) type_definitions
  in
  families

let collect_closures program =
  let rec collect acc = function
    | Ast.Int _ | Ast.Var _ -> acc
    | Ast.Succ expr -> collect acc expr
    | Ast.Constr (_, args) -> List.fold_left collect acc args
    | Ast.Let (_, value, body) | Ast.Drop (value, body) ->
        collect (collect acc value) body
    | Ast.Dup (value, _, _, body) -> collect (collect acc value) body
    | Ast.Lambda _ -> invalid_arg "collect_closures: unconverted lambda"
    | Ast.Closure (id, params, captures, body) ->
        let spec =
          { closure_id = id; params; captures; closure_body = body }
        in
        collect (spec :: acc) body
    | Ast.Apply (callee, args) -> List.fold_left collect (collect acc callee) args
  in
  let acc =
    List.fold_left (fun acc def -> collect acc def.Ast.body) []
      program.Ast.definitions
  in
  List.rev (collect acc program.Ast.main)

let bind_closure_inputs body names ports net =
  List.fold_left2
    (fun (env, net) name port -> bind_for_body name port body env net)
    (StringMap.empty, net) names ports

let install_closure_rules ctx =
  List.iter
    (fun spec ->
      let symbol = closure_symbol spec.closure_id in
      Interaction_net.register_value_symbol ctx.rulebook symbol
        (List.length spec.captures);
      Interaction_net.def_rule ctx.rulebook symbol
        (apply_symbol (List.length spec.params))
        (fun captures interface net ->
          let args, output = split_last interface in
          let names = spec.captures @ spec.params in
          let ports = captures @ args in
          let env, net = bind_closure_inputs spec.closure_body names ports net in
          let result, _env, net = compile_expr ctx env net spec.closure_body in
          Interaction_net.connect result output net))
    ctx.closures;
  FunctionMap.iter
    (fun name spec ->
      let symbol = function_symbol name in
      Interaction_net.register_value_symbol ctx.rulebook symbol 0;
      Interaction_net.def_rule ctx.rulebook symbol (apply_symbol spec.arity)
        (fun _closure interface net ->
          let args, output = split_last interface in
          instantiate_tree ctx spec.tree args output net))
    ctx.functions

let build_context type_definitions definitions constructors closures =
  let families = build_constructor_families type_definitions in
  let functions = build_function_specs definitions constructors families in
  let rulebook = Interaction_net.base_rulebook () in
  ConstructorMap.iter
    (fun name arity ->
      if name <> "Z" && name <> "S" then
        Interaction_net.install_constructor_rules rulebook name arity)
    constructors;
  let ctx = { rulebook; functions; constructors; closures } in
  FunctionMap.iter (fun _ spec -> install_function_rules ctx spec) functions;
  install_closure_rules ctx;
  ctx

let compile program =
  let program = closure_convert_program program in
  let constructors = collect_program_constructors program in
  let closures = collect_closures program in
  let ctx =
    build_context program.Ast.type_definitions program.Ast.definitions constructors
      closures
  in
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
