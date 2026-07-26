module PortMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module IntMap = Map.Make (Int)

type port = int * int

type dup_label = int

type agent_identity = Named of string | Dup of dup_label

type agent = { symbol : agent_identity; arity : int }

type value = VConstr of string * value list

type net = {
  agents : agent IntMap.t;
  wires : port PortMap.t;
  next_id : int;
  next_dup_label : dup_label;
}

let empty_net =
  { agents = IntMap.empty; wires = PortMap.empty; next_id = 0; next_dup_label = 0 }

let add_agent symbol arity net =
  if arity < 0 then invalid_arg "agent arity must be non-negative";
  let id = net.next_id in
  let net =
    {
      net with
      agents = IntMap.add id { symbol; arity } net.agents;
      next_id = id + 1;
    }
  in
  (id, net)

let fresh_dup_label net =
  let label = net.next_dup_label in
  (label, { net with next_dup_label = label + 1 })

let new_labeled_dup_agent label net =
  if label < 0 then invalid_arg "duplicator label must be non-negative";
  let net =
    if label < net.next_dup_label then net
    else { net with next_dup_label = label + 1 }
  in
  add_agent (Dup label) 2 net

let new_dup_agent net =
  let label, net = fresh_dup_label net in
  new_labeled_dup_agent label net

(* Short aliases intended for compiler code.  The longer names make the fact
   that these functions allocate agents (rather than labels alone) explicit. *)
let new_dup = new_dup_agent
let new_labeled_dup = new_labeled_dup_agent

let new_agent symbol arity net =
  if symbol = "Dup" then
    if arity = 2 then new_dup_agent net
    else invalid_arg "new_agent: Dup always has arity 2; use new_dup_agent"
  else add_agent (Named symbol) arity net

let connect p1 p2 net =
  { net with wires = net.wires |> PortMap.add p1 p2 |> PortMap.add p2 p1 }

let remove_agent id net =
  let a = IntMap.find id net.agents in
  let wires =
    List.fold_left
      (fun w slot -> PortMap.remove (id, slot) w)
      net.wires
      (List.init (a.arity + 1) Fun.id)
  in
  { net with agents = IntMap.remove id net.agents; wires }

type rule = port list -> port list -> net -> net

type rulebook = {
  ordinary_rules : (string * string, rule) Hashtbl.t;
  value_symbols : (string, int) Hashtbl.t;
}

let create_rulebook () : rulebook =
  { ordinary_rules = Hashtbl.create 32; value_symbols = Hashtbl.create 32 }

let def_rule rules s1 s2 (f : rule) =
  if s1 = "Dup" || s2 = "Dup" then
    invalid_arg
      "def_rule: Dup is runtime-labeled; use install_value_rules for structural duplication";
  Hashtbl.replace rules.ordinary_rules (s1, s2) f;
  if s1 <> s2 then
    Hashtbl.replace rules.ordinary_rules (s2, s1)
      (fun ifc2 ifc1 net -> f ifc1 ifc2 net)

let erase_port port net =
  let e, net = new_agent "Era" 0 net in
  connect (e, 0) port net

let install_value_rules rules name arity =
  if name = "Dup" then
    invalid_arg "install_value_rules: Dup is reserved for runtime duplicators";
  if arity < 0 then invalid_arg "install_value_rules: arity must be non-negative";
  (match Hashtbl.find_opt rules.value_symbols name with
  | Some registered_arity when registered_arity <> arity ->
      invalid_arg
        (Printf.sprintf
           "install_value_rules: %s was already registered with arity %d (not %d)"
           name registered_arity arity)
  | _ -> Hashtbl.replace rules.value_symbols name arity);
  def_rule rules name "Era" (fun ifcValue _ifcEra net ->
      List.fold_left (fun net field -> erase_port field net) net ifcValue)

(* Constructor values are one instance of the generic structural-value API. *)
let install_constructor_rules = install_value_rules
let register_value_symbol = install_value_rules

let install_builtin_rules rules =
  def_rule rules "Z" "Add" (fun _ifcZ ifcAdd net ->
      match ifcAdd with
      | [ y; r ] -> connect y r net
      | _ -> assert false);
  def_rule rules "S" "Add" (fun ifcS ifcAdd net ->
      match (ifcS, ifcAdd) with
      | [ x' ], [ y; r ] ->
          let s_id, net = new_agent "S" 1 net in
          let add_id, net = new_agent "Add" 2 net in
          net
          |> connect (s_id, 0) r
          |> connect (add_id, 0) x'
          |> connect (add_id, 1) y
          |> connect (add_id, 2) (s_id, 1)
      | _ -> assert false);
  install_value_rules rules "Z" 0;
  install_value_rules rules "S" 1

let base_rulebook () =
  let rules = create_rulebook () in
  install_builtin_rules rules;
  rules

let structural_dup_rule name arity label ifcValue ifcDup net =
  match ifcDup with
  | [ o1; o2 ] ->
      if List.length ifcValue <> arity then
        failwith
          (Printf.sprintf
             "registered value %s/%d encountered with %d auxiliary ports" name arity
             (List.length ifcValue));
      let v1, net = new_agent name arity net in
      let v2, net = new_agent name arity net in
      let net = net |> connect (v1, 0) o1 |> connect (v2, 0) o2 in
      let rec duplicate_fields slot fields net =
        match fields with
        | [] -> net
        | field :: rest ->
            let dup, net = new_labeled_dup_agent label net in
            let net =
              net
              |> connect (dup, 0) field
              |> connect (dup, 1) (v1, slot)
              |> connect (dup, 2) (v2, slot)
            in
            duplicate_fields (slot + 1) rest net
      in
      duplicate_fields 1 ifcValue net
  | _ -> assert false

let annihilate_dups ifc1 ifc2 net =
  match (ifc1, ifc2) with
  | [ x1; x2 ], [ y1; y2 ] -> net |> connect x1 y1 |> connect x2 y2
  | _ -> assert false

let commute_dups label1 label2 ifc1 ifc2 net =
  match (ifc1, ifc2) with
  | [ x1; x2 ], [ y1; y2 ] ->
      let across_x1, net = new_labeled_dup_agent label2 net in
      let across_x2, net = new_labeled_dup_agent label2 net in
      let across_y1, net = new_labeled_dup_agent label1 net in
      let across_y2, net = new_labeled_dup_agent label1 net in
      net
      |> connect (across_x1, 0) x1
      |> connect (across_x2, 0) x2
      |> connect (across_y1, 0) y1
      |> connect (across_y2, 0) y2
      |> connect (across_x1, 1) (across_y1, 1)
      |> connect (across_x1, 2) (across_y2, 1)
      |> connect (across_x2, 1) (across_y1, 2)
      |> connect (across_x2, 2) (across_y2, 2)
  | _ -> assert false

let string_of_agent_identity = function
  | Named name -> name
  | Dup label -> Printf.sprintf "Dup[%d]" label

let dynamic_rule rules a b =
  match (a.symbol, b.symbol) with
  | Named s1, Named s2 -> Hashtbl.find_opt rules.ordinary_rules (s1, s2)
  | Named name, Dup label -> (
      match Hashtbl.find_opt rules.value_symbols name with
      | Some arity -> Some (structural_dup_rule name arity label)
      | None -> None)
  | Dup label, Named name -> (
      match Hashtbl.find_opt rules.value_symbols name with
      | Some arity ->
          Some (fun ifcDup ifcValue net ->
              structural_dup_rule name arity label ifcValue ifcDup net)
      | None -> None)
  | Dup label1, Dup label2 ->
      if label1 = label2 then Some annihilate_dups
      else Some (commute_dups label1 label2)

let find_all_active_pairs net =
  IntMap.fold
    (fun aid _ acc ->
      match PortMap.find_opt (aid, 0) net.wires with
      | Some (bid, 0) when aid < bid -> (aid, bid) :: acc
      | _ -> acc)
    net.agents []

let step ~rules ~pick net =
  match find_all_active_pairs net with
  | [] -> None
  | pairs -> (
      let a1, b1 = pick pairs in
      let a = IntMap.find a1 net.agents in
      let b = IntMap.find b1 net.agents in
      match dynamic_rule rules a b with
      | None ->
          failwith
            (Printf.sprintf "No rule defined for %s >< %s"
               (string_of_agent_identity a.symbol)
               (string_of_agent_identity b.symbol))
      | Some rule ->
          let ifc1 = List.init a.arity (fun i -> PortMap.find (a1, i + 1) net.wires) in
          let ifc2 = List.init b.arity (fun i -> PortMap.find (b1, i + 1) net.wires) in
          let net = remove_agent a1 net in
          let net = remove_agent b1 net in
          let fired =
            (string_of_agent_identity a.symbol, string_of_agent_identity b.symbol)
          in
          Some (rule ifc1 ifc2 net, fired))

let run ~rules ~pick net =
  let rec loop net log =
    match step ~rules ~pick net with
    | None -> (net, List.rev log)
    | Some (net', fired) -> loop net' (fired :: log)
  in
  loop net []

let show_log log =
  log
  |> List.map (fun (s1, s2) -> Printf.sprintf "%s><%s" s1 s2)
  |> String.concat ", "

let pick_first pairs = List.hd pairs
let pick_random pairs = List.nth pairs (Random.int (List.length pairs))

let rec build_num n net =
  if n < 0 then invalid_arg "build_num expects a natural number"
  else if n = 0 then new_agent "Z" 0 net
  else
    let rest, net = build_num (n - 1) net in
    let s, net = new_agent "S" 1 net in
    let net = connect (s, 1) (rest, 0) net in
    (s, net)

let rec readback_value net (aid, slot) =
  if slot <> 0 then failwith "readback expects a principal port";
  let a = IntMap.find aid net.agents in
  let fields =
    List.init a.arity (fun i -> readback_value net (PortMap.find (aid, i + 1) net.wires))
  in
  VConstr (string_of_agent_identity a.symbol, fields)

let rec int_of_value_opt = function
  | VConstr ("Z", []) -> Some 0
  | VConstr ("S", [ value ]) ->
      Option.map (fun n -> n + 1) (int_of_value_opt value)
  | _ -> None

let rec value_to_string value =
  match int_of_value_opt value with
  | Some n -> string_of_int n
  | None ->
      match value with
      | VConstr (name, []) -> name
      | VConstr (name, fields) ->
          Printf.sprintf "%s(%s)" name
            (fields |> List.map value_to_string |> String.concat ", ")

let readback net port =
  match int_of_value_opt (readback_value net port) with
  | Some n -> n
  | None -> failwith "readback: normal form is not a nat"

let make_root net = new_agent "Root" 1 net

let attach_root port net =
  let root, net = make_root net in
  let net = connect (root, 1) port net in
  (root, net)

let observe_root root net = PortMap.find (root, 1) net.wires

let string_of_port (aid, slot) = Printf.sprintf "%d:%d" aid slot

let dump_net net =
  let agents =
    net.agents
    |> IntMap.bindings
    |> List.map (fun (id, agent) ->
           Printf.sprintf "%d %s/%d" id
             (string_of_agent_identity agent.symbol)
             agent.arity)
    |> String.concat "\n"
  in
  let wires =
    net.wires
    |> PortMap.bindings
    |> List.filter (fun ((aid, slot), (bid, bslot)) ->
           aid < bid || (aid = bid && slot < bslot))
    |> List.map (fun (p1, p2) ->
           Printf.sprintf "%s -- %s" (string_of_port p1) (string_of_port p2))
    |> String.concat "\n"
  in
  Printf.sprintf "Agents:\n%s\n\nWires:\n%s\n" agents wires
