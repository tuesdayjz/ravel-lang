module PortMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module IntMap = Map.Make (Int)

type port = int * int

type agent = { symbol : string; arity : int }

type net = {
  agents : agent IntMap.t;
  wires : port PortMap.t;
  next_id : int;
}

let empty_net = { agents = IntMap.empty; wires = PortMap.empty; next_id = 0 }

let new_agent symbol arity net =
  let id = net.next_id in
  let net =
    {
      net with
      agents = IntMap.add id { symbol; arity } net.agents;
      next_id = id + 1;
    }
  in
  (id, net)

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
type rulebook = (string * string, rule) Hashtbl.t

let create_rulebook () : rulebook = Hashtbl.create 32

let def_rule rules s1 s2 (f : rule) =
  Hashtbl.replace rules (s1, s2) f;
  if s1 <> s2 then
    Hashtbl.replace rules (s2, s1) (fun ifc2 ifc1 net -> f ifc1 ifc2 net)

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
  def_rule rules "Z" "Dup" (fun _ifcZ ifcDup net ->
      match ifcDup with
      | [ o1; o2 ] ->
          let z1, net = new_agent "Z" 0 net in
          let z2, net = new_agent "Z" 0 net in
          net |> connect (z1, 0) o1 |> connect (z2, 0) o2
      | _ -> assert false);
  def_rule rules "S" "Dup" (fun ifcS ifcDup net ->
      match (ifcS, ifcDup) with
      | [ x' ], [ o1; o2 ] ->
          let s1, net = new_agent "S" 1 net in
          let s2, net = new_agent "S" 1 net in
          let d, net = new_agent "Dup" 2 net in
          net
          |> connect (s1, 0) o1
          |> connect (s2, 0) o2
          |> connect (d, 0) x'
          |> connect (d, 1) (s1, 1)
          |> connect (d, 2) (s2, 1)
      | _ -> assert false);
  def_rule rules "Z" "Era" (fun _ _ net -> net);
  def_rule rules "S" "Era" (fun ifcS _ifcEra net ->
      match ifcS with
      | [ x' ] ->
          let e, net = new_agent "Era" 0 net in
          connect (e, 0) x' net
      | _ -> assert false)

let base_rulebook () =
  let rules = create_rulebook () in
  install_builtin_rules rules;
  rules

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
      match Hashtbl.find_opt rules (a.symbol, b.symbol) with
      | None ->
          failwith
            (Printf.sprintf "No rule defined for %s >< %s" a.symbol b.symbol)
      | Some rule ->
          let ifc1 = List.init a.arity (fun i -> PortMap.find (a1, i + 1) net.wires) in
          let ifc2 = List.init b.arity (fun i -> PortMap.find (b1, i + 1) net.wires) in
          let net = remove_agent a1 net in
          let net = remove_agent b1 net in
          Some (rule ifc1 ifc2 net, (a.symbol, b.symbol)))

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

let rec readback net (aid, slot) =
  if slot <> 0 then failwith "readback expects a principal port";
  let a = IntMap.find aid net.agents in
  match a.symbol with
  | "Z" -> 0
  | "S" -> 1 + readback net (PortMap.find (aid, 1) net.wires)
  | s -> failwith ("readback: unexpected symbol " ^ s ^ " (normal form not a nat?)")

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
           Printf.sprintf "%d %s/%d" id agent.symbol agent.arity)
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
