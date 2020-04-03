/* -------------------------------------------------------------------------- */
/* Integer Map Key */

module Int = {
  type t = int;
  let compare = Pervasives.compare;
};

/* -------------------------------------------------------------------------- */
/* Action */

type action =
  | Bot
  | Sub
  | Add;

let show_action = act =>
  switch (act) {
  | Bot => {j|⊥|j}
  | Sub => {j|⊖|j}
  | Add => {j|⊕|j}
  };

/* -------------------------------------------------------------------------- */
/* Edge */

module Edge = {
  type t =
    | Root(list(int))
    | Edge(int, list(int));
  let compare = Pervasives.compare;
  let last_index = ref(1);
  let next_index = () => {
    last_index := last_index^ + 1;
    last_index^;
  };
  let show_edge = e =>
    switch (e) {
    | Root(children) =>
      let cs = List.map(Printf.sprintf("%i"), children);
      Printf.sprintf("Root([%s])", String.concat(", ", cs));
    | Edge(parent, children) =>
      let cs = List.map(Printf.sprintf("%i"), children);
      Printf.sprintf("Edge(%i, [%s])", parent, String.concat(", ", cs));
    };
};

open Edge;

/* -------------------------------------------------------------------------- */
/* Graph */

module Graph = Map.Make(Int);

let root = 1;
let edges: ref(Graph.t((Edge.t, action))) =
  ref(Graph.(empty |> add(root, (Root([]), Bot))));

let get_edge = key => edges^ |> Graph.find(key);

let update_edge = (key, update) => {
  let def = get_edge(key);
  edges := edges^ |> Graph.remove(key);
  edges := edges^ |> Graph.add(key, update(def));
};

let update_action = (key, act) => {
  let update = ((e, a)) => (
    e,
    switch (a, act) {
    | (Bot, _) => act
    | (_, Bot) => a
    | (Sub, Sub) => Sub
    | (Add, _) => Add
    | (_, Add) => Add
    },
  );
  update_edge(key, update);
};

let make_edge = parent => {
  let key = next_index();
  edges := edges^ |> Graph.add(key, (Edge(parent, []), Bot));
  let update = ((e, a)) =>
    switch (e) {
    | Root(cs) => (Root(List.concat([cs, [key]])), a)
    | Edge(p, cs) => (Edge(p, List.concat([cs, [key]])), a)
    };
  update_edge(parent, update);
  key;
};

let print_graph = () => {
  let print_edge = (i, (e, a)) =>
    Printf.printf("%i = %s = %s\n", i, show_action(a), show_edge(e));
  print_string("--------------------\n");
  edges^ |> Graph.iter(print_edge);
};

/* -------------------------------------------------------------------------- */
/* Example */

print_graph();

let e1 = make_edge(root);
let e2 = make_edge(root);

print_graph();

let e3 = make_edge(e1);
let e4 = make_edge(e1);

update_action(e1, Sub);
update_action(e2, Add);

print_graph();

update_action(e1, Add);
update_action(e2, Sub);

print_graph();

update_action(e1, Sub);
update_action(e2, Sub);

print_graph();
