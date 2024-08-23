open Yojson.Basic.Util
open Expr

(* JSON representation of const *)
let json_of_const c = match c with
  | Int x -> `Assoc [("Int", `Int x)]
  | Real x -> `Assoc [("Real", `Float x)]
  | String x -> `Assoc [("String", `String x)]
  | Bool x -> `Assoc [("Bool", `Bool x)]
  | Null -> `Assoc [("Null", `Null)]

(* JSON representation of var *)
let json_of_var v = match v with
  | NamedVar x -> `Assoc [("NamedVar", `String x)]
  | NumberedVar x -> `Assoc [("NumberedVar", `Int x)]
  | ConstVar c -> `Assoc [("ConstVar", json_of_const c)]
  | AnonVar -> `Assoc [("AnonVar", `String "_")]
  | AggVar (fn, vn) -> `Assoc [("AggVar", `Assoc [("fn", `String fn); ("vn", `String vn)])]

(* JSON representation of vterm *)
let rec json_of_vterm vt = match vt with
  | Const c -> `Assoc [("Const", json_of_const c)]
  | Var v -> `Assoc [("Var", json_of_var v)]
  | BinaryOp (op, vt1, vt2) ->
      `Assoc [("BinaryOp", `Assoc [("op", `String op); ("left", json_of_vterm vt1); ("right", json_of_vterm vt2)])]
  | UnaryOp (op, vt) -> `Assoc [("UnaryOp", `Assoc [("op", `String op); ("operand", json_of_vterm vt)])]

(* JSON representation of eterm *)
let json_of_eterm et = match et with
  | Equation (op, vt1, vt2) ->
      `Assoc [("Equation", `Assoc [("op", `String op); ("left", json_of_vterm vt1); ("right", json_of_vterm vt2)])]

(* JSON representation of rterm *)
let json_of_rterm rt = match rt with
  | Pred (pn, vars) ->
      `Assoc [("Pred", `Assoc [("predicate_name", `String pn); ("variables", `List (List.map json_of_var vars))])]
  | Deltainsert (pn, vars) ->
      `Assoc [("Deltainsert", `Assoc [("predicate_name", `String pn); ("variables", `List (List.map json_of_var vars))])]
  | Deltadelete (pn, vars) ->
      `Assoc [("Deltadelete", `Assoc [("predicate_name", `String pn); ("variables", `List (List.map json_of_var vars))])]

(* JSON representation of term *)
let json_of_term t = match t with
  | Rel rt -> `Assoc [("Rel", json_of_rterm rt)]
  | Not rt -> `Assoc [("Not", json_of_rterm rt)]
  | Equat et -> `Assoc [("Equat", json_of_eterm et)]
  | Noneq et -> `Assoc [("Noneq", json_of_eterm et)]

(* JSON representation of stype *)
let json_of_stype st = match st with
  | Sint -> `String "int"
  | Sreal -> `String "real"
  | Sstring -> `String "string"
  | Sbool -> `String "bool"

(* JSON representation of rule *)
let json_of_rule (rt, terms) =
  `Assoc [("rule", `Assoc [("rterm", json_of_rterm rt); ("terms", `List (List.map json_of_term terms))])]

(* JSON representation of fact *)
let json_of_fact rt = `Assoc [("fact", json_of_rterm rt)]

(* JSON representation of query *)
let json_of_query = function
  | Some q -> `Assoc [("query", json_of_rterm q)]
  | None -> `Null

(* JSON representation of source *)
let json_of_source (name, lst) =
  `Assoc [
    ("source", 
      `Assoc [
        ("name", `String name); 
        ("columns", 
          `Assoc (List.map (fun (col, typ) -> (col, json_of_stype typ)) lst)
        )
      ]
    )
  ]

(* JSON representation of view *)
let json_of_view = function
  | Some (name, lst) ->
      `Assoc [
        ("view", 
          `Assoc [
            ("name", `String name); 
            ("columns", 
              `Assoc (List.map (fun (col, typ) -> (col, json_of_stype typ)) lst)
            )
          ]
        )
      ]
  | None -> `Null


(* JSON representation of constraint' *)
let json_of_constraint' (rt, terms) =
  `Assoc [("constraint", `Assoc [("rterm", json_of_rterm rt); ("terms", `List (List.map json_of_term terms))])]

(* JSON representation of primary_key *)
let json_of_primary_key (name, attrs) =
  `Assoc [("primary_key", `Assoc [("name", `String name); ("attributes", `List (List.map (fun att -> `String att) attrs))])]

(* JSON representation of expr *)
let json_of_expr e =
  `Assoc [
    ("rules", `List (List.map json_of_rule e.rules));
    ("facts", `List (List.map json_of_fact e.facts));
    ("query", json_of_query e.query);
    ("sources", `List (List.map json_of_source e.sources));
    ("view", json_of_view e.view);
    ("constraints", `List (List.map json_of_constraint' e.constraints));
    ("primary_keys", `List (List.map json_of_primary_key e.primary_keys))
  ]