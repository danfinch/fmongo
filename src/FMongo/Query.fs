namespace FMongo

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Builders
open Microsoft.FSharp.Quotations
open FMongo.Util
open FMongo.Mapping
open System.Collections

type private FV = FSharpValue
type private FT = FSharpType
type private Q = Query
type private QC = QueryComplete

module P = Patterns
module DP = DerivedPatterns

module Query =
  let Exists o (b : bool) = false
  let Matches o (r : string) = false
  let All e z = false
  let In z (s : #seq<_>) = false
  let NotIn e (s : #seq<_>) = false
  let Size e (z:int) = false

  let rec eval expr = 
    match expr with 
    | P.Value(value,_) -> value //value 
    | P.PropertyGet(Some(instance), pi, args) -> //instance property get 
      pi.GetValue(eval instance, evalAll args) //notice recursive eval of instance expression and arg expressions 
    | P.PropertyGet(None, pi, args) -> //static property get 
      pi.GetValue(pi, evalAll args) 
    | P.Call(Some(instance), mi, args) -> //instance call 
      mi.Invoke(eval instance, evalAll args) 
    | P.Call(None, mi, args) -> //static call 
      mi.Invoke(null, evalAll args) 
    | P.NewUnionCase(uci, exprs) -> // list
      FV.MakeUnion(uci, evalAll exprs)
    | P.NewRecord(t, exprs) -> // record
      FV.MakeRecord(t, evalAll exprs)
    | P.NewTuple(e) -> // tuple
      let r = evalAll e
      let types = r |> Array.map(fun o -> o.GetType())
      let t = FT.MakeTupleType types
      FV.MakeTuple(r, t)
    | P.NewArray(t, exprs) -> // array
      evalAll exprs :> obj
    | _ -> failwith "(eval) Invalid expression"
  and evalAll exprs = 
    exprs |> Seq.map eval |> Seq.toArray 

  let rec evalSimple expr =
    match expr with
      | P.Value(value,_) -> string value //value 
      | P.PropertyGet(Some(instance), pi, args) -> pi.Name //instance property get 
      | _ -> failwith "(evalSimple) Invalid expression"

  let (|Eval|) expr = eval expr 
  let (|Simple|) expr = evalSimple expr

  let build (e : Expr<'a -> bool>) =
    let rec build' e =
      match e with
      | P.Lambda(v, e) -> build' e
      | DP.AndAlso(arg1, arg2) -> Q.And(build' arg1, build' arg2)
      | DP.OrElse(arg1, arg2) -> Q.Or(build' arg1, build' arg2)
      | P.Call(_, mi, [Simple(left); Eval(right)]) -> 
        let encodeType s = s |> Seq.map(fun v -> encode(v.GetType()))
        let pName = if left.ToUpper() = "ID" then "_id" else left
        let bValue x = BsonValue.Create x
        let bArray (x:IEnumerable) = seq { for z in x -> z }
                                     |> encodeType
                                     |> BsonArray.Create

        match mi.Name.ToLower() with
        | "op_equality"           -> Q.EQ(pName, bValue right)
        | "op_inequality"         -> Q.NE(pName, bValue right) :> QC
        | "op_lessthan"           -> Q.LT(pName, bValue right) :> QC
        | "op_greaterthan"        -> Q.GT(pName, bValue right) :> QC
        | "op_lessthanorequal"    -> Q.GTE(pName, bValue right) :> QC
        | "op_greaterthanorequal" -> Q.GTE(pName, bValue right) :> QC
        | "exists"                -> Q.Exists(pName, right :?> bool) :> QC
        | "matches"               -> Q.Matches(pName, BsonRegularExpression.Create(string right))
        | "in"                    -> Q.In(pName, bArray(right :?> IEnumerable)) :> QC
        | "notin"                 -> Q.NotIn(pName, bArray(right :?> IEnumerable)) :> QC
        | "all"                   -> Q.All(pName, bArray(right :?> IEnumerable)) :> QC
        | "size"                  -> Q.Size(pName, right :?> int) :> QC
        | _ -> failwith "Invalid query"
      | _ -> failwith "Invalid expression"

    build' e

