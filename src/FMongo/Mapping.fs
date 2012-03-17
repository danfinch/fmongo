
module FMongo.Mapping

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Driver
open FMongo.Util

let private getFields (t : Type) =
  t.GetFields ()
  |> Array.filter (fun f -> not <| (f.IsStatic || f.IsInitOnly))
  |> Array.sortBy (fun f -> f.Name)
let private getProperties (t : Type) =
  t.GetProperties ()
  |> Array.filter (fun p -> p.CanRead && p.CanWrite  && not <| (p.GetGetMethod ()).IsStatic)
  |> Array.sortBy (fun p -> p.Name)
let private fkey (k : obj) =
  let s = string k
  match s.ToUpperInvariant() with
  | "ID" -> "_id"
  | _ -> s

let rec private asArray (t : Type) (v : 'x) =
  let enu =
    v :> obj :?> Collections.IEnumerable
  seq { for x in enu do yield x }
  |> Seq.map (fun v -> encode (if t = typeof<obj> then v.GetType() else t) v) |> BsonArray.Create :> BsonValue

and encode (t : Type) (v : obj) : BsonValue =
  let asBin (v : byte array) = v |> BsonBinaryData.Create :> BsonValue
  let asInt (v : int) = v |> BsonInt32.Create :> BsonValue
  let asLong (v : int64) = v |> BsonInt64.Create :> BsonValue
  let asString (v : 'v) = v :> obj |> string |> BsonString.Create :> BsonValue
  match t with
  | t when t = typeof<bool> -> v |> BsonBoolean.Create :> BsonValue
  | t when t = typeof<byte> -> v :?> byte |> int |> asInt
  | t when t = typeof<sbyte> -> v :?> sbyte |> int |> asInt
  | t when t = typeof<char> -> v :?> char |> int |> asInt
  | t when t = typeof<int16> -> v :?> int16 |> int |> asInt
  | t when t = typeof<uint16> -> v :?> uint16 |> int |> asInt
  | t when t = typeof<int> -> v :?> int |> asInt
  | t when t = typeof<uint32> -> v :?> uint32 |> int64 |> asLong
  | t when t = typeof<int64> -> v :?> int64 |> asLong
  | t when t = typeof<uint64> -> v :?> uint64 |> BitConverter.GetBytes |> asBin
  | t when t = typeof<single> -> v :?> single |> double |> BsonDouble.Create :> BsonValue
  | t when t = typeof<double> -> v :?> double |> BsonDouble.Create :> BsonValue
  | t when t = typeof<decimal> ->
    let v = v :?> decimal
    let bits = Decimal.GetBits v
    let bs = Array.create 16 0uy
    Buffer.BlockCopy (bits, 0, bs, 0, 16)
    bs |> asBin
  | t when t = typeof<DateTime> -> (v :?> DateTime).Ticks |> asLong
  | t when t = typeof<TimeSpan> -> (v :?> TimeSpan).Ticks |> asLong
  | t when t = typeof<Net.IPAddress> -> (v :?> Net.IPAddress).GetAddressBytes () |> asBin
  | t when t = typeof<Guid> -> v |> asString
  | t when t = typeof<Uri> -> v |> asString
  | t when t = typeof<string> -> v :?> string |> BsonString.Create :> BsonValue
  | t when t = typeof<byte array> -> v :?> byte array |> asBin
  | EnumType t -> v |> asString
  | SeqType t -> v |> asArray (t.GetGenericArguments().[0])
  | ArrayType t -> v |> asArray (t.GetElementType())
  | ResizeArrayType t -> v |> asArray (t.GetGenericArguments().[0])
  | DictType t -> v |> asArray (typedefof<Pair<_,_>>.MakeGenericType (t.GetGenericArguments()))
  | ListType t -> v |> asArray (t.GetGenericArguments().[0])
  | TupleType t -> v |> FSharpValue.GetTupleFields |> asArray typeof<obj>
  | OptionType t ->
    match v with
    | null -> BsonNull.Value :> BsonValue
    | _ ->
      let et = t.GetGenericArguments().[0]
      let value = typedefof<option<_>>.MakeGenericType(et).GetProperty("Value").GetValue(v, null)
      encode et value
  | t -> v |> encodeDoc t :> BsonValue
and encodeDoc (t : Type) (v : obj) : BsonDocument =
  match t with
  | PairType t ->
    let doc = BsonDocument ()
    let ets = t.GetGenericArguments ()
    doc.["Key"] <- encode ets.[0] (v?Key)
    doc.["Value"] <- encode ets.[1] v?Value
    doc
  | RecordType t ->
    let doc = BsonDocument ()
    let fields = Seq.zip (FSharpType.GetRecordFields (v.GetType ())) (FSharpValue.GetRecordFields v)
    fields |> Seq.iter (fun (pi, pv) -> doc.[fkey pi.Name] <- encode pi.PropertyType pv)
    doc
  | UnionType t ->
    let t = v.GetType ()
    let case, vs = FSharpValue.GetUnionFields (v, t)
    let doc = BsonDocument ()
    doc.["case"] <- BsonString.Create case.Name
    doc.["fields"] <- vs |> asArray typeof<obj>
    doc
  | t ->
    let doc = BsonDocument ()
    for f in getFields t do
      let fv = f.GetValue v
      doc.[fkey f.Name] <- encode f.FieldType fv
    for p in getProperties t do
      let pv = p.GetValue (v, null)
      doc.[p.Name] <- encode p.PropertyType pv
    doc
let rec decode (t : Type) (v : BsonValue) : 'x =
  match t with
  | t when t = typeof<bool> -> v.AsBoolean :> obj
  | t when t = typeof<byte> -> v.AsInt32 |> byte :> obj
  | t when t = typeof<sbyte> -> v.AsInt32 |> sbyte :> obj
  | t when t = typeof<char> -> v.AsInt32 |> char :> obj
  | t when t = typeof<int16> -> v.AsInt32 |> int16 :> obj
  | t when t = typeof<uint16> -> v.AsInt32 |> uint16 :> obj
  | t when t = typeof<int> -> v.AsInt32 :> obj
  | t when t = typeof<uint32> -> v.AsInt64 |> uint32 :> obj
  | t when t = typeof<int64> -> v.AsInt64 :> obj
  | t when t = typeof<uint64> -> BitConverter.ToUInt64 (v.AsByteArray, 0) :> obj
  | t when t = typeof<single> -> v.AsDouble |> single :> obj
  | t when t = typeof<double> -> v.AsDouble :> obj
  | t when t = typeof<decimal> ->
    [0 .. 3] |> List.map (fun n -> BitConverter.ToInt32 (v.AsByteArray, n * 4))
    |> Array.ofList |> Decimal.fromBits :> obj
  | t when t = typeof<Net.IPAddress> -> Net.IPAddress v.AsByteArray :> obj
  | t when t = typeof<DateTime> -> v.AsInt64 |> DateTime.fromTicks :> obj
  | t when t = typeof<TimeSpan> -> v.AsInt64 |> TimeSpan.fromTicks :> obj
  | t when t = typeof<Guid> -> v.AsString |> Guid.fromString :> obj
  | t when t = typeof<Uri> -> v.AsString |> Uri.fromString :> obj
  | t when t = typeof<string> -> v.AsString :> obj
  | t when t = typeof<byte array> -> v.AsByteArray :> obj
  | EnumType t -> Enum.Parse (t, v.AsString)
  | ArrayType t ->
    let v = v.AsBsonArray
    let et = t.GetElementType()
    let array = Array.CreateInstance (et, v.Count)
    for i = 0 to array.Length - 1 do
      array.SetValue (decode et v.[i], i)
    array :> obj
  | SeqType t | ResizeArrayType t ->
    let et = t.GetGenericArguments().[0]
    let lt = typedefof<ResizeArray<_>>.MakeGenericType et
    let list = (Activator.CreateInstance lt) :?> Collections.IList
    v.AsBsonArray
    |> Seq.iter (fun e -> list.Add (decode et e) |> ignore)
    list
    :> obj
  | DictType t ->
    let ets = t.GetGenericArguments ()
    let dt = typedefof<Dict<_,_>>.MakeGenericType ets
    let dic = Activator.CreateInstance dt
    let meth = dt.GetMethod ("Add", ets)
    let add (p : BsonValue) =
      let doc = p.AsBsonDocument
      meth.Invoke (dic, [| doc.["Key"] |> decode ets.[0]; doc.["Value"] |> decode ets.[1] |])
      |> ignore
    v.AsBsonArray |> Seq.iter add
    dic
  | PairType t ->
    let ets = t.GetGenericArguments ()
    let doc = v.AsBsonDocument
    let ps = [| doc.["Key"] |> decode ets.[0], doc.["Value"] |> decode ets.[1] |]
    let pair = Activator.CreateInstance (t, ps)
    pair
  | ListType t ->
    let lt = t.GetGenericArguments().[0]
    let v = v.AsBsonArray
    let array = Array.CreateInstance (lt, v.Count)
    for i = 0 to array.Length - 1 do
      array.SetValue (decode lt v.[i], i)
    let mt = typedefof<list<_>>.Assembly.GetType "Microsoft.FSharp.Collections.ListModule"
    ((mt.GetMethod "OfArray").MakeGenericMethod ([| lt |])).Invoke (null, [| array |])
  | TupleType t ->
    let ets = FSharpType.GetTupleElements t
    let es =
      v.AsBsonArray
      |> Seq.mapi (fun i x -> (decode ets.[i] x) :> obj)
      |> Array.ofSeq
    FSharpValue.MakeTuple (es, t)
  | OptionType t ->
    if v.IsBsonNull then
      null
    else
      let value = decode (t.GetGenericArguments().[0]) v
      let case = FSharpType.GetUnionCases(t) |> Array.find (fun c -> c.Name = "Some")
      FSharpValue.MakeUnion (case, [| value |])
  | _ -> decodeDoc t v.AsBsonDocument :> obj
  :?> 'x
and decodeDoc (t : Type) (v : BsonDocument) : 'x =
  let pickVal (f : Reflection.PropertyInfo) =
    let bv = ref null
    if v.TryGetValue ((fkey f.Name), bv) then
      !bv |> decode f.PropertyType :> obj
    else
      match f.PropertyType with
      | OptionType t ->
        None :> obj
      | ListType t ->
        let lt = typedefof<_ list>.MakeGenericType [| t.GetGenericArguments().[0] |]
        lt.GetProperty("Empty").GetValue(null, null)
      | _ -> failwith "Unsupported type for new fields."
  match t with
  | RecordType t ->
    let fs = FSharpType.GetRecordFields t
    let vs =
      fs
      |> Seq.map pickVal
      |> Array.ofSeq
    FSharpValue.MakeRecord (t, vs)
  | UnionType t ->
    let case = v.["case"].AsString
    let ci =
      FSharpType.GetUnionCases t
      |> Seq.find (fun c -> c.Name = case)
    let fs = ci.GetFields ()
    let args =
      v.["fields"].AsBsonArray
      |> Seq.mapi (fun i fv -> decode fs.[i].PropertyType fv :> obj)
      |> Array.ofSeq
    FSharpValue.MakeUnion (ci, args)
  | t ->
        let inst = Activator.CreateInstance t
        getFields t
        |> Seq.iter (fun f -> f.SetValue (inst, decode f.FieldType v.[fkey f.Name]))
        getProperties t
        |> Seq.iter (fun f -> f.SetValue (inst, decode f.PropertyType v.[fkey f.Name], null))
        inst
  :?> 'x

