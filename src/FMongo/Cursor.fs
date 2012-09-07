
namespace FMongo

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Builders
open Microsoft.FSharp.Quotations
open FMongo.Util
open FMongo.Mapping

type 'a Cursor (cur : BsonDocument MongoCursor) =
  let mutable result : 'a ResizeArray option = None
  new (col : BsonDocument MongoCollection, q : IMongoQuery) =
    Cursor (MongoCursor<BsonDocument>(col, q, ReadPreference.Nearest))
  member self.Clone<'b> () = Cursor cur
  member self.Count () = cur.Count ()
  member self.Explain (?verbose : bool) = cur.Explain (match verbose with Some b -> b | _ -> false)
  member self.SetBatchSize x = Cursor (cur.SetBatchSize x)
  member self.SetFields (x : string array) = Cursor (cur.SetFields x)
  member self.SetFields (x : IMongoFields) = Cursor (cur.SetFields x)
  member self.SetFlags x = Cursor (cur.SetFlags x)
  member self.SetHint (x : string) = Cursor (cur.SetHint x)
  member self.SetHint (x : BsonDocument) = Cursor (cur.SetHint x)
  member self.SetLimit x = Cursor (cur.SetLimit x)
  member self.SetMax x = Cursor (cur.SetMax x)
  member self.SetMaxScan x = Cursor (cur.SetMaxScan x)
  member self.SetMin x = Cursor (cur.SetMin x)
  member self.SetOption (name, value) = Cursor (cur.SetOption (name, value))
  member self.SetOptions x = Cursor (cur.SetOptions x)
  member self.SetShowDiskLoc () = Cursor (cur.SetShowDiskLoc ())
  member self.SetSkip x = Cursor (cur.SetSkip x)
  member self.SetReadPreference b = Cursor (cur.SetReadPreference b)
  member self.SetSnapshot () = Cursor (cur.SetSnapshot ())
  member self.SetSortOrder (x : string array) = Cursor (cur.SetSortOrder x)
  member self.SetSortOrder (x : IMongoSortBy) = Cursor (cur.SetSortOrder x)
  member self.Size () = cur.Size ()
  interface 'a seq with
    member self.GetEnumerator () = 
      if result.IsNone then do
        result <- Some (ResizeArray ())
        for x in cur :> Collections.IEnumerable do
          result.Value.Add (x :?> BsonValue |> decode typeof<'a> :> obj :?> 'a)
      result.Value.GetEnumerator () :> Collections.Generic.IEnumerator<'a>
    member self.GetEnumerator () =
      (self :> 'a seq).GetEnumerator () :> Collections.IEnumerator

module Cursor =
  let clone<'b> (m : _ Cursor) = m.Clone<'b>()
  let count (m : _ Cursor) = m.Count()
  let explain (m : _ Cursor) = m.Explain()
  let setBatchSize x (m : _ Cursor) = m.SetBatchSize x
  let setFields (x : IMongoFields) (m : _ Cursor) = m.SetFields x
  let setFieldsByArray (s : string array) (m : _ Cursor) = m.SetFields s
  let setFlags x (m : _ Cursor) = m.SetFlags x
  let setHintByDoc (doc : BsonDocument) (m : _ Cursor) = m.SetHint doc
  let setHintByString (s : string) (m : _ Cursor) = m.SetHint s
  let setLimit x (m : _ Cursor) = m.SetLimit x
  let setMax doc (m : _ Cursor) = m.SetMax doc
  let setMaxScan i (m : _ Cursor) = m.SetMaxScan i
  let setMin doc (m : _ Cursor) = m.SetMin doc
  let setOption value (m : _ Cursor) = m.SetOption value
  let setOptions doc (m : _ Cursor) = m.SetOptions doc
  let setShowDiskLoc (m : _ Cursor) = m.SetShowDiskLoc()
  let setSkip x (m : _ Cursor) = m.SetSkip x
  let setReadPreference b (m : _ Cursor) = m.SetReadPreference b
  let setSnapshot (m : _ Cursor) = m.SetSnapshot()
  let setSortOrder (x : IMongoSortBy) (m : _ Cursor) = m.SetSortOrder x
  let setSortOrderByArray (s : string array) (m : _ Cursor) = m.SetSortOrder s
  let size (m : _ Cursor) = m.Size()
  let tryEval (m : _ Cursor) = if Seq.isEmpty m then None else Some m