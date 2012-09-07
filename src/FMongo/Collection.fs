
namespace FMongo

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Builders
open Microsoft.FSharp.Quotations
open FMongo.Util
open FMongo.Mapping

/// <summary>
/// Provides F#-specific BSON serialization and deserialization for a MongoDB collection.
/// </summary>
type 'a Collection (c : BsonDocument MongoCollection) =
  let nullop (f : 'x -> 'y) (x : 'x) : 'y option =
    if isNull x then None
    else Some (f x)
  let idQuery id =
    QueryDocument ("_id", BsonString id)
  let addId id (doc : BsonDocument) =
    doc.["_id"] <- BsonString id
    doc

  member self.FindAll () =
    Cursor<'a>(c, Query.Null)

  (*member self.Find e =
    Cursor<'a>(c, Query.build e)*)

  (*member self.FindOne e =
    self.Find e
    |> Cursor.setLimit 1
    |> List.ofSeq
    |> function
       | [] -> None
       | h::t -> Some h*)

  member self.Insert (v : 'a) =
    v 
    |> encodeDoc typeof<'a> 
    |> c.Insert 
    |> ignore

  member self.Insert (id : string, v : 'a) =
    v 
    |> encodeDoc typeof<'a>
    |> addId id
    |> c.Insert 
    |> ignore

  member self.Save (v : 'a) =
    v 
    |> encodeDoc typeof<'a> 
    |> c.Save 
    |> ignore

  member self.Save (id : string, v : 'a) =
    v 
    |> encodeDoc typeof<'a>
    |> addId id
    |> c.Save 
    |> ignore

  member self.Count () =
    c.Count ()

  member self.FindOneByID (id : string) : 'a option =
    BsonString id
    |> c.FindOneById
    |> nullop (decodeDoc typeof<'a>)

  member self.Remove (id : string) =
    id 
    |> idQuery 
    |> c.Remove 
    |> ignore
