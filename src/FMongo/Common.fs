
module FMongo.Util

open System
open Microsoft.FSharp.Reflection

let inline isNull (a : obj) = Object.ReferenceEquals (a, null)
let inline (?) (x : obj) (y : string) =
  x.GetType().GetProperty(y).GetValue(x, null)
type IResizeArray<'a> = System.Collections.Generic.IList<'a>
type IDict<'k, 'v> = System.Collections.Generic.IDictionary<'k, 'v>
type Dict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type Pair<'k, 'v> = System.Collections.Generic.KeyValuePair<'k, 'v>

let inline (|EnumType|_|) (t : Type) = if t.IsEnum then Some t else None
let inline (|TupleType|_|) (t : Type) = if FSharpType.IsTuple t then Some t else None
let inline (|ArrayType|_|) (t : Type) = if t.IsArray then Some t else None
let inline (|UnionType|_|) (t : Type) = if FSharpType.IsUnion t then Some t else None
let inline (|RecordType|_|) (t : Type) = if FSharpType.IsRecord t then Some t else None
let inline (|ListType|_|) (t : Type) =
  if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<List<_>> then
    Some t
  else
    None
let inline (|OptionType|_|) (t : Type) =
  if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<Option<_>> then
    Some t
  else
    None
let inline (|MapType|_|) (t : Type) =
  if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<Map<_,_>> then
    Some t
  else
    None
let inline (|DictType|_|) (t : Type) =
  if t.IsGenericType then
    let gt = t.GetGenericTypeDefinition ()
    if gt = typedefof<System.Collections.Generic.IDictionary<_,_>> || gt = typedefof<System.Collections.Generic.Dictionary<_,_>> then
      Some t
    else
      None
  else
    None
let inline (|PairType|_|) (t : Type) =
  if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<Pair<_, _>> then
    Some t
  else
    None
let inline (|ResizeArrayType|_|) (t : Type) =
  if t.IsGenericType then
    let gt = t.GetGenericTypeDefinition ()
    if gt = typedefof<Collections.Generic.IList<_>> || gt = typedefof<Collections.Generic.List<_>> then
      Some t
    else
      None
  else
    None
let inline (|SeqType|_|) (t : Type) =
  if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<seq<_>> then
    Some t
  else
    None

module Decimal =
  let inline fromBits (bits : int array) = Decimal bits

module DateTime =
  let inline fromTicks (ticks : int64) = DateTime ticks

module TimeSpan =
  let inline fromTicks (ticks : int64) = TimeSpan ticks

module Guid =
  let inline fromString (s : string) = Guid s

module Uri =
  let inline fromString (s : string) = Uri s

module ResizeArray =
  let inline ofSeq (x : _ seq) = ResizeArray x
