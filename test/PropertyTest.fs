module PropertyTests

open Xunit
open FsCheck
open FsCheck.Xunit

open scSet

type ArbitraryHashMap<'k, 'v when 'k : comparison and 'v : comparison>() =
    static member HashMap =
        Arb.fromGen <|
            gen {
                let! size = Gen.choose (5, 20) // Размер таблицы от 5 до 20
                let! keyValuePairs = Gen.listOfLength size (Gen.zip (Arb.generate<'k>) (Arb.generate<'v>))
                return List.fold (fun acc (k, v) -> acc.Add(k, v)) (SeparateChainingHashMap(10, hash)) keyValuePairs
            }


[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Add does not change content for existing key-value pair`` (map: SeparateChainingHashMap<int, string>) key value =
    let updatedMap = map.Add(key, value)
    compare map updatedMap || (map.ToSet() = updatedMap.ToSet())

[<Property>]
let ``Remove from empty map does not throw exception`` key =
    let emptyMap = SeparateChainingHashMap<int, string>(10, hash)
    let updatedMap = emptyMap.Remove(key)
    Assert.Equal(emptyMap.ToSet(), updatedMap.ToSet())

[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Merge is associative`` (map1: SeparateChainingHashMap<int, string>) (map2: SeparateChainingHashMap<int, string>) (map3: SeparateChainingHashMap<int, string>) =
    let merged1 = SeparateChainingHashMap.Merge(map1, SeparateChainingHashMap.Merge(map2, map3))
    let merged2 = SeparateChainingHashMap.Merge(SeparateChainingHashMap.Merge(map1, map2), map3)
    compare merged1 merged2

[<Property>]
let ``Compare maps with different data returns false`` (pairs1: (int * string) list) (pairs2: (int * string) list) =
    let map1 = List.fold (fun acc (k, v) -> acc.Add(k, v)) (SeparateChainingHashMap(10, hash)) pairs1
    let map2 = List.fold (fun acc (k, v) -> acc.Add(k, v)) (SeparateChainingHashMap(10, hash)) pairs2
    pairs1 = pairs2 || not (compare map1 map2)
