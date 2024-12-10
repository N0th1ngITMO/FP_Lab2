module PropertyTests

open Xunit
open FsCheck
open FsCheck.Xunit

open scSet

type ArbitraryHashMap<'k, 'v when 'k : comparison and 'v : comparison>() =
    static member HashMap =
        Arb.fromGen <|
            gen {
                let! size = Gen.choose (5, 20)
                let! keyValuePairs = Gen.listOfLength size (Gen.zip (Arb.generate<'k>) (Arb.generate<'v>))
                return List.fold (fun (acc: SeparateChainingHashMap<'k, 'v>) (k, v) -> acc.Add k v ) (SeparateChainingHashMap(10, hash)) keyValuePairs
            }


[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Add does not change content for existing key-value pair`` (map: SeparateChainingHashMap<int, string>) key value =
    let updatedMap = map.Add key value
    let keyExistsWithSameValue =
        map.ToSet() |> Set.exists (fun (k, v) -> k = key && v = value)
    let isTestPassing =
        if keyExistsWithSameValue then
            SeparateChainingHashMap.Compare map updatedMap
        else
            updatedMap.ToSet() = (map.ToSet() |> Set.add (key, value))
    Assert.True(isTestPassing)



[<Property>]
let ``Remove from empty map does not throw exception`` key =
    let emptyMap = SeparateChainingHashMap<int, string>(10, hash)
    let updatedMap = emptyMap.Remove(key)
    Assert.True(SeparateChainingHashMap.Compare emptyMap updatedMap)

[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Merge is associative`` (map1: SeparateChainingHashMap<int, string>) 
                              (map2: SeparateChainingHashMap<int, string>) 
                              (map3: SeparateChainingHashMap<int, string>) =

    let merged1: SeparateChainingHashMap<int, string> = SeparateChainingHashMap.Merge map1 map2
    let merged2: SeparateChainingHashMap<int, string> = SeparateChainingHashMap.Merge merged1 map3

    let merged3: SeparateChainingHashMap<int, string> = SeparateChainingHashMap.Merge map2 map3
    let merged4: SeparateChainingHashMap<int, string> = SeparateChainingHashMap.Merge map1 merged3

    Assert.True(SeparateChainingHashMap.Compare merged2 merged4)


[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Compare returns false for different data`` (map1: SeparateChainingHashMap<int, string>) =
    let map2 = 
        map1.Buckets
        |> Array.collect Set.toArray
        |> Array.map (fun (k, v) -> (k, v + "_diff"))
        |> Array.fold (fun (acc: SeparateChainingHashMap<int, string>) (k, v) -> acc.Add k v) (SeparateChainingHashMap<int, string>(10, hash))
    Assert.False(SeparateChainingHashMap.Compare map1 map2)
