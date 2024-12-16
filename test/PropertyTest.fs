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
                return List.fold (fun (acc: SeparateChainingHashMap<'k, 'v>) (k, v) -> add k v acc) (create 10 hash) keyValuePairs
            }
            

[<Property>]
let ``Remove from empty map does not throw exception`` key =
    let emptyMap : SeparateChainingHashMap<int, int> = create 10 hash
    let updatedMap = remove key emptyMap
    Assert.True(compare emptyMap updatedMap)


[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Merge is associative`` (map1: SeparateChainingHashMap<int, string>) 
                              (map2: SeparateChainingHashMap<int, string>) 
                              (map3: SeparateChainingHashMap<int, string>) =
    let merged1 = merge (merge map1 map2) map3
    let merged2 = merge map1 (merge map2 map3)
    Assert.True(compare merged1 merged2)


[<Property(Arbitrary = [| typeof<ArbitraryHashMap<int, string>> |])>]
let ``Compare returns false for different data`` (map1: SeparateChainingHashMap<int, string>) =
    let map2 = 
        map1.Buckets
        |> Array.collect Set.toArray
        |> Array.map (fun (k, v) -> (k, v + "_diff"))
        |> Array.fold (fun (acc: SeparateChainingHashMap<int, string>) (k, v) -> add k v acc) (create 10 hash)
    Assert.False(compare map1 map2)
