module Tests

open Xunit
open System
open scSet

[<Fact>]
let ``Test Add method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, string>(5, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, "One") |> Add(2, "Two")
    
    Assert.Equal(2, updatedMap.Count())
    Assert.Contains((1, "One"), updatedMap.ToSet())
    Assert.Contains((2, "Two"), updatedMap.ToSet())

[<Fact>]
let ``Test Remove method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, string>(5, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, "One") |> Add(2, "Two") |> Remove(1)
    Assert.Equal(1, updatedMap.Count())
    Assert.DoesNotContain((1, "One"), updatedMap.ToSet())
    Assert.Contains((2, "Two"), updatedMap.ToSet())

[<Fact>]
let ``Test Map method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, int>(5, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, 10) |> Add(2, 20) |> Map(fun _ v -> v * 2)
    Assert.Equal(2, updatedMap.Count())
    Assert.Contains((1, 20), updatedMap.ToSet())
    Assert.Contains((2, 40), updatedMap.ToSet())

[<Fact>]
let ``Test Filter method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, int>(5, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, 10) |> Add(2, 20) |> Filter(fun _ v -> v > 10)
    Assert.Equal(1, updatedMap.Count())
    Assert.DoesNotContain((1, 10), updatedMap.ToSet())
    Assert.Contains((2, 20), updatedMap.ToSet())

[<Fact>]
let ``Test FoldLeft method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, int>(5, hashFunction)
    let updatedMap = hashMap.Add(1, 10).Add(2, 20).Add(3, 30)
    let updatedMap = 
        hashMap |> Add(1, 10) |> Add(2, 20) |> Add(3, 30)
    let sum = updatedMap.FoldLeft(fun acc _ v -> acc + v) 0
    Assert.Equal(60, sum)

[<Fact>]
let ``Test FoldRight method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, int>(5, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, 10) |> Add(2, 20) |> Add(3, 30)
    let sum = updatedMap.FoldRight(fun _ v acc -> acc + v) 0
    Assert.Equal(60, sum)

[<Fact>]
let ``Test Rehash method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, string>(2, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, "One") |> Add(2, "Two") |> Add(3, "Three")
    Assert.True(updatedMap.BucketCount > 2)
    Assert.Equal(3, updatedMap.Count())
    Assert.Contains((1, "One"), updatedMap.ToSet())
    Assert.Contains((2, "Two"), updatedMap.ToSet())
    Assert.Contains((3, "Three"), updatedMap.ToSet())

[<Fact>]
let ``Test Merge method`` () =
    let hashFunction key = key.GetHashCode()
    let map1 = SeparateChainingHashMap<int, string>(5, hashFunction).Add(1, "One")
    let map2 = SeparateChainingHashMap<int, string>(5, hashFunction).Add(2, "Two")
    let mergedMap = SeparateChainingHashMap.Merge(map1, map2)
    Assert.Equal(2, mergedMap.Count())
    Assert.Contains((1, "One"), mergedMap.ToSet())
    Assert.Contains((2, "Two"), mergedMap.ToSet())

[<Fact>]
let ``Test ToSet method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = SeparateChainingHashMap<int, string>(5, hashFunction)
    let updatedMap = 
        hashMap |> Add(1, "One") |> Add(2, "Two")
    let resultSet = updatedMap.ToSet()
    Assert.Contains((1, "One"), resultSet)
    Assert.Contains((2, "Two"), resultSet)
    Assert.Equal(2, resultSet.Count)
