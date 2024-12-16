module Tests

open Xunit
open System
open scSet

[<Fact>]
let ``Test Add method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 5 hashFunction
    let updatedMap = hashMap |> add 1 "One" |> add 2 "Two"
    Assert.Equal(2, count updatedMap)
    Assert.Contains((1, "One"), toSet updatedMap)
    Assert.Contains((2, "Two"), toSet updatedMap)

[<Fact>]
let ``Test Remove method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 5 hashFunction
    let updatedMap = hashMap |> add 1 "One" |> add 2 "Two" |> remove 1
    Assert.Equal(1, count updatedMap)
    Assert.DoesNotContain((1, "One"), toSet updatedMap)
    Assert.Contains((2, "Two"), toSet updatedMap)

[<Fact>]
let ``Test Map method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 5 hashFunction
    let updatedMap = hashMap |> add 1 10 |> add 2 20 |> map (fun _ v -> v * 2)
    Assert.Equal(2, count updatedMap)
    Assert.Contains((1, 20), toSet updatedMap)
    Assert.Contains((2, 40), toSet updatedMap)

[<Fact>]
let ``Test Filter method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 5 hashFunction
    let updatedMap = hashMap |> add 1 10 |> add 2 20 |> filter (fun _ v -> v > 10)
    Assert.Equal(1, count updatedMap)
    Assert.DoesNotContain((1, 10), toSet updatedMap)
    Assert.Contains((2, 20), toSet updatedMap)

[<Fact>]
let ``Test FoldLeft method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 5 hashFunction
    let updatedMap = hashMap |> add 1 10 |> add 2 20 |> add 3 30
    let sum = foldLeft updatedMap (fun acc _ v -> acc + v) 0
    Assert.Equal(60, sum)

[<Fact>]
let ``Test FoldRight method`` () =
    let hashFunc (key: int) = key.GetHashCode()
    let hashMap = create 5 hashFunc
    let updatedMap = hashMap |> add 1 10 |> add 2 20 |> add 3 30
    let sum = foldLeft updatedMap (fun acc _ v -> acc + v) 0
    Assert.Equal(60, sum)


[<Fact>]
let ``Test Rehash method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 2 hashFunction
    let updatedMap = hashMap |> add 1 "One" |> add 2 "Two" |> add 3 "Three"
    Assert.True(count updatedMap > 2)
    Assert.Equal(3, count updatedMap)
    Assert.Contains((1, "One"), toSet updatedMap)
    Assert.Contains((2, "Two"), toSet updatedMap)
    Assert.Contains((3, "Three"), toSet updatedMap)

[<Fact>]
let ``Test Merge method`` () =
    let hashFunction key = key.GetHashCode()
    let map1 = create 5 hashFunction |> add 1 "One"
    let map2 = create 5 hashFunction |> add 2 "Two"
    let mergedMap = merge map1 map2
    Assert.Equal(2, count mergedMap)
    Assert.Contains((1, "One"), toSet mergedMap)
    Assert.Contains((2, "Two"), toSet mergedMap)

[<Fact>]
let ``Test ToSet method`` () =
    let hashFunction key = key.GetHashCode()
    let hashMap = create 5 hashFunction
    let updatedMap = hashMap |> add 1 "One" |> add 2 "Two"
    let resultSet = toSet updatedMap
    Assert.Contains((1, "One"), resultSet)
    Assert.Contains((2, "Two"), resultSet)
    Assert.Equal(2, resultSet.Count)
