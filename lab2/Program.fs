open System
open scSet

[<EntryPoint>]
let main (argv: string array) =

    let hashFunction key = abs key % 10

    let emptyMap = SeparateChainingHashMap<int, string>(10, hashFunction)

    let map = 
        emptyMap
        |> fun m -> m.Add 1 "one"
        |> fun m -> m.Add 2 "two"
        |> fun m -> m.Add 11 "eleven"

    let mappedMap = map.Map (fun k v -> v.ToUpper())

    let filteredMap = mappedMap.Filter (fun k v -> not (v.StartsWith "T"))

    let foldedResult = filteredMap.FoldLeft (fun acc k v -> acc + k) 0

    let allElements = filteredMap.ToSet()

    printfn "Все элементы как множество: %A" allElements
    printfn "Результат свертки: %d" foldedResult
    0
