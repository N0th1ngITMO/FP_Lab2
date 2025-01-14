# FP_Lab2  
# Лабораторная работа 2  
## Комягин Дмитрий, P3332  
## Вариант - sc-set  

# Реализация  
Добавление + увеличение размера структуры  
```
member private this.Rehash (newBucketCount: int) =
        let newBuckets = Array.init newBucketCount (fun _ -> Set.empty)
        let newHashFunction = fun key -> abs (hashFunction key) % newBucketCount
        for bucket in buckets do
            for (key, value) in bucket do
                let index = abs (newHashFunction key)
                newBuckets.[index] <- Set.add (key, value) newBuckets.[index]
        SeparateChainingHashMap(newBuckets, newHashFunction)

    member this.Add (key: 'k) (value: 'v) =
        let updatedMap =
            if float (this.Count()) / float this.BucketCount >= 0.7 then
                this.Rehash(this.BucketCount * 2)
            else
                this
        let index = updatedMap.GetBucketIndex key
        let updatedBucket = 
            updatedMap.Buckets.[index]
            |> Set.filter (fun (k, _) -> k <> key)
            |> Set.add (key, value)
        let newBuckets = Array.copy updatedMap.Buckets
        newBuckets.[index] <- updatedBucket
        SeparateChainingHashMap(newBuckets, updatedMap.HashFunction)
```  
Удаление элемента  
```
    member this.Remove (key: 'k) =
        let index = this.GetBucketIndex key
        let updatedBucket =
            buckets.[index]
            |> Set.filter (fun (k, _) -> k <> key)
        let newBuckets = Array.copy buckets
        newBuckets.[index] <- updatedBucket
        SeparateChainingHashMap(newBuckets, hashFunction)
```
Map  
```
let map (f: 'k -> 'v -> 'u) (map: SeparateChainingHashMap<'k, 'v>): SeparateChainingHashMap<'k, 'u> =
    let mappedBuckets =
        map.Buckets |> Array.map (Set.map (fun (k, v) -> (k, f k v)))
    { Buckets = mappedBuckets; HashFunction = map.HashFunction}
```
Filter  
```
    member this.Filter (predicate: 'k -> 'v -> bool) =
        let filteredBuckets =
            buckets
            |> Array.map (Set.filter (fun (k, v) -> predicate k v))
        SeparateChainingHashMap(filteredBuckets, hashFunction)
```
Свертки  
```
    member this.FoldLeft (folder: 'state -> 'k -> 'v -> 'state) (state: 'state) =
        buckets
        |> Array.fold (fun acc bucket -> 
            bucket |> Set.fold (fun acc' (k, v) -> folder acc' k v) acc
        ) state

    member this.FoldRight (folder: 'k -> 'v -> 'state -> 'state) (state: 'state) =
        Array.foldBack (fun bucket acc -> 
            Set.foldBack (fun (k, v) acc' -> folder k v acc') bucket acc
        ) buckets state
```
Merge  
```
let merge (map1: SeparateChainingHashMap<'k, 'v>) (map2: SeparateChainingHashMap<'k, 'v>) : SeparateChainingHashMap<'k, 'v> =
    map1.Buckets
    |> Array.fold (fun acc bucket ->
        bucket
        |> Set.fold (fun accMap (k, v) -> add k v accMap) acc
    ) map2
```
