# FP_Lab2  
# Лабораторная работа 2  
## Комягин Дмитрий, P3332  
## Вариант - sc-set  

# Реализация  
Добавление + увеличение размера структуры  
```
let rehash (map: SeparateChainingHashMap<'k, 'v>) (newBucketCount: int) : SeparateChainingHashMap<'k, 'v> =
    let newBuckets = Array.init newBucketCount (fun _ -> Set.empty<'k * 'v>)
    let newHashFunction = fun key -> abs (map.HashFunction key) % newBucketCount
    let redistributed =
        map.Buckets
        |> Array.collect Set.toArray
        |> Array.fold (fun (acc : Set<'k * 'v> array) (key, value) ->
            let index = abs (newHashFunction key)
            let updatedBucket = Set.add (key, value) acc.[index]
            Array.set acc index updatedBucket
            acc
        ) newBuckets
    { Buckets = redistributed; HashFunction = newHashFunction }

let add (key: 'k) (value: 'v) (map: SeparateChainingHashMap<'k, 'v>) : SeparateChainingHashMap<'k, 'v> =
    let updatedMap =
        if float (count map) / float map.Buckets.Length >= 0.7 then
            rehash map (map.Buckets.Length * 2)
        else
            map
    let index = getBucketIndex updatedMap key
    let currentBucket = updatedMap.Buckets.[index]
    let updatedBucket =
        currentBucket
        |> Set.filter (fun (k, _) -> k <> key)
        |> Set.add (key, value)
    let newBuckets = Array.copy updatedMap.Buckets
    Array.set newBuckets index updatedBucket
    { updatedMap with Buckets = newBuckets }
```  
Удаление элемента  
```
let remove (key: 'k) (map: SeparateChainingHashMap<'k, 'v>): SeparateChainingHashMap<'k, 'v> =
    let index = getBucketIndex map key
    let updatedBucket = map.Buckets.[index] |> Set.filter (fun (k, _) -> k <> key)
    let newBuckets = Array.copy map.Buckets
    Array.set newBuckets index updatedBucket
    { map with Buckets = newBuckets }
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
let filter (predicate: 'k -> 'v -> bool) (map: SeparateChainingHashMap<'k, 'v>) : SeparateChainingHashMap<'k, 'v> =
    let filteredBuckets =
        map.Buckets |> Array.map (Set.filter (fun (k, v) -> predicate k v))
    { map with Buckets = filteredBuckets }
```
Свертки  
```
let foldLeft (map: SeparateChainingHashMap<'k, 'v>) (folder: 'state -> 'k -> 'v -> 'state) (state: 'state) : 'state =
    map.Buckets |> Array.fold (fun acc bucket -> Set.fold (fun acc' (k, v) -> folder acc' k v) acc bucket) state

let foldRight (map: SeparateChainingHashMap<'k, 'v>) (folder: 'k -> 'v -> 'state -> 'state) (state: 'state) : 'state =
    map.Buckets |> Array.foldBack (fun bucket acc -> Set.foldBack (fun (k, v) acc' -> folder k v acc') bucket acc) state
```
Compare
```
let compare (map1: SeparateChainingHashMap<'k, 'v>) (map2: SeparateChainingHashMap<'k, 'v>) : bool =
    if count map1 <> count map2 then
        false
    else
        map1.Buckets
        |> Array.forall (fun bucket1 ->
            bucket1 |> Set.forall (fun (k, v) ->
                let bucketIndex = getBucketIndex map2 k
                let bucket2 = map2.Buckets.[bucketIndex]
                bucket2 |> Set.exists (fun (k2, v2) -> k = k2 && v = v2)))
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
