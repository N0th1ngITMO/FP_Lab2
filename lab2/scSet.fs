module scSet

open System
open System.Collections.Generic

type SeparateChainingHashMap<'k, 'v when 'k : comparison and 'v : comparison> = {
    Buckets: Set<'k * 'v> array
    HashFunction: 'k -> int
}

let create (bucketCount: int) (hashFunction: 'k -> int) : SeparateChainingHashMap<'k, 'v> =
    let emptyBuckets = Array.init bucketCount (fun _ -> Set.empty<'k * 'v>)
    { Buckets = emptyBuckets; HashFunction = hashFunction }

let getBucketIndex (map: SeparateChainingHashMap<'k, 'v>) (key: 'k) =
    abs (map.HashFunction key) % map.Buckets.Length

let count (map: SeparateChainingHashMap<'k, 'v>) =
    map.Buckets |> Array.sumBy (fun bucket -> bucket.Count)

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

let remove (key: 'k) (map: SeparateChainingHashMap<'k, 'v>): SeparateChainingHashMap<'k, 'v> =
    let index = getBucketIndex map key
    let updatedBucket = map.Buckets.[index] |> Set.filter (fun (k, _) -> k <> key)
    let newBuckets = Array.copy map.Buckets
    Array.set newBuckets index updatedBucket
    { map with Buckets = newBuckets }

let map (f: 'k -> 'v -> 'u) (map: SeparateChainingHashMap<'k, 'v>): SeparateChainingHashMap<'k, 'u> =
    let mappedBuckets =
        map.Buckets |> Array.map (Set.map (fun (k, v) -> (k, f k v)))
    { Buckets = mappedBuckets; HashFunction = map.HashFunction}

let filter (predicate: 'k -> 'v -> bool) (map: SeparateChainingHashMap<'k, 'v>) : SeparateChainingHashMap<'k, 'v> =
    let filteredBuckets =
        map.Buckets |> Array.map (Set.filter (fun (k, v) -> predicate k v))
    { map with Buckets = filteredBuckets }

let foldLeft (map: SeparateChainingHashMap<'k, 'v>) (folder: 'state -> 'k -> 'v -> 'state) (state: 'state) : 'state =
    map.Buckets |> Array.fold (fun acc bucket -> Set.fold (fun acc' (k, v) -> folder acc' k v) acc bucket) state

let foldRight (map: SeparateChainingHashMap<'k, 'v>) (folder: 'k -> 'v -> 'state -> 'state) (state: 'state) : 'state =
    map.Buckets |> Array.foldBack (fun bucket acc -> Set.foldBack (fun (k, v) acc' -> folder k v acc') bucket acc) state

let toSet (map: SeparateChainingHashMap<'k, 'v>) : Set<'k * 'v> =
    map.Buckets |> Array.fold (fun acc bucket -> Set.union acc bucket) Set.empty

let compare (map1: SeparateChainingHashMap<'k, 'v>) (map2: SeparateChainingHashMap<'k, 'v>) : bool =
    if count map1 <> count map2 then
        false
    else
        let entries1 = 
            map1.Buckets 
            |> Array.collect Set.toArray
            |> Set.ofArray
        let entries2 = 
            map2.Buckets 
            |> Array.collect Set.toArray
            |> Set.ofArray
        entries1 = entries2


let merge (map1: SeparateChainingHashMap<'k, 'v>) (map2: SeparateChainingHashMap<'k, 'v>) : SeparateChainingHashMap<'k, 'v> =
    map1.Buckets
    |> Array.fold (fun acc bucket ->
        bucket
        |> Set.fold (fun accMap (k, v) -> add k v accMap) acc
    ) map2
