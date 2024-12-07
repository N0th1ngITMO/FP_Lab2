# FP_Lab2  
# Лабораторная работа 2  
## Комягин Дмитрий, P3332  
## Вариант - sc-set  

# Реализация  
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
        SeparateChainingHashMap(newBuckets, updatedMap.HashFunction)```
