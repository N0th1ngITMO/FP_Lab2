module scSet

open System
open System.Collections.Generic

// Тип данных для неизменяемой хеш-таблицы
type SeparateChainingHashMap<'k, 'v when 'k : comparison and 'v:comparison>(buckets: Set<'k * 'v> array, hashFunction: 'k -> int) =
    
    // Конструктор для создания пустой хеш-таблицы
    new(bucketCount: int, hashFunction: 'k -> int) =
        let emptyBuckets = Array.init bucketCount (fun _ -> Set.empty)
        SeparateChainingHashMap(emptyBuckets, hashFunction)
    
    // Свойства
    member this.Buckets = buckets
    member this.HashFunction = hashFunction
    member this.BucketCount = buckets.Length

    // Приватная функция для вычисления индекса
    member private this.GetBucketIndex (key: 'k) =
        abs (hashFunction key) % this.BucketCount

    member this.Count() =
        buckets
        |> Array.sumBy (fun bucket -> bucket.Count)

    member private this.Rehash (newBucketCount: int) =
        let newBuckets = Array.init newBucketCount (fun _ -> Set.empty)
        let newHashFunction = fun key -> abs (hashFunction key) % newBucketCount
        for bucket in buckets do
            for (key, value) in bucket do
                let index = abs (newHashFunction key)
                newBuckets.[index] <- Set.add (key, value) newBuckets.[index]
        SeparateChainingHashMap(newBuckets, newHashFunction)

    // Добавление элемента
    member this.Add (key: 'k) (value: 'v) =
        // Проверка на переполнение и ре-хеширование
        let updatedMap =
            if float (this.Count()) / float this.BucketCount >= 0.7 then
                this.Rehash(this.BucketCount * 2)
            else
                this
    
        // Найти индекс бакета для добавления
        let index = updatedMap.GetBucketIndex key
        let updatedBucket = 
            updatedMap.Buckets.[index]
            |> Set.filter (fun (k, _) -> k <> key) // Удалить старое значение с тем же ключом
            |> Set.add (key, value) // Добавить новое значение
    
        // Обновить бакеты
        let newBuckets = Array.copy updatedMap.Buckets
        newBuckets.[index] <- updatedBucket
        SeparateChainingHashMap(newBuckets, updatedMap.HashFunction)



    // Удаление элемента
    member this.Remove (key: 'k) =
        let index = this.GetBucketIndex key
        let updatedBucket =
            buckets.[index]
            |> Set.filter (fun (k, _) -> k <> key) // Удалить все элементы с этим ключом
        let newBuckets = Array.copy buckets
        newBuckets.[index] <- updatedBucket
        SeparateChainingHashMap(newBuckets, hashFunction)

    // Отображение (map)
    member this.Map (f: 'k -> 'v -> 'v) =
        let mappedBuckets =
            buckets
            |> Array.map (Set.map (fun (k, v) -> (k, f k v)))
        SeparateChainingHashMap(mappedBuckets, hashFunction)

    // Фильтрация
    member this.Filter (predicate: 'k -> 'v -> bool) =
        let filteredBuckets =
            buckets
            |> Array.map (Set.filter (fun (k, v) -> predicate k v))
        SeparateChainingHashMap(filteredBuckets, hashFunction)

    // Свертка (левая)
    member this.FoldLeft (folder: 'state -> 'k -> 'v -> 'state) (state: 'state) =
        buckets
        |> Array.fold (fun acc bucket -> 
            bucket |> Set.fold (fun acc' (k, v) -> folder acc' k v) acc
        ) state

   // Свертка (правая)
    member this.FoldRight (folder: 'k -> 'v -> 'state -> 'state) (state: 'state) =
        Array.foldBack (fun bucket acc -> 
            Set.foldBack (fun (k, v) acc' -> folder k v acc') bucket acc
        ) buckets state


    // Реализация интерфейса множества для перечисления всех элементов
    member this.ToSet() =
        buckets |> Array.fold (fun acc bucket -> Set.union acc bucket) Set.empty

    // Моноидная операция (объединение хеш-таблиц)
    static member Merge (map1: SeparateChainingHashMap<'k, 'v>) (map2: SeparateChainingHashMap<'k, 'v>) =
        if map1.BucketCount <> map2.BucketCount then
            failwith "Both hash maps must have the same number of buckets to merge"
        else
            let mergedBuckets =
                Array.map2 (fun b1 b2 -> Set.union b1 b2) map1.Buckets map2.Buckets
            SeparateChainingHashMap(mergedBuckets, map1.HashFunction)