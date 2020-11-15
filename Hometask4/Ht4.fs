namespace HT4

module Ht4 =

    let readArray input =
        let a = System.IO.File.ReadAllLines input
        let resArray = [| for i in a -> int (i.Trim()) |]
        resArray

    let readList input =
        let a = readArray input
        Array.toList a

    let writeArray (output, arToWrite:array<int>) =
        let mutable str = ""
        for i = 0 to arToWrite.Length - 1 do
            str <- str + string(arToWrite.[i]) + "\n"
        System.IO.File.WriteAllText(output, str)

    let writeList (output, listToWrite) =
        let a = List.toArray(listToWrite)
        writeArray(output, a)

    let arBubbleSort (a:array<_>) =
        for i = 0 to a.Length - 1 do
            for j = 0 to a.Length - 2 - i do
                if a.[j] > a.[j+1]
                then
                    let temp = a.[j]
                    a.[j] <- a.[j+1] 
                    a.[j+1] <- temp
        a

    let arQuickSort (a:array<_>) =
        let rec _go (arr:array<_>, l, r) =
            let pivot = arr.[(l + r) / 2]
            let mutable i = l
            let mutable j = r
            while i <= j do
                while arr.[i] < pivot do i <- i + 1
                while arr.[j] > pivot do j <- j - 1
                if i <= j
                then
                    let temp = a.[i]
                    a.[i] <- a.[j]
                    a.[j] <- temp
                    i <- i + 1
                    j <- j - 1
                if j > l then _go (arr, l, j)
                if i < r then _go (arr, i, r)
        if a.Length > 0
        then
            _go (a, 0, a.Length - 1 )
        a

    let listQuickSort l =
        let rec _go lst =
            match lst with
            | [] -> []
            | h::t ->
                let greater = List.filter ((>=) h) t
                let lesser = List.filter ((<) h) t
                (_go greater) @[h] @(_go lesser)
        _go l

    let listBubbleSort l =
        let rec sort lst = 
            match lst with
            | [] -> []
            | x::y::t when x > y -> y::(sort (x::t))
            | x::y -> x::(sort y)
        let rec go lst n =
            match n with
            | 0 -> lst
            | x -> go (sort lst) (x - 1)
        go l l.Length

    let pack32to64 a b =
        if b >= 0
        then
            (int64 a <<< 32) + int64 b
        else
            (int64(a+1) <<< 32) + int64 b 

    let pack16to32 (a:int16) (b:int16) =
        if b >= 0s
        then
            (int32 a <<< 16) + int32 b
        else
            (int32(a + 1s) <<< 16) + int32 b

    let pack64to32 (c:int64) =
        int32(c >>> 32), int32((c <<< 32) >>> 32)

    let pack16to64 (a:int16, b:int16, c:int16, d:int16) =
        pack32to64  (pack16to32 a b)  (pack16to32 c d)
     
    let pack64to16 (a:int64) =
        int16(a >>> 48), int16((a <<< 16) >>> 48), int16((a <<< 32) >>> 48), int16((a <<< 48) >>> 48)
