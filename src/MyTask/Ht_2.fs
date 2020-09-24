namespace MyTask

module Ht_2 =
    open System

    let randomArray n = Array.init n (fun _ -> System.Random().Next())

    let task1 x = x*x*x*x + x*x*x + x*x + x + 1

    let task2 x =
        let a = x*x
        let b = a+1
        b*(a+x) + 1
        
    let task3 (ar:array<int>, x )=
        let mutable k = 0
        for i in 0..ar.Length-1 do
            if ar.[i] <= x then k <- k + 1
        let indexes = Array.zeroCreate k
        k <- 0
        for i = 0 to ar.Length-1 do
            if ar.[i] <= x then
                indexes.[k] <- i
                k <- k + 1
        indexes
            
    let task4 (ar:array<int>, a, b )=
        let mutable k = 0
        for i in 0..ar.Length-1 do
             if ar.[i] < a || ar.[i] > b then k <- k + 1
        let indexes = Array.zeroCreate k
        k <- 0
        for i = 0 to ar.Length-1 do
            if ar.[i] < a || ar.[i] > b then
                indexes.[k] <- i
                k <- k + 1
        indexes
        
    let task5 (ar:array<int>) =
        ar.[0] <- ar.[1] + ar.[0]
        ar.[1] <- ar.[0] - ar.[1]
        ar.[0] <- ar.[0] - ar.[1]
        ar

    let task6 (ar:array<int>, i, j) =
        ar.[i] <- ar.[j] + ar.[i]
        ar.[j] <- ar.[i] - ar.[j]
        ar.[i] <- ar.[i] - ar.[j]
        ar
