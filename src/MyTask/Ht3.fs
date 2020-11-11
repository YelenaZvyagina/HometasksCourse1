namespace MyTask
module Ht3 =

    open System

    let e n =
        let unit = Array2D.zeroCreate n n
        for i = 0 to n - 1 do
            unit.[i, i] <- 1
        unit
        
    
    let matrixMult (m1 : int [,]) (m2 : int [,]) =
        if (m1.GetLength 1 = m2.GetLength 0) then 
            let a = m1.GetLength 0
            let b = m1.GetLength 1
            let c = m2.GetLength 1
            let res = Array2D.zeroCreate a c 
            for i = 0 to a - 1 do
                for j = 0 to c - 1 do
                    for l = 0 to b - 1 do
                        res.[i, j] <- res.[i, j] + m1.[i, l] * m2.[l, j]
            res
        else failwith "It's impossible to multiply matrixes of this sizes"

    let rec matrixPow (m : int [,]) n =
        if m.GetLength 0 = m.GetLength 1 then 
            if n = 0 then e (m.GetLength 0)
            else matrixMult m ( matrixPow m (n - 1) )
        else failwith "It's impossible to multiply matrixes of this sizes"
    
    let rec fibRec n =
       if n = 0 || n = 1 then n
       elif n < 0 then failwith "It's unreal to find Fibonacci number if N below zero"
       else fibRec(n - 1) + fibRec(n - 2)
      

    let fibIter n =
        if n = 0 || n = 1 then n
        elif n < 0 then failwith "It's unreal to find Fibonacci number if N below zero"
        else 
            let mutable fib0 = 0
            let mutable fib1 = 1
            for i = 0 to n-2 do
                fib1 <- fib0 + fib1
                fib0 <- fib1 - fib0
            fib1

    let fibTail n =
        let rec _go n acc1  acc2 =
            if n = 0 then acc1
            elif n < 0 then failwith "It's unreal to find Fibonacci number if N below zero"
            else _go (n - 1) acc2 (acc1 + acc2)
        _go n 0 1

    let fibMatrixNaive n =
        let m = array2D [| [|0; 1|]; [|1; 1|] |]
        if n >= 0 then 
            let result = matrixPow m n
            result.[0, 1]
        else failwith "It's unreal to find Fibonacci number if N below zero"

    let fibMatrixLog n =
        let m = array2D [| [|0; 1|]; [|1; 1|] |]
        let rec go m n =
            if n = 0 then e 2
            elif n > 0 then
                if n % 2 = 1 then matrixMult ( go m (n - 1) ) m
                else
                    let b = go m (n / 2)
                    matrixMult b b 
            else failwith "It's unreal to find Fibonacci number if N below zero"
        let result = go m n
        result.[0, 1]

    let fibCalc n =
        let a = Array.zeroCreate (n + 1)
        if n > 0 then
            a.[0] <- 0
            a.[1] <- 1
            for i = 2 to n do
                a.[i] <- a.[i-1] + a.[i-2]
        elif n < 0 then failwith "It's unreal to find Fibonacci number if N below zero"
        a
