module Ht12

open System
open Quadtree
open System.Threading.Tasks
open AlgStcruct

let printStringAr (m:string [,]) path =
    for i = 0 to m.GetLength 0 - 1 do
        let mutable str = ""
        for j = 0 to m.GetLength 1 - 1 do
            str <- str + m.[i, j] + " "
        str <- str + "\n"
        System.IO.File.AppendAllText(path, str)

let genRandMatr<'t> rows cols n (zeroPercent: float) (path: string) =  
    for i = 0 to n-1 do
        let output = Array2D.zeroCreate rows cols
        for j = 0 to rows-1 do
            for k = 0 to cols-1 do
                let a = (new Random()).NextDouble() 
                if typeof<'t> = typeof<int32> then
                    if a > zeroPercent   
                    then output.[j, k] <- string ((new System.Random()).Next(1, 50))
                    else output.[j, k] <- "0"
                elif typeof<'t> = typeof<float> then
                    if a > zeroPercent   
                    then output.[j, k] <- string ((new System.Random()).NextDouble() + double ((new System.Random()).Next(1, 50))) 
                    else output.[j, k] <- "0"
                elif typeof<'t> = typeof<bool> then
                    if a > zeroPercent  
                    then output.[j, k] <- "1" 
                    else output.[j, k] <- "0"
        printStringAr output (path.[0..path.Length - 5] + string i + path.[path.Length - 4..path.Length - 1])

let consoleGenRandMatr =
    Console.WriteLine("Enter the sizes of matrices")
    let rows = int (Console.ReadLine())
    let cols = int (Console.ReadLine())
    Console.WriteLine("Enter the type of martrices (int float or bool) ")
    let typ = Console.ReadLine()
    Console.WriteLine("Enter the path to a folder to store")
    let path = Console.ReadLine()
    Console.WriteLine("Enter the percent of zeros im the matrix (from 0 to 1)")
    let zeroPercent = float (Console.ReadLine())
    Console.WriteLine("Enter the number of matrices")
    let n = int (Console.ReadLine())
    match typ with
    | "int" -> genRandMatr<int> rows cols n zeroPercent path
    | "float" -> genRandMatr<float> rows cols n zeroPercent path
    | "bool" -> genRandMatr<bool> rows cols n zeroPercent path
    | _ -> failwith "this type is not supported, sorry"

let parMultQuadTree (qt1:QuadTree<int>) (qt2:QuadTree<int>) (astr: AlStruct<int>) lim =
    let zero = first (getPars astr)
    let oper = third (getPars astr)
    let rec inner qt1 qt2 count =
        match qt1, qt2 with
        | None, x | x, None -> None
        | Leaf a, Leaf b -> if (oper a b) = zero then None else Leaf (oper a b)
        | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
            if count < lim
            then
                let s1 = async.Return (sumInner (inner nw1 nw2 (count + 1)) (inner ne1 sw2 (count + 1)) astr)
                let s2 = async.Return (sumInner (inner nw1 ne2 (count + 1)) (inner ne1 se2 (count + 1)) astr)
                let s3 = async.Return (sumInner (inner sw1 nw2 (count + 1)) (inner se1 sw2 (count + 1)) astr)
                let s4 = async.Return (sumInner (inner sw1 ne2 (count + 1)) (inner se1 se2 (count + 1)) astr)
                let s = [s1; s2; s3; s4] |> Async.Parallel |> Async.RunSynchronously
                reduceNone (s.[0], s.[1], s.[2], s.[3])
            else 
                let nw = sumInner (multInner nw1 nw2 astr) (multInner ne1 sw2 astr) astr
                let ne = sumInner (multInner nw1 ne2 astr) (multInner ne1 se2 astr) astr
                let sw = sumInner (multInner sw1 nw2 astr) (multInner se1 sw2 astr) astr
                let se = sumInner (multInner sw1 ne2 astr) (multInner se1 se2 astr) astr
                reduceNone (nw, ne, sw, se)
        | _, _ -> failwith "bla"
    inner qt1 qt2 0





let parMatrixMultLoop1 (m1: int[,]) (m2: int[,]) = 
    if (m1.GetLength 1 = m2.GetLength 0) then 
        let a = m1.GetLength 0
        let b = m1.GetLength 1
        let c = m2.GetLength 1
        let res = Array2D.zeroCreate a c
        [ for i in 0 .. a - 1 ->
            async {
                    do
                        for j in 0 .. c - 1 do
                            for k in 0 .. b - 1 do
                                res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
            }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        res
    else failwith "It's impossible to multiply matrixes of this sizes"

let parMatrixMultLoop2 (m1: int[,]) (m2: int[,]) = 
    if (m1.GetLength 1 = m2.GetLength 0) then 
        let a = m1.GetLength 0
        let b = m1.GetLength 1
        let c = m2.GetLength 1
        let res = Array2D.zeroCreate a c
        [ for i in 0 .. a - 1 do
            for j in 0 .. c - 1 ->
                async {
                    do
                        for k in 0 .. b - 1 do
                        res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
                }   
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        res
    else failwith "It's impossible to multiply matrixes of this sizes"

let parMatrixMultLoop3 (m1: int[,]) (m2: int[,]) = 
    if (m1.GetLength 1 = m2.GetLength 0) then 
        let a = m1.GetLength 0
        let b = m1.GetLength 1
        let c = m2.GetLength 1
        let res = Array2D.zeroCreate a c
        for i = 0 to a - 1 do 
            for j = 0 to c - 1 do
                 Parallel.For(0, b, (fun l -> 
                    res.[i, j] <- res.[i, j] + m1.[i, l] * m2.[l, j]))
                 |> ignore

        res
    else failwith "It's impossible to multiply matrixes of this sizes"
