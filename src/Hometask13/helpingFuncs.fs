module Hometask13.helpingFunctions

open System
open Quadtree

let parMatrixMult (m1: int[,]) (m2: int[,]) =
    if (m1.GetLength 1 = m2.GetLength 0)
    then
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

let listAllFiles inputPath =
    let files = System.IO.Directory.GetFiles(inputPath)
    List.ofArray files

