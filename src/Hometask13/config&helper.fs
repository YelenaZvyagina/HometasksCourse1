module Hometask13.helpingFunctions

open System
open Quadtree

[<Struct>]
type mbMatrConf<'t> =
    val m1 : 't
    val m2 : 't
    val fname1 : String
    val fname2 : String

    new (matrix1, matrix2, filename1, filename2) =
        {m1 = matrix1; m2 = matrix2; fname1 = filename1; fname2 = filename2}

    static member sizeSparsityCheck (matr1 : CompMatrix) (matr2 : CompMatrix) =
        let isBig = (matr1.lines >= 1000 || matr2.columns >= 1000)
        let isSparse = (matr1.lines*matr1.columns >=  2*matr1.lstNonZero.Length || matr2.lines*matr2.columns >= 2*matr2.lstNonZero.Length)
        (isBig, isSparse)

    static member confCmToQt (confCm : mbMatrConf<CompMatrix>) =
        mbMatrConf(cmatrToQt confCm.m1, cmatrToQt confCm.m2, confCm.fname1, confCm.fname2)

    static member confCmto2d (confCm : mbMatrConf<CompMatrix>) =
         mbMatrConf(cmatrTo2d confCm.m1, cmatrTo2d confCm.m2, confCm.fname1, confCm.fname2)

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

