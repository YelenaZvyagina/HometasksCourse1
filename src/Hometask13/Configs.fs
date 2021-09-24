module Hometask13.Configs

open System
open AlgStcruct
open Quadtree
open CompMatrices

type configs<'t> =
    val sparsity : float
    val size : int
    val astr : AlStruct<'t>
    val parLim : int
    new (sp, sz, astr, lim) = {sparsity = sp; size = sz; astr = astr; parLim = lim}

type mbMatrConf<'t> =
    val m1 : 't
    val m2 : 't
    val fname1 : String
    val fname2 : String

    new (matrix1, matrix2, filename1, filename2) =
        {m1 = matrix1; m2 = matrix2; fname1 = filename1; fname2 = filename2}

let sizeSparsityCheck (matr1 : CompMatrix) (matr2 : CompMatrix) (cfg : configs<_>) =
    let isBig = (matr1.lines >= cfg.size || matr2.columns >= cfg.size)
    let isSparse = (cfg.sparsity * float(matr1.lines*matr1.columns) >= float matr1.lstNonZero.Length || cfg.sparsity * float(matr2.lines*matr2.columns) >= float matr2.lstNonZero.Length)
    (isBig, isSparse)

let confCmToQt (confCm : mbMatrConf<CompMatrix>)  =
    mbMatrConf(cmatrToQt confCm.m1, cmatrToQt confCm.m2, confCm.fname1, confCm.fname2)

let confCmto2d (confCm : mbMatrConf<CompMatrix>) =
    mbMatrConf(cmatrTo2d confCm.m1, cmatrTo2d confCm.m2, confCm.fname1, confCm.fname2)

let mainCfg = configs(0.5, 1000, standSemiring, 1)

let listAllFiles inputPath =
    let files = System.IO.Directory.GetFiles(inputPath)
    List.ofArray files


