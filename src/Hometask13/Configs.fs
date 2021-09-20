module Hometask13.Configs

open System
open Hometask13
open Quadtree

type Configs =
    val sparsity : float
    val size : int
    new (sp, sz) = {sparsity = sp; size = sz}

[<Struct>]
type mbMatrConf<'t> =
    val m1 : 't
    val m2 : 't
    val fname1 : String
    val fname2 : String
    val conf : Configs

    new (matrix1, matrix2, filename1, filename2, confs) =
        {m1 = matrix1; m2 = matrix2; fname1 = filename1; fname2 = filename2; conf = confs}

    static member sizeSparsityCheck (matr1 : CompMatrix) (matr2 : CompMatrix) (cfg : Configs) =
        let isBig = (matr1.lines >= cfg.size || matr2.columns >= cfg.size)
        let isSparse = (cfg.sparsity * float(matr1.lines*matr1.columns) >= float matr1.lstNonZero.Length || cfg.sparsity * float(matr2.lines*matr2.columns) >= float matr2.lstNonZero.Length)
        (isBig, isSparse)

    static member confCmToQt (confCm : mbMatrConf<CompMatrix>) (glConf : Configs) =
        mbMatrConf(cmatrToQt confCm.m1, cmatrToQt confCm.m2, confCm.fname1, confCm.fname2, glConf)

    static member confCmto2d (confCm : mbMatrConf<CompMatrix>) (glConf : Configs) =
         mbMatrConf(cmatrTo2d confCm.m1, cmatrTo2d confCm.m2, confCm.fname1, confCm.fname2, glConf)

let glConfigs = Configs(0.5, 1000)

