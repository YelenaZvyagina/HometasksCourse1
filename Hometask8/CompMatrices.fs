module CompMatrices

open System

type CompMatrix =
    val lines: int
    val columns: int
    val lstNonZero: list<int*int*int>
    new (x, y, list) = {lines = x; columns = y; lstNonZero = list}

let isPow2 n =
    if n > 1
    then double (int (Math.Log(double n, 2.0))) = Math.Log(double n, 2.0)
    else false

let toDeg2 m =
    int (2.0**(Math.Log(double m, 2.0) + 1.0))

let toCompMatr input =
    let strAr = System.IO.File.ReadAllLines input
    let matLenForComp = (strAr.[0].Split()).Length
    let l = [
        for i = 0 to strAr.Length - 1 do
            let arVals = strAr.[i].Split()
            if arVals.Length <> matLenForComp then failwith "All of matrix's strings should contain the same amount of numbers"
            for j = 0 to arVals.Length - 1 do
                if arVals.[j] <> "0" then (i, j, int arVals.[j] - int "0")
            ]
    CompMatrix(strAr.Length, matLenForComp, l)

let toExCompMatr (cm:CompMatrix) =
    if cm.columns = cm.lines && isPow2 cm.lines then cm
    else
        let m = Math.Max (cm.lines, cm.columns)
        if isPow2 m
        then CompMatrix(m, m, cm.lstNonZero)
        else CompMatrix(toDeg2 m, toDeg2 m, cm.lstNonZero)

let cmatrTo2d (cm : CompMatrix) =
    let matr2d = Array2D.zeroCreate cm.lines cm.columns
    List.iter (fun (x, y, elem) -> matr2d.[x, y] <- elem ) cm.lstNonZero
    matr2d
