module Ht6

open System
open System.IO

[<Measure>] type col
[<Measure>] type str

[<Struct>]
type Cell =
    val S: int<str>
    val C: int<col>
    new (s, c) = {S = s; C = c}

[<Struct>]
type BoolMatrix =
    val lines: int<str>
    val columns: int<col>
    val listOfCells: list<Cell>
    new (x, y, list) = {lines = x; columns = y; listOfCells = list}

let readMatrix input =
    let strAr = System.IO.File.ReadAllLines input 
    let l = [
            for i = 0 to strAr.Length - 1 do
                if strAr.[i].Length <> strAr.[0].Length then failwith "Incorrect matrix"
                for j = 0 to strAr.[0].Length - 1 do
                    if strAr.[i].[j] = '1'
                    then
                        Cell(i*1<str>, j*1<col>)
        ]
    BoolMatrix(strAr.Length*1<str>, strAr.[0].Length*1<col>, l)

let multBoolMatrix (m1:BoolMatrix, m2:BoolMatrix) =
    if int m1.columns <> int m2.lines 
        then
        failwith "Matrixes of these sizes cannot be multiplied"
        else
            let resList = [
                for i in m1.listOfCells do
                    for j in m2.listOfCells do
                        if int i.S = int j.C then Cell(i.S, j.C)
                        ]
            BoolMatrix (m1.lines, m2.columns, List.distinct resList)

let boolMatrixToMatrix (m:BoolMatrix) =
    let output = Array2D.zeroCreate (int m.lines) (int m.columns)
    for i in m.listOfCells do
        output.[int i.S, int i.C] <- true
    output

let boolToStringMat (m:Boolean [,]) =
    let res = Array.zeroCreate (m.GetLength 0)
    for i = 0 to (m.GetLength 0) - 1 do
        for j = 0 to (m.GetLength 1) - 1 do
            if m.[i, j] = true
            then res.[i] <- res.[i] + "1"
            else res.[i] <- res.[i] + "0"
    res

let writeMatrix output (m:Boolean [,])  =
    System.IO.File.WriteAllLines (output, boolToStringMat m)

let boolMatOper input1 input2 output =
    let resMult = multBoolMatrix (readMatrix input1, readMatrix input2)
    writeMatrix output (boolMatrixToMatrix resMult)
