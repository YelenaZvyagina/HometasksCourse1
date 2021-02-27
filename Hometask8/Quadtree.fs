module Quadtree

open System

type QuadTree<'t> =
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

type CompMatrix =
    val lines: int
    val columns: int
    val lstNonZero: list<int*int*int>
    new (x, y, list) = {lines = x; columns = y; lstNonZero = list}

type QTwithSize<'t> =
    val qtree: QuadTree<'t>
    val lines: int
    val columns: int
    new (qt, x, y) = {qtree = qt; lines = x; columns = y}

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

let div4matr (m:CompMatrix) =
    let l1 = List.filter(fun (i, j, k) -> i < (m.lines)/2 && j < (m.columns)/2) (m.lstNonZero)
    let m1 = new CompMatrix((m.lines)/2, (m.columns)/2, l1)
    let l2 = List.filter(fun (i, j, k) -> i > (m.lines)/2 && j < (m.columns)/2) (m.lstNonZero)
    let m2 = new CompMatrix((m.lines)/2, (m.columns)/2, l2)
    let l3 = List.filter(fun (i, j, k) -> i < (m.lines)/2 && j > (m.columns)/2) (m.lstNonZero)
    let m3 = new CompMatrix((m.lines)/2, (m.columns)/2, l3)
    let l4 = List.filter(fun (i, j, k) -> i > (m.lines)/2 && j > (m.columns)/2) (m.lstNonZero)
    let m4 = new CompMatrix((m.lines)/2, (m.columns)/2, l4)
    (m1, m2, m3, m4)

let cmatrToQtWS (cm:CompMatrix) =
    let x = cm.lines
    let y = cm.columns
    let rec inner (m:CompMatrix) =
        let third (_, _, c) = c
        if List.isEmpty m.lstNonZero then QuadTree.None
        elif m.lines = 1 && m.columns = 1 && m.lstNonZero.Length <> 0
        then QuadTree.Leaf <| third (m.lstNonZero.Head)
        else
            let (m1, m2, m3, m4) = div4matr m
            QuadTree.Node (inner m1, inner m2, inner m3, inner m4)
    let a = inner (toExCompMatr cm)
    QTwithSize (a, x, y)

let rec sumQuadTrWS (qtws1:QTwithSize<int>) (qtws2:QTwithSize<int>) =
    if qtws1.lines = qtws2.lines && qtws1.columns = qtws2.columns then
        let qt1 = qtws1.qtree
        let qt2 = qtws2.qtree
        let rec sum qt1 qt2 =
            match qt1, qt2 with
            | None, x -> x
            | x, None -> x
            | Leaf a, Leaf b -> Leaf (a+b)
            | Node (a1, a2, a3, a4), Node (b1, b2, b3, b4) ->
                if sum a1 b1 = None && sum a2 b2  = None && sum a3 b3  = None && sum a4 b4  = None
                then None
                else Node (sum a1 b1, sum a2 b2, sum a3 b3, sum a4 b4)
            | _, _ -> failwith "wrong sizes of matrices"
        QTwithSize((sum qt1 qt2), qtws1.lines, qtws1.columns)
    else failwith "Only marices of the same sizes can be summed"

let lengthOfQt (qtr:QuadTree<int>) =
    let rec inner (qt:QuadTree<int>) x =
        match qt with
        | None -> x
        | Leaf a -> x
        | Node (a1, a2, a3, a4) -> Math.Max(Math.Max(inner a1 (x+1), inner a2 (x+1)),  Math.Max(inner a3 (x+1), inner a4 (x+1)))
    inner qtr 0

let multQuadTrWS (qtws1:QTwithSize<int>) (qtws2:QTwithSize<int>) =
    if qtws1.lines = qtws2.columns then
        let qt1 = qtws1.qtree
        let qt2 = qtws2.qtree
        let rec inner (qt1:QuadTree<int>) (qt2:QuadTree<int>) =
            match qt1, qt2 with
            | None, x -> None
            | x, None -> None
            | Leaf a, Leaf b -> Leaf (a*b)
            | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
                let nw = (sumQuadTrWS (QTwithSize((inner nw1 nw2), 1, 1)) (QTwithSize((inner ne1 sw2), 1, 1))).qtree
                //передаю единички в качестве параметров длины и ширины поскольку функция суммы принимает тип QTWithSize
                let ne = (sumQuadTrWS (QTwithSize((inner nw1 ne2), 1, 1)) (QTwithSize((inner ne1 sw2), 1, 1))).qtree
                let sw = (sumQuadTrWS (QTwithSize((inner sw1 nw2), 1, 1)) (QTwithSize((inner se1 sw2), 1, 1))).qtree
                let se = (sumQuadTrWS (QTwithSize((inner sw1 ne2), 1, 1)) (QTwithSize((inner se1 se2), 1, 1))).qtree
                Node(nw, ne, sw, se)
            | _, _ -> failwith "Wrong sizes of matrices"
        QTwithSize((inner qt1 qt2), qtws1.lines, qtws2.columns)
    else failwith "Wrong sizes of matrices"
