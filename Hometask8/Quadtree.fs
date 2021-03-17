module Quadtree

open System
open AlgStcruct

type QuadTree<'t> =
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

type CompMatrix =
    val lines: int
    val columns: int
    val lstNonZero: list<int*int*int>
    new (x, y, list) = {lines = x; columns = y; lstNonZero = list}

type QuadTreeWithSize<'t> =
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

let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

let reduceNone (nw, ne, sw, se) =
    if sw = None && se = None && ne = None && nw = None then None
    else Node(nw, ne, sw, se)

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
        if List.isEmpty m.lstNonZero then QuadTree.None
        elif m.lines = 1 && m.columns = 1 && m.lstNonZero.Length <> 0
        then QuadTree.Leaf <| third (m.lstNonZero.Head)
        else
            let (m1, m2, m3, m4) = div4matr m
            QuadTree.Node(inner m1, inner m2, inner m3, inner m4)
    let a = inner (toExCompMatr cm)
    QuadTreeWithSize(a, x, y)

let sumQuadTrWS (qtws1:QuadTreeWithSize<int>) (qtws2:QuadTreeWithSize<int>) (astr:AlStruct<int>) =
    let oper = third (getPars astr)
    let zero = first (getPars astr)
    if qtws1.lines = qtws2.lines && qtws1.columns = qtws2.columns then
        let qt1 = qtws1.qtree
        let qt2 = qtws2.qtree
        let rec sum qt1 qt2 =
            match qt1, qt2 with
            | None, x -> x
            | x, None -> x
            | Leaf a, Leaf b -> if (oper a b) = zero then None else Leaf (oper a b)
            | Node (a1, a2, a3, a4), Node (b1, b2, b3, b4) ->
                reduceNone (sum a1 b1, sum a2 b2, sum a3 b3, sum a4 b4)
            | _, _ -> failwith "wrong sizes of matrices"
        QuadTreeWithSize ((sum qt1 qt2), qtws1.lines, qtws1.columns)
    else failwith "Only marices of the same sizes can be summed"

let multQuadTrWS (qtws1:QuadTreeWithSize<int>) (qtws2:QuadTreeWithSize<int>) (astr:AlStruct<int>) =
    let zero = first (getPars astr)
    let oper = third (getPars astr)
    if qtws1.lines = qtws2.columns then
        let qt1 = qtws1.qtree
        let qt2 = qtws2.qtree
        let rec inner (qt1:QuadTree<int>) (qt2:QuadTree<int>) =
            match qt1, qt2 with
            | None, x -> None
            | x, None -> None
            | Leaf a, Leaf b -> if oper a b = zero then None else Leaf (oper a b)
            | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
                let nw = (sumQuadTrWS (QuadTreeWithSize((inner nw1 nw2), 1, 1)) (QuadTreeWithSize((inner ne1 sw2), 1, 1)) astr).qtree
                //передаю единички в качестве параметров длины и ширины поскольку функция суммы принимает тип QuadTreeWithSize
                let ne = (sumQuadTrWS (QuadTreeWithSize((inner nw1 ne2), 1, 1)) (QuadTreeWithSize((inner ne1 sw2), 1, 1)) astr).qtree
                let sw = (sumQuadTrWS (QuadTreeWithSize((inner sw1 nw2), 1, 1)) (QuadTreeWithSize((inner se1 sw2), 1, 1)) astr).qtree
                let se = (sumQuadTrWS (QuadTreeWithSize((inner sw1 ne2), 1, 1)) (QuadTreeWithSize((inner se1 se2), 1, 1)) astr).qtree
                reduceNone (nw, ne, sw, se)
            | _, _ -> failwith "Wrong sizes of matrices"
        QuadTreeWithSize((inner qt1 qt2), qtws1.lines, qtws2.columns)
    else failwith "Wrong sizes of matrices"

let multQtToNum (num:int) (qt:QuadTree<int>) (astr:AlStruct<int>) =
    let zero = first (getPars astr)
    let oper = third (getPars astr)
    let rec inner (num:int) (qt:QuadTree<int>) =
        match qt with
        | None -> None
        | Leaf a -> if oper a num = zero then None else Leaf (oper a num)
        | Node (a1, a2, a3, a4) ->
            let nw = inner num a1
            let ne = inner num a2
            let sw = inner num a3
            let se = inner num a4
            reduceNone (nw, ne, sw, se)
    inner num qt

let tenzMultQuadTr (qtws1:QuadTreeWithSize<int>) (qtws2:QuadTreeWithSize<int>) (astr:AlStruct<int>) =
    let qt1 = qtws1.qtree
    let qt2 = qtws2.qtree
    let rec inner (qt1:QuadTree<int>) (qt2:QuadTree<int>) =
        match qt1, qt2 with
        | None, x -> None
        | x, None -> None
        | Leaf b, Node(a1, a2, a3, a4) -> Node ((multQtToNum b a1 astr), (multQtToNum b a2 astr), (multQtToNum b a3 astr), (multQtToNum b a4 astr))
        | Node (a1, a2, a3, a4), Leaf b -> Node ((multQtToNum b a1 astr), (multQtToNum b a2 astr), (multQtToNum b a3 astr), (multQtToNum b a4 astr))
        | Node (a1, a2, a3, a4), Node (b1, b2, b3, b4) ->
            let nw = inner a1 (Node (b1, b2, b3, b4))
            let ne = inner a2 (Node (b1, b2, b3, b4))
            let sw = inner a3 (Node (b1, b2, b3, b4))
            let se = inner a4 (Node (b1, b2, b3, b4))
            reduceNone(nw, ne, sw, se)
        | _, _ -> failwith "Something went wrong"
    QuadTreeWithSize ((inner qt1 qt2), (qtws1.lines * qtws2.lines), (qtws1.columns * qtws2.columns))
