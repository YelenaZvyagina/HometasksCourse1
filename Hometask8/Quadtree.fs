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

let div4matr (m: CompMatrix) =
    let l1 = List.filter(fun (i, j, k) -> i+1 <= m.lines/2 && j+1 <= m.columns/2) m.lstNonZero
    let m1 = new CompMatrix(m.lines/2, m.columns/2, l1)
    let l2 = List.filter (fun (i, j, k) -> i+1 <= m.lines/2 && j+1 > m.columns/2) m.lstNonZero
    let l2m = List.map (fun (i, j, k) -> i, j - m.columns/2, k) l2
    let m2 = new CompMatrix(m.lines/2, m.columns/2, l2m)
    let l3 = List.filter (fun (i, j, k) -> i+1 > m.lines/2 && j+1 <= m.columns/2) m.lstNonZero
    let l3m = List.map (fun (i, j, k) -> i - m.lines/2, j, k) l3
    let m3 = new CompMatrix(m.lines/2, m.columns/2, l3m)
    let l4 = List.filter (fun (i, j, k) -> i+1 > m.lines/2 && j+1 > m.columns / 2) m.lstNonZero
    let l4m = List.map (fun (i, j, k) -> i - m.lines/2, j - m.columns/2, k) l4
    let m4 = new CompMatrix(m.lines/2, m.columns/2, l4m)
    (m1, m2, m3, m4)

let cmatrToQtWS (cm:CompMatrix) = 
    let rec inner (m:CompMatrix) =
        if List.isEmpty m.lstNonZero then QuadTree.None
        elif m.lines = 1 && m.columns = 1 && m.lstNonZero.Length <> 0
        then QuadTree.Leaf <| third (m.lstNonZero.Head)
        else
            let (m1, m2, m3, m4) = div4matr m
            let nw = inner m1
            let ne = inner m2
            let sw = inner m3
            let se = inner m4
            reduceNone (nw, ne, sw, se)
    let a = inner (toExCompMatr cm)
    let x = (toExCompMatr cm).lines
    let y = (toExCompMatr cm).columns
    QuadTreeWithSize(a, x, y)

let rec sumInner (qt1:QuadTree<int>) (qt2:QuadTree<int>) (astr:AlStruct<int>) =
    let oper = second (getPars astr)
    let zero = first (getPars astr)
    match qt1, qt2 with
    | None, x -> x
    | x, None -> x
    | Leaf a, Leaf b -> if (oper a b) = zero then None else Leaf (oper a b)
    | Node (a1, a2, a3, a4), Node (b1, b2, b3, b4) ->
        reduceNone ((sumInner a1 b1 astr), (sumInner a2 b2 astr), (sumInner a3 b3 astr), (sumInner a4 b4 astr))
    | _, _ -> failwith "wrong sizes of matrices"

let sumQuadTrWS (qtws1:QuadTreeWithSize<int>) (qtws2:QuadTreeWithSize<int>) (astr:AlStruct<int>) =
    if qtws1.lines = qtws2.lines && qtws1.columns = qtws2.columns then
        let qt1 = qtws1.qtree
        let qt2 = qtws2.qtree
        let res = sumInner qt1 qt2 astr
        QuadTreeWithSize (res, qtws1.lines, qtws1.columns)
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
            | Leaf a, Leaf b ->  if (oper a b) = zero then None else Leaf (oper a b)
            | Node(nw1, ne1, sw1, se1), Node(nw2, ne2, sw2, se2) ->
                let nw = sumInner (inner nw1 nw2) (inner ne1 sw2) astr
                let ne = sumInner (inner nw1 ne2) (inner ne1 se2) astr
                let sw = sumInner (inner sw1 nw2) (inner se1 sw2) astr
                let se = sumInner (inner sw1 ne2) (inner se1 se2) astr
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
        | Leaf a -> Leaf (oper a num)
        | Node (a1, a2, a3, a4) ->
            let nw = inner num a1
            let ne = inner num a2
            let sw = inner num a3
            let se = inner num a4
            Node(nw, ne, sw, se)
    if num = zero then None else inner num qt

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

