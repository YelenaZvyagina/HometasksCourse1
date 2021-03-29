namespace Ht8Tests

module Ht8Tests = 

    open System
    open Expecto
    open Quadtree
    open AlgStcruct
    open MyTask.Ht3   

    let monoidT = Monoid(new Monoid<int>(0, (+)))
    let semiringT = Semiring(new Semiring<int>(new Monoid<int>(0, (+)), (*)))
    let qtws1 = new QuadTreeWithSize<int>( (Node (Leaf (3), None, Leaf (4), None)), 2, 2) 
    let qtws2 = new QuadTreeWithSize<int>( (Node (None, None, Leaf (34), None)), 2, 2)
    let qtws3 = new QuadTreeWithSize<int>(Node(Leaf (2), None, None, Leaf (3)), 2, 2)
    let qtws4 = new QuadTreeWithSize<int>(Node(None, Leaf (6), Leaf (1), None), 2, 2)
    let qtws5 = new QuadTreeWithSize<int>(Node((Node( Leaf (1), None, None, Leaf(5) ) ), None, (Node( None, Leaf (7), None, Leaf(2) )), None), 4, 4)
    let qtws6 = new QuadTreeWithSize<int>(Node( None, (Node( Leaf (8), None, None, Leaf(6) ) ), (Node( None, Leaf (3), None, Leaf(14) )), None), 4, 4)

    let arToQTWS (ar:int [,]) =
        let l = ar.GetLength 0
        let c = ar.GetLength 1
        let lst = [
            for i = 0 to l-1 do
                for j = 0 to c-1 do
                    if ar.[i, j] <> 0 then (i, j, ar.[i, j])
                ]
        let res = Quadtree.CompMatrix(l, c, lst)
        Quadtree.cmatrToQtWS res

    let genRandomIntArray  =
        let ar = Array2D.zeroCreate 2 2
        for i = 0 to 1 do
            for j = 0 to 1 do
                ar.[i, j] <- (new System.Random()).Next(0, 15)
        ar
 
    [<Tests>]
    let quadTrTenzMultTests =
        testList "Tests for QuadTree tensor multipliction function"
            [
             testCase "Tensor multiplicating matrices 2x2 and 2x2" <| fun _ -> 
              let result = (Quadtree.tenzMultQuadTr qtws4 qtws3 semiringT).qtree
              let exp = Node(None, (Node(Leaf(12), None, None, Leaf(18))), (Node(Leaf(2), None, None, Leaf(3))), None)
              Expect.equal result exp "We should get the same quadtrees as a result of tensor multiplication"

             testCase "Tensor multiplicating matrices 2x2 and 2x2 with a lot of nones" <| fun _ ->
              let result = (Quadtree.tenzMultQuadTr qtws1 qtws2 semiringT).qtree
              let exp = Node((Node(None, None, Leaf(102), None)), None, (Node(None, None, Leaf(136), None)), None)
              Expect.equal result exp "We should get the same quadtrees as a result of tensor multiplication"
            ]

    [<Tests>]
    let quadTrSumTests =
        testList "Tests for QuadTree summ function"
            [
             testCase "Summing matrices 2x2 and 2x2" <| fun _ ->
              let result = (Quadtree.sumQuadTrWS qtws4 qtws3 monoidT).qtree
              let exp = Node(Leaf(2), Leaf(6), Leaf(1), Leaf(3))
              Expect.equal result exp "We should get the same quadtrees as a result"

             testCase "Summing matrices 4x4 and 4x4" <| fun _ ->
              let result = (Quadtree.sumQuadTrWS qtws5 qtws6 monoidT).qtree
              let exp = Node((Node(Leaf (1), None, None, Leaf (5)) ), (Node(Leaf(8), None, None, Leaf(6)) ), (Node(None, Leaf (10), None, Leaf(16))), None)
              Expect.equal exp result "We should get the same quadtrees as a result"

             testCase "Summing matrices 2x2 and 2x2 with a lot of nones" <| fun _ ->
             let result = (Quadtree.sumQuadTrWS qtws1 qtws2 monoidT).qtree
             let exp = Node(Leaf (3), None, Leaf (38), None)
             Expect.equal exp result "We should get the same quadtrees as a result"
            ]

    [<Tests>]
    let quadTrMultTests =
        testList "Tests for Quadtree multiplication function"
            [
             testCase "Multiplying matrices 2x2 and 2x2" <| fun _ ->
              let result = (Quadtree.multQuadTrWS qtws3 qtws4 semiringT).qtree
              let exp = Node(None, Leaf (12), Leaf(3), None)
              Expect.equal exp result "We should get the same quadtrees as a result"

             testCase "Multiplying matrices 2x2 and 2x2 with a none result" <| fun _ ->
             let result = (Quadtree.multQuadTrWS qtws1 qtws2 semiringT).qtree
             let exp = None
             Expect.equal exp result "We should get none as a result"
            ]

    let arraySumm (a1: int [,]) (a2: int [,]) =
        if (a1.GetLength 0) <> (a2.GetLength 0) || (a1.GetLength 1) <> (a2.GetLength 1)
        then failwith "wrong sizes"
        else
            let arRes = Array2D.zeroCreate (a1.GetLength 0) (a1.GetLength 1)
            for i = 0 to (a1.GetLength 0) - 1 do
                for j = 0 to (a1.GetLength 1) - 1 do
                    arRes.[i, j] <- a1.[i, j] + a2.[i, j]
            arRes

    let arTensMult (a1: int [,]) (a2: int [,]) =
        let r1 = a1.GetLength 0
        let c1 = a1.GetLength 1
        let r2 = a2.GetLength 0
        let c2 = a2.GetLength 1
        let res = Array2D.zeroCreate (r1*r2) (c1*c2)
        for i = 0 to r1-1 do
            for k = 0 to c1-1 do
                for j = 0 to r2-1 do
                    for l = 0 to c2-1 do
                        res.[r2*i + j, c2*k + l] <- a1.[i, k] * a2.[j, l]
        res

    let helper f =
        let arTest1 = genRandomIntArray
        let arTest2 = genRandomIntArray
        let qTest1 = arToQTWS arTest1
        let qTest2 = arToQTWS arTest2
        let arRes = (arToQTWS (f arTest1 arTest2)).qtree
        (qTest1, qTest2, arRes)

    [<Tests>]
    let propertyListTests =
        testList "Comparing functions for arrays and for quadtrees"
            [
             testProperty "Comparing array and quadtree summ function" <| fun _ ->
                let res = helper arraySumm
                let qtres = (Quadtree.sumQuadTrWS (Quadtree.first res) (Quadtree.second res) monoidT).qtree
                Expect.equal qtres (Quadtree.third res) "Results of operations with arrays and Quadtree matrices should be equal"

             testProperty "Comparing array and quadtree mult function" <| fun _ ->
                let res = helper (MyTask.Ht3.matrixMult)
                let qtres = (Quadtree.multQuadTrWS (Quadtree.first res) (Quadtree.second res) semiringT).qtree
                Expect.equal qtres (Quadtree.third res) "Results of operations with arrays and Quadtree matrices should be equal" 

             testProperty "Comparing array and quadtree tensor product function" <| fun _ ->
                 let res = helper arTensMult
                 let qtres = (Quadtree.tenzMultQuadTr (Quadtree.first res) (Quadtree.second res) semiringT).qtree
                 Expect.equal qtres (Quadtree.third res) "Results of operations with arrays and Quadtree matrices should be equal" 

            ]
