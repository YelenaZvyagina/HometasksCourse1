namespace Ht8Tests

module Ht8Tests = 

    open System
    open Expecto
    open Quadtree
    open AlgStcruct

    let monoidT = Monoid(new Monoid<int>(0, (+)))
    let semiringT = Semiring(new Semiring<int>(new Monoid<int>(0, (+)), (*)))
    let qtws1 = new QuadTreeWithSize<int>( (Node (Leaf (3), None, Leaf (4), None)), 2, 2) 
    let qtws2 = new QuadTreeWithSize<int>( (Node (None, None, Leaf (34), None)), 2, 2)
    let qtws3 = new QuadTreeWithSize<int>(Node(Leaf (2), None, None, Leaf (3)), 2, 2)
    let qtws4 = new QuadTreeWithSize<int>(Node(None, Leaf (6), Leaf (1), None), 2, 2)
    let qtws5 = new QuadTreeWithSize<int>(Node((Node( Leaf (1), None, None, Leaf(5) ) ), None, (Node( None, Leaf (7), None, Leaf(2) )), None), 4, 4)
    let qtws6 = new QuadTreeWithSize<int>(Node( None, (Node( Leaf (8), None, None, Leaf(6) ) ), (Node( None, Leaf (3), None, Leaf(14) )), None), 4, 4)
 
   
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


