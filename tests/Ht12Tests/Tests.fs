namespace Ht12Tests

module Ht12Tests =

    open Expecto
    open System
    open Ht12
    open Quadtree
    open AlgStcruct

    let semiringT = Semiring(new Semiring<int>(new Monoid<int>(0, (+)), (*)))

    let genRandomQuadTree h lim =
        let rand = new System.Random()
        let rec go h count =
            if h = 1
            then
                let pos = rand.Next(1, 100)
                if pos % 2 = 1 then Leaf(rand.Next(1, 50))
                else None
            else
                if count < lim
                then
                    let n1 = async{return go (h-1) (count + 1)}
                    let n2 = async{return go (h-1) (count + 1)}
                    let n3 = async{return go (h-1) (count + 1)}
                    let n4 = async{return go (h-1) (count + 1)}
                    let nds = [n1;n2;n3;n4] |> Async.Parallel |> Async.RunSynchronously
                    reduceNone (nds.[0], nds.[1], nds.[2], nds.[3])
                else Node (go (h-1) (count + 1), go (h-1) (count + 1), go (h-1) (count+1), go (h-1) (count+1) )
        go h 0

    let time f =
        let start = System.DateTime.Now
        let r = f()
        let time = (System.DateTime.Now - start).TotalMilliseconds
        r,time 

    [<Tests>]
        let parMultQtwsTests =
            testList "Comparing functions for arrays and for quadtrees"
             [
              testProperty "parMultQuadTrWS works correctly" <| fun _ ->
                let a = (new System.Random()).Next(1, 5)
                let qt1 = genRandomQuadTree a 1
                let qt2 = genRandomQuadTree a 1
                let seqRes = multInner qt1 qt2 semiringT
                let parRes = parMultQuadTree qt1 qt2 semiringT 1
                Expect.equal seqRes parRes "Results of sequential and parallel functions should be equal"
             ]

