namespace Hw6Tests

module Ht6Tests =

    open Expecto
    open System
    open Ht6   

    [<Tests>]

    let multBoolMatrixesTests =
        testList "Tests for multBoolMatrix function"
            [
                testCase "Multiplying matrixes of 3 * 3 and 3 * 3" <| fun _ ->
                    let m1 = new Ht6.BoolMatrix (3*1<str>, 3*1<col>, [Cell(1*1<str>, 1*1<col>)])
                    let m2 = new Ht6.BoolMatrix (3*1<str>, 3*1<col>, [Cell(1*1<str>, 1*1<col>)])
                    let subject = Ht6.multBoolMatrix (m1, m2)
                    let m = new Ht6.BoolMatrix (3*1<str>, 3*1<col>, [Cell(1*1<str>, 1*1<col>)])
                    Expect.equal subject m "This file should result this matrix" 

                testCase "Multiplying matrixes of 4 * 3 and 3 * 4" <| fun _ ->
                    let m1 = new Ht6.BoolMatrix (4*1<str>, 3*1<col>, [Cell(1*1<str>, 1*1<col>)])
                    let m2 = new Ht6.BoolMatrix (3*1<str>, 4*1<col>, [Cell(1*1<str>, 1*1<col>)])
                    let subject = Ht6.multBoolMatrix (m1, m2)
                    let m = new Ht6.BoolMatrix (4*1<str>, 4*1<col>, [Cell(1*1<str>, 1*1<col>)])
                    Expect.equal subject m "This file should result this matrix"

                testCase "Multiplying matrixes of 2 * 2 and 2 * 2" <| fun _ ->
                    let m1 = new Ht6.BoolMatrix (2*1<str>, 2*1<col>, [Cell(0*1<str>, 1*1<col>)])
                    let m2 = new Ht6.BoolMatrix (2*1<str>, 2*1<col>, [Cell(1*1<str>, 0*1<col>)])
                    let subject = Ht6.multBoolMatrix (m1, m2)
                    let m = new Ht6.BoolMatrix (2*1<str>, 2*1<col>, [Cell(0*1<str>, 0*1<col>)])
                    Expect.equal subject m "This file should result this matrix"

                testCase "Multiplying matrixes of 2 * 3 and 3 * 2" <| fun _ ->
                    let m1 = new Ht6.BoolMatrix (2*1<str>, 3*1<col>, [Cell(1*1<str>, 2*1<col>)])
                    let m2 = new Ht6.BoolMatrix (3*1<str>, 2*1<col>, [Cell(2*1<str>, 0*1<col>)])
                    let subject = Ht6.multBoolMatrix (m1, m2)
                    let m = new Ht6.BoolMatrix (2*1<str>, 2*1<col>, [])
                    Expect.equal subject m "This file should result this matrix"
            ]

    [<Tests>]

    let boolMatrixToMatrixTests =
        testList "Tests for boolMatrixToMatrix function"
            [
                testCase "Transforming bool matrix 2 * 2" <| fun _ ->
                    let m1 = new Ht6.BoolMatrix (2*1<str>, 2*1<col>, [Cell(1*1<str>, 0*1<col>)])
                    let subject = Ht6.boolMatrixToMatrix m1
                    let m = array2D [| [|false; false |]; [| true; false |] |]
                    Expect.equal subject m "Matrixes should be equal"
            ]
