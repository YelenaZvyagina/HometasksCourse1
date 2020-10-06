module Ht3.Tests 

open Expecto
open MyTask

[<Tests>]
let fibRecTests =
    testList "Tests for task 1"
       [
        testCase "Fibonacci 0 element using recursive method" <| fun _ ->
           let subject = Ht3.fibRec 0
           Expect.equal subject 0 "Fibonacci 0 element is 0"
        
        testCase "Fibonacci 5th element using recursion" <| fun _ ->
            let subject = Ht3.fibRec 5
            Expect.equal subject 5 "Fibonacci 5th element is 5"
       ]

[<Tests>]
let fibIterTests =
    testList "Tests for task 2"
       [
        testCase "Fibonacci 0 element using iterative method" <| fun _ ->
                   let subject = Ht3.fibIter 0
                   Expect.equal subject 0 "Fibonacci 0 element is 0"

        testCase "Fibonacci 1st element using iterative method" <| fun _ ->
            let subject = Ht3.fibIter 1
            Expect.equal subject 1 "Fibonacci 1st element is 1"
       ]

[<Tests>]
let fibTailTests =
    testList "Tests for task 3"
       [
        testCase "Fibonacci 0 element using tail recursion" <| fun _ ->
            let subject = Ht3.fibTail 0
            Expect.equal subject 0 "Fibonacci 0 element is 0"

        testCase "Fibonacci 8th element using tail recursion" <| fun _ ->
            let subject = Ht3.fibTail 8
            Expect.equal subject 21 "Fibonacci 8th element is 21"
       ]

[<Tests>]
let fibMatrixNaiveTests =
    testList "Tests for task 4"
       [
        testCase "Fibonacci 1st element using naive matrix method" <| fun _ ->
            let subject = Ht3.fibMatrixNaive 1
            Expect.equal subject 1 "Fibonacci 1st element is 1"
        
        testCase "Fibonacci 3d element using naive matrix method" <| fun _ ->
            let subject = Ht3.fibMatrixNaive 3
            Expect.equal subject 2 "Fibonacci 3d element is 2"
       ]

[<Tests>]
let fibMatrixLogTests =
    testList "Tests for task 5"
       [
        testCase "Fibonacci 3d element using log matrix method" <| fun _ ->
            let subject = Ht3.fibMatrixLog 3
            Expect.equal subject 2 "Fibonacci 3d element is 2"

        testCase "Fibonacci 0 element using log matrix method" <| fun _ ->
            let subject = Ht3.fibMatrixLog 0
            Expect.equal subject 0 "Fibonacci 0 element is 0"

        testCase "Fibonacci 1st element using log matrix method" <| fun _ ->
            let subject = Ht3.fibMatrixLog 1
            Expect.equal subject 1 "Fibonacci 1st element is 1"
       ]

[<Tests>]
let fibCalcTests =
    testList "Tests for task 6"
       [
        testCase "Fibonacci sequence till 2nd element" <| fun _ ->
           let subject = Ht3.fibCalc 2
           Expect.equal subject [|0; 1; 1|] "Fibonacci 0, 1st and 2nd elements are 0, 1, 1"

        testCase "Fibonacci sequence till 0 element" <| fun _ ->
           let subject = Ht3.fibCalc 0
           Expect.equal subject [|0|] "Fibonacci 0 element is 0"

        testCase "Fibonacci sequence till 5th element" <| fun _ ->
           let subject = Ht3.fibCalc 5
           Expect.equal subject [|0; 1; 1; 2; 3; 5|] "Fibonacci 0, 1st, 2nd, 3d, 4th, 5th elements are 0, 1, 1, 2, 3, 5"
       ]

[<Tests>]
let testPropertyTests =
    testList "Tests using Test Property"
       [
        testProperty "Comparing tasks 2 and 3" <| fun (n:int) ->
            Expect.equal (Ht3.fibIter (abs n) ) (Ht3.fibTail (abs n) ) "Results for fibIter and fibTail should be equal"

        testProperty "Comparing tasks 3 and 4" <| fun (n:int) ->
                   Expect.equal (Ht3.fibTail (abs n) ) (Ht3.fibMatrixNaive (abs n) ) "Results for fibTail and fibMatrixNaive should be equal"

        testProperty "Comparing tasks 2 and 4" <| fun (n:int) ->
                   Expect.equal (Ht3.fibIter (abs n) ) (Ht3.fibMatrixNaive (abs n) ) "Results for fibIter and fibMatrixNaive should be equal"
        
        testProperty "Comparing tasks 2 and 5" <| fun (n:int) ->
            Expect.equal (Ht3.fibIter (abs n) ) (Ht3.fibMatrixLog (abs n) ) "Results for fibIter and fibMatrixLog should be equal"

        testProperty "Comparing tasks 4 and 5" <| fun (n:int) ->
            Expect.equal (Ht3.fibMatrixNaive (abs n) ) (Ht3.fibMatrixLog (abs n) ) "Results for fibMatrixNaive and fibMatrixLog should be equal"

        testProperty "Comparing tasks 3 and 5" <| fun (n:int) ->
            Expect.equal (Ht3.fibTail (abs n) ) (Ht3.fibMatrixLog (abs n) ) "Results for fibTail and fibMatrixLog should be equal"
       ]   
