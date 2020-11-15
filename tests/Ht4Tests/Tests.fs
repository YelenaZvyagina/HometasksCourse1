namespace HT4

module Ht4Tests =

    open Expecto
    open System

    [<Tests>]

    let emptyCaseSortsTests =
        testList "Empty cases tests for sorts functions"
            [
             let emptyCases func inputVal expectedVal =
                 Expect.equal (func inputVal) expectedVal "There is nothing to sort in empty structure"

             testCase "Sorting empty array with Quick Sort" <| fun _ -> emptyCases Ht4.arQuickSort [||] [||]
             testCase "Sorting empty array with Bubble Sort" <| fun _ -> emptyCases Ht4.arBubbleSort [||] [||]
             testCase "Sorting empty list with Quick Sort" <| fun _ -> emptyCases Ht4.listQuickSort [] []
             testCase "Sorting empty list with Bubble Sort" <| fun _ -> emptyCases Ht4.listBubbleSort [] []
            ]

    [<Tests>]
    let propertySortTests = 
        testList "Tests for Sorts functions using Test Property"
            [
             let tpSort func1 func2 inputVal msg =
                Expect.equal (func1 inputVal) (func2 inputVal) msg

             testProperty "Comparing sorts for lists" <| fun (l:list<int>) -> tpSort Ht4.listQuickSort Ht4.listBubbleSort (l:list<int>) "Results for listQuickSort and listBubbleSort should be equal"
             testProperty "Comparing Quick sort and System Sort for lists" <| fun (l:list<int>) -> tpSort Ht4.listQuickSort  List.sort (l:list<int>) "Results for listQuickSort and system list sort should be equal"
             testProperty "Comparing Bubble sort and System Sort for lists" <| fun (l:list<int>) -> tpSort Ht4.listBubbleSort  List.sort (l:list<int>) "Results for listBubbleSort and system list sort should be equal"
            ] 

    [<Tests>]
    let propertyPackTests =
        testList "Tests for packing functions using Test Property"
            [
             testProperty  "Packing int64 to int32 and unpacking back" <| fun a b ->
                 Expect.equal (a, b) (Ht4.pack64to32(Ht4.pack32to64 a b)) "Results should be equal"

             testProperty  "Unpacking int64 to int16 and packing back" <| fun (a:int64) ->
                 Expect.equal a (Ht4.pack16to64(Ht4.pack64to16 a)) "Results should be equal"
            ]
