
module Ht_2.Tests 

open Expecto
open MyTask

[<Tests>]
let tests =
    testList "Ht_2 Tests"
       [testCase "Результат = 0 при x = 0" <| fun _ ->
            let subject = Ht_2.task1 0
            Expect.equal subject 1 "0 в любой ненулевой степени равен 0. 0 + 1 = 1"

        testCase "Результат = 0 при x = -1" <| fun _ ->
              let subject = Ht_2.task1 0
              Expect.equal subject 1 "-1 в четной степени = 1, а в нечетной - -1"

        testCase "Результат < 0, он = -2, при x = -2" <| fun _ ->
            let subject = Ht_2.task2 -2
            Expect.equal subject 11 "x^4 >= x^3, x^2 >= x для всех х. операция +1 делает результат всегда положительным"

        testCase "Результат = 0, x = 0" <| fun _ ->
            let subject = Ht_2.task2 0
            Expect.equal subject 1 "0 в любой ненулевой степени равен 0. 0 + 1 = 1"

        testCase "Результат должен выводить пустой массив" <| fun _ ->
            let subject = Ht_2.task3 ([|7; 8|], 2)
            Expect.equal subject [||] "7 > 2 и 8 > 2"

        testCase "На вход подается пустой массив, функция должна возвращать пустой массив" <| fun _ ->
            let subject = Ht_2.task3 ([||], 6)
            Expect.equal subject [||] "Элементы не могут появиться из ниоткуда, как и их индексы"

        testCase "Результат должен выводить массив индексов, содержащий индексы 0 и 1." <| fun _ ->
            let subject = Ht_2.task3 ([|0; 2|], 2)
            Expect.equal subject [|0; 1|] "Положительное число всегда больше 0. Любое число не меньше самого себя"

        testCase "Элементы с индексами 0, 2, 3 и 4 должны лежать вне диапазона 1-3" <| fun _ ->
            let subject = Ht_2.task4 ([|0; 2; 9; 8; 7|], 1, 3)
            Expect.equal subject [|0; 2; 3; 4|] "1<2<3"

        testCase "Элементы массива должны меняться местами" <| fun _ ->
            let subject = Ht_2.task5 [|6; 5|] 
            Expect.equal subject [|5; 6|] "Элементы массива должны меняться местами"

        testCase "На вход подается пустой массив, функция возврaщать пустой массив" <| fun _ ->
            let subject = Ht_2.task6 ([||], 6, 8)
            Expect.equal subject ([||]) "Элементы не могут появиться из ниоткуда, как и их индексы"

        testCase "Элементы массива с указанными индексами должны меняться местами" <| fun _ ->
            let subject = Ht_2.task6 ([|6; 1; 5|], 0, 1)
            Expect.equal subject ([|1; 6; 5|]) "Элементы массива с указанными индексами должны меняться местами"

       ]

        

       
          
