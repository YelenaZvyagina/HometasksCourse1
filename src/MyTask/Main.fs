namespace MyTask

module Main =
    open Argu

    open System

    type CLIArguments =
        
        |Ht2Task1 of x :int 
        |Ht2Task2 of x :int 
        |Ht2Task3 of n :int * x:int
        |Ht2Task4 of n :int * a:int * b:int
        |Ht2Task5 
        |Ht2Task6 of n :int * i:int * j:int

        |Ht3Task1 of n :int 
        |Ht3Task2 of n :int 
        |Ht3Task3 of n :int
        |Ht3Task4 of n :int
        |Ht3Task5 of n :int
        |Ht3Task6 of n :int

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Ht2Task1 _ -> "Solves task number 1. Enter x"
                | Ht2Task2 _ -> "Solves task number 2. Enter x"
                | Ht2Task3 _ -> "Solves task number 3. Enter n - number of elements in array. Enter x" 
                | Ht2Task4 _ -> "Solves task number 4. Enter n - number of elements in array. Enter a - left limit. Enter b - right limit"
                | Ht2Task5  -> "Solves task number 5."
                | Ht2Task6 _ -> "Solves task number 6. Enter n - number of elements in array. Enter i. Enter j."
                | Ht3Task1 _ -> "Computes fibonacci element using recursion. Enter n - number of element"
                | Ht3Task2 _ -> "Computes fibonacci element using iterative method. Enter n - number of element"
                | Ht3Task3 _ -> "Computes fibonacci element using tail recursion. Enter n - number of element"
                | Ht3Task4 _ -> "Computes fibonacci element using naive matrix method. Enter n - number of element"
                | Ht3Task5 _ -> "Computes fibonacci element using log matrix method. Enter n - number of element"
                | Ht3Task6 _ -> "Computes fibonacci elements from 0 to n. Enter n - number of element"
                


    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MyTask")
            let results = parser.Parse(argv)
            let args = parser.ParseCommandLine argv

            if args.Contains(Ht2Task1)
            then
                let x = args.GetResult(Ht2Task1)
                let res = Ht_2.task1 x
                printfn "Task 1 result = %A" res
            elif args.Contains(Ht2Task2)
            then
                let x = args.GetResult(Ht2Task2)
                let res = Ht_2.task2 x
                printfn "Task 2 result = %A" res
            elif args.Contains(Ht2Task3)
            then
                let n, x = args.GetResult(Ht2Task3)
                let ar = Ht_2.randomArray n
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task3 (ar, x)
                printf "Task 3 result = %A" res
            elif args.Contains(Ht2Task4)
            then
                let n, a, b = args.GetResult(Ht2Task4)
                let ar = Ht_2.randomArray n
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task4 (ar, a, b)
                printf "Task 4 result = %A" res
            elif args.Contains(Ht2Task5)
            then
                let ar = Ht_2.randomArray 2
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task5 (ar)
                printf "Task 5 result = %A" res
            elif args.Contains(Ht2Task6)
            then
                let n, i, j = args.GetResult(Ht2Task6)
                let ar = Ht_2.randomArray n
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task6 (ar, i, j)
                printf "Task 6 result = %A" res


            elif args.Contains(Ht3Task1)
            then
                let n = args.GetResult(Ht3Task1)
                let res = Ht3.fibRec n
                printfn "Task 1 result = %A" res
            elif args.Contains(Ht3Task2)
            then
                let n = args.GetResult(Ht3Task2)
                let res = Ht3.fibIter n
                printfn "Task 2 result = %A" res
            elif args.Contains(Ht3Task3)
            then
                let n = args.GetResult(Ht3Task3)
                let res = Ht3.fibTail n
                printfn "Task 3 result = %A" res
            elif args.Contains(Ht3Task4)
            then
                let n = args.GetResult(Ht3Task4)
                let res = Ht3.fibMatrixNaive n
                printfn "Task 4 result = %A" res
            elif args.Contains(Ht3Task5)
            then
                let n = args.GetResult(Ht3Task5)
                let res = Ht3.fibMatrixLog n
                printfn "Task 5 result = %A" res
            elif args.Contains(Ht3Task6)
            then
                let n = args.GetResult(Ht3Task6)
                let res = Ht3.fibCalc n
                printfn "Task 6 result = %A" res
            else printfn "This task doesn't exist"
            0
             
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1

