namespace MyTask

module Main =
    open Argu

    open System

    type CLIArguments =
        |Task1 of x:int 
        |Task2 of x:int 
        |Task3 of n:int * x:int
        |Task4 of n:int * a:int * b:int
        |Task5 
        |Task6 of n:int * i:int * j:int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Task1 _ -> "Solves task number 1. Enter x"
                | Task2 _ -> "Solves task number 2. Enter x"
                | Task3 _ -> "Solves task number 3. Enter n - number of elements in array. Enter x" 
                | Task4 _ -> "Solves task number 4. Enter n - number of elements in array. Enter a - left limit. Enter b - right limit"
                | Task5  -> "Solves task number 5."
                | Task6 _ -> "Solves task number 6. Enter n - number of elements in array. Enter i. Enter j."

    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MyTask")
            let results = parser.Parse(argv)
            let args = parser.ParseCommandLine argv
            
            if args.Contains(Task1)
            then
                let x = args.GetResult(Task1)
                let res = Ht_2.task1 x
                printfn "Task 1 result = %A" res
                
            elif args.Contains(Task2)
            then
                let x = args.GetResult(Task2)
                let res = Ht_2.task2 x
                printfn "Task 2 result = %A" res
            elif args.Contains(Task3)
            then
                let n, x = args.GetResult(Task3)
                let ar = Ht_2.randomArray n
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task3 (ar, x)
                printf "Task 3 result = %A" res
            elif args.Contains(Task4)
            then
                let n, a, b = args.GetResult(Task4)
                let ar = Ht_2.randomArray n
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task4 (ar, a, b)
                printf "Task 4 result = %A" res
            elif args.Contains(Task5)
            then
                let ar = Ht_2.randomArray 2
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task5 (ar)
                printf "Task 5 result = %A" res
            elif args.Contains(Task6)
            then
                let n, i, j = args.GetResult(Task6)
                let ar = Ht_2.randomArray n
                printf "Generated array: "
                printfn "%A" ar
                let res = Ht_2.task6 (ar, i, j)
                printf "Task 6 result = %A" res
            else printfn "This task doesn't exist"
            0
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1
