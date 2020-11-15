namespace HT4

open System

module Main =
    open Argu

    open System

    type CLIArguments =
        
        |Pack32to64 of a:int32 * b:int32
        |Pack64to32 of c:int64
        |Pack16to64 of a:int16 * b:int16 * c:int16 * d:int16
        |Pack64to16 of a:int64
        |ArQuickSort of i:string * o:string
        |ArBubbleSort of i:string * o:string
        |ListQuickSort of i:string * o:string
        |ListBubbleSort of i:string * o:string
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                |Pack32to64 _ -> "Packs 2 of int32 into 1 of int64. Enter the numbers to pack"
                |Pack64to32 _ -> "Packs 1 of int64 into 2 of int32. Enter the number to pack"
                |Pack16to64 _ -> "Packs 4 of int16 into 1 of int64. Enter the numbers to pack"
                |Pack64to16 _ -> "Packs 1 of int64 into 4 of int16. Enter the number to pack"
                |ArQuickSort _ -> "Sorts array using quick sort"
                |ArBubbleSort _ -> "Sorts array using bubble sort"
                |ListQuickSort _ -> "Sorts list using quick sort"
                |ListBubbleSort _ -> "Sorts list using bubble sort"


    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MyTask")
            let results = parser.Parse(argv)
            let args = parser.ParseCommandLine argv

            let forSorts cliarg readfun sortfun writefun =
                let i, o = fst cliarg, snd cliarg
                writefun (o, sortfun (readfun i))

            let forSomePacks cliarg packfun =
                let i = cliarg
                let res =  packfun i
                printfn "%A" res

            if args.Contains(Pack32to64)
            then
                let a, b = args.GetResult(Pack32to64)
                let res = Ht4.pack32to64 a b
                printfn "%A" res
            elif args.Contains(Pack16to64)
            then
                let a, b, c, d = args.GetResult(Pack16to64)
                let res = Ht4.pack16to64(a, b, c, d)
                printfn "%A" res
            elif args.Contains(Pack64to32)
            then
                forSomePacks (args.GetResult(Pack64to32)) Ht4.pack64to32
            elif args.Contains(Pack64to16)
            then
                forSomePacks (args.GetResult(Pack64to16)) Ht4.pack64to16
            elif args.Contains(ArBubbleSort)
            then
                forSorts (args.GetResult(ArBubbleSort)) Ht4.readArray Ht4.arBubbleSort Ht4.writeArray
            elif args.Contains(ArQuickSort)
            then
                forSorts (args.GetResult(ArQuickSort)) Ht4.readArray Ht4.arQuickSort Ht4.writeArray
            elif args.Contains(ListBubbleSort)
            then
                forSorts (args.GetResult(ListBubbleSort)) Ht4.readList Ht4.listBubbleSort Ht4.writeList
            elif args.Contains(ListQuickSort)
            then
                forSorts (args.GetResult(ListQuickSort)) Ht4.readList Ht4.listQuickSort Ht4.writeList
            else printfn "This task doesn't exist"
            0
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1
