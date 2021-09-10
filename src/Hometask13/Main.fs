open Argu
open Hometask13.Ht13

    type CLIArguments =
        |MultSome of path:string * amount:int
        |MultAll of path:string

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                |MultSome _ -> "Multiplies concrete amount of pairs of matrices"
                |MultAll _ -> "Multiplies all pairs of matrices"

    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MatricesMailbox")
            let results = parser.Parse(argv)
            let args = parser.ParseCommandLine argv

            if args.Contains(MultSome)
            then
                let inputpath, amount = args.GetResult(MultSome)
                processSomeFilesAsync inputpath amount
            elif args.Contains(MultAll)
            then
                let inputpath = args.GetResult(MultAll)
                processAllFilesAsync inputpath
            else printfn "No such regime, sorry"
            0
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1
