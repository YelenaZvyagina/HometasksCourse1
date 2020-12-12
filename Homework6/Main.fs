namespace HT6
module main =

    open System

    [<EntryPoint>]
    let main argv =
        Console.WriteLine("Enter the input file paths")
        let input1 = Console.ReadLine()
        let input2 = Console.ReadLine()
        Console.WriteLine("Enter the output file path")
        let output = Console.ReadLine()
        let matrix = Ht6.boolMatOper input1 input2 output
        0
