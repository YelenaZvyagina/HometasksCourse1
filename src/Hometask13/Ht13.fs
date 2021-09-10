module Hometask13.Ht13

open Quadtree
open AlgStcruct
open MyTask

let parMatrixMult (m1: int[,]) (m2: int[,]) =
    if (m1.GetLength 1 = m2.GetLength 0)
    then
        let a = m1.GetLength 0
        let b = m1.GetLength 1
        let c = m2.GetLength 1
        let res = Array2D.zeroCreate a c
        [ for i in 0 .. a - 1 ->
            async {
                do
                    for j in 0 .. c - 1 do
                        for k in 0 .. b - 1 do
                            res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
            }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        res
    else failwith "It's impossible to multiply matrixes of this sizes"

type msg =
    | Go of AsyncReplyChannel<unit>
    | EOS of AsyncReplyChannel<unit>
    | Matrices of string*CompMatrix*string*CompMatrix*int*AsyncReplyChannel<unit>
    | Qt of string*QuadTree<int>*string*QuadTree<int>
    | Ar2d of string*int[,]*string*int[,]

let listAllFiles inputPath =
    let files = System.IO.Directory.GetFiles(inputPath)
    List.ofArray files

let matrixLoader inputPath (balancer:MailboxProcessor<_>) count =
    MailboxProcessor.Start(fun inbox ->
        let mutable remain = count
        let rec loop files count =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Matrix loader is ready to finish!"
                    balancer.PostAndReply (fun ch -> EOS ch)
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        if remain = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOS ch)
                            return! loop files 0
                        elif remain > 0 then
                            printfn "There are no more matrices to read. Matrix reading is finished. %A pairs read" (count - remain)
                            inbox.Post (EOS ch)
                            return! loop files 0
                        else failwith "Something went totally wrong"
                    | file :: files ->
                        if remain = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOS ch)
                            return! loop files 0
                        else
                            printfn "Loading: %A" file
                            let file2 = files.Head
                            let matr1 = toCompMatr file
                            printfn "Loading: %A" file2
                            let matr2 = toCompMatr file2
                            remain <- remain - 1
                            balancer.Post (Matrices (file, matr1, file2, matr2, remain, ch))
                            inbox.Post (Go ch)

                            return! loop files.Tail count
            }
        loop (listAllFiles inputPath) count
        )

let balancer (qtSeqMult : MailboxProcessor<_>) (qtParMult : MailboxProcessor<_>) (arSeqMult : MailboxProcessor<_>) (arParMult : MailboxProcessor<_>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Matrix balancer is ready to finish!"
                    qtSeqMult.PostAndReply (fun ch -> EOS ch)
                    arSeqMult.PostAndReply (fun ch -> EOS ch)
                    qtParMult.PostAndReply (fun ch -> EOS ch)
                    arParMult.PostAndReply (fun ch -> EOS ch)
                    printfn "Balancer is finished!"
                    ch.Reply()
                | Matrices (file1, matr1, file2, matr2, remain, ch) ->
                    if matr1.lines = matr2.columns then
                        printfn "Processing %A and %A" file1 file2
                        let big = (matr1.lines >= 1000 || matr2.columns >= 1000)
                        let sparse = (matr1.lines*matr1.columns >=  2*matr1.lstNonZero.Length || matr2.lines*matr2.columns >= 2*matr2.lstNonZero.Length)
                        match big, sparse with
                        | true, true -> qtParMult.Post (Qt (file1, (cmatrToQt matr1), file2, (cmatrToQt matr2)))
                        | true, false -> arParMult.Post (Ar2d (file1, (cmatrTo2d matr1), file2, (cmatrTo2d matr2)))
                        | false, true -> qtSeqMult.Post (Qt (file1, (cmatrToQt matr1), file2, (cmatrToQt matr2)))
                        | false, false -> arSeqMult.Post (Ar2d (file1, (cmatrTo2d matr1), file2, (cmatrTo2d matr2)))
                        return! loop
                    else
                        printfn "Processing %A and %A" file1 file2
                        printfn "Matrices of these sizes cannot be multiplied"
                        if remain = 0 then
                            qtSeqMult.PostAndReply (fun ch -> EOS ch)
                            arSeqMult.PostAndReply (fun ch -> EOS ch)
                            qtParMult.PostAndReply (fun ch -> EOS ch)
                            arParMult.PostAndReply (fun ch -> EOS ch)
                            ch.Reply()
                        return! loop

            }
        loop
        )

let qtSeqMult =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "qtSeqMult is finished!"
                    ch.Reply()
                | Qt (file1, qt1, file2, qt2) ->
                    printfn "Multing: %A, %A using qtSeqMult" file1 file2
                    let res = multInner qt1 qt2 standSemiring
                    return! loop ()
            }
        loop ()
        )

let qtParMult =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "qtParMult is finished!"
                    ch.Reply()
                | Qt (file1, qt1, file2, qt2) ->
                    printfn "Multing: %A, %A using qtParMult" file1 file2
                    let res = parMultQuadTree qt1 qt2
                    return! loop ()
            }
        loop ()
        )

let arSeqMult =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "arSeqMult is finished!"
                    ch.Reply()
                | Ar2d (file1, ar1, file2, ar2) ->
                    printfn "Multing: %A, %A using arSeqMult" file1 file2
                    let res = Ht3.matrixMult ar1 ar2
                    return! loop ()
            }
        loop ()
        )

let arParMult =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "arParMult is finished!"
                    ch.Reply()
                | Ar2d (file1, ar1, file2, ar2) ->
                    printfn "Multing: %A, %A using arParMult" file1 file2
                    let res = parMatrixMult ar1 ar2
                    return! loop ()
            }
        loop ()
        )

let processSomeFilesAsync inputPath amount =
    let balancer = balancer qtSeqMult qtParMult arSeqMult arParMult
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply(fun ch -> Go ch)

let processAllFilesAsync inputPath =
    let amount = (listAllFiles inputPath).Length/2
    let balancer = balancer qtSeqMult qtParMult arSeqMult arParMult
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply(fun ch -> Go ch)
