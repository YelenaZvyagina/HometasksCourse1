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
    | MatrsForBalancer of string*CompMatrix*string*CompMatrix*int*AsyncReplyChannel<unit>
    | MatrsForMult of string*CompMatrix*string*CompMatrix

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
                    | file1 :: file2 :: files ->
                        if remain = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOS ch)
                            return! loop files 0
                        else
                            printfn "Loading: %A and %A" file1 file2
                            let matr1 = toCompMatr file1
                            let matr2 = toCompMatr file2
                            remain <- remain - 1
                            balancer.Post (MatrsForBalancer (file1, matr1, file2, matr2, remain, ch))
                            inbox.Post (Go ch)
                            return! loop files count
                    | _ -> failwith "Something went wrong"
                | _ -> failwith "Something went wrong"
            }
        loop (listAllFiles inputPath) count
    )

let balancer (qtSeqMult : MailboxProcessor<_>) (qtParMult : MailboxProcessor<_>) (arSeqMult : MailboxProcessor<_>) (arParMult : MailboxProcessor<_>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
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
                | MatrsForBalancer (file1, matr1, file2, matr2, remain, ch) ->
                    if matr1.lines = matr2.columns then
                        printfn "Processing %A and %A" file1 file2
                        let big = (matr1.lines >= 1000 || matr2.columns >= 1000)
                        let sparse = (matr1.lines*matr1.columns >=  2*matr1.lstNonZero.Length || matr2.lines*matr2.columns >= 2*matr2.lstNonZero.Length)
                        match big, sparse with
                        | true, true -> qtParMult.Post (MatrsForMult (file1, matr1, file2, matr2))
                        | true, false -> arParMult.Post (MatrsForMult (file1, matr1, file2, matr2))
                        | false, true -> qtSeqMult.Post (MatrsForMult (file1, matr1, file2, matr2))
                        | false, false -> arSeqMult.Post (MatrsForMult (file1, matr1, file2, matr2))
                        return! loop ()
                    else
                        printfn "Processing %A and %A" file1 file2
                        printfn "Matrices of these sizes cannot be multiplied"
                        if remain = 0 then
                            qtSeqMult.PostAndReply (fun ch -> EOS ch)
                            arSeqMult.PostAndReply (fun ch -> EOS ch)
                            qtParMult.PostAndReply (fun ch -> EOS ch)
                            arParMult.PostAndReply (fun ch -> EOS ch)
                            ch.Reply()
                        return! loop ()
                    | _ -> failwith "Something went wrong"
            }
        loop ()
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
                | MatrsForMult (file1, matr1, file2, matr2) ->
                    printfn "Multing: %A, %A using qtSeqMult" file1 file2
                    let res = multInner (cmatrToQt matr1) (cmatrToQt matr2) standSemiring
                    return! loop ()
                | _ -> failwith "Something went wrong"
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
                | MatrsForMult (file1, matr1, file2, matr2) ->
                    printfn "Multing: %A, %A using qtParMult" file1 file2
                    let res = parMultQuadTree (cmatrToQt matr1) (cmatrToQt matr2)
                    return! loop ()
                | _ -> failwith "Something went wrong"
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
                | MatrsForMult (file1, matr1, file2, matr2)  ->
                    printfn "Multing: %A, %A using arSeqMult" file1 file2
                    let res = Ht3.matrixMult (cmatrTo2d matr1) (cmatrTo2d matr2)
                    return! loop ()
                | _ -> failwith "Something went wrong"
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
                | MatrsForMult (file1, matr1, file2, matr2) ->
                    printfn "Multing: %A, %A using arParMult" file1 file2
                    let res = parMatrixMult (cmatrTo2d matr1) (cmatrTo2d matr2)
                    return! loop ()
                | _ -> failwith "Something went wrong"
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
