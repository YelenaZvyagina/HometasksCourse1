module Hometask13.Ht13

open System
open Hometask13
open Quadtree
open AlgStcruct
open MyTask
open helpingFunctions
open Configs

type loaderMsg =
    | Go of AsyncReplyChannel<unit>
    | EOSl of AsyncReplyChannel<unit>

type balancerMsg =
    | EOSb of AsyncReplyChannel<unit>
    | СompMatrs of mbMatrConf<CompMatrix>

type multMsg =
    | Qtseq of mbMatrConf<QuadTree<int>>
    | Qtpar of mbMatrConf<QuadTree<int>>
    | Ar2dseq of mbMatrConf<int [,]>
    | Ar2dpar of mbMatrConf<int [,]>
    | EOSmult of AsyncReplyChannel<unit>

let matrixLoader inputPath (balancer:MailboxProcessor<balancerMsg>) count =
    MailboxProcessor.Start(fun inbox ->
        let rec loop files count =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSl ch ->
                    printfn "Matrix loader is ready to finish!"
                    balancer.PostAndReply (fun ch -> EOSb ch)
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        if count = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOSl ch)
                            return! loop files 0
                        elif count > 0 then
                            printfn "There are no more matrices to read. Matrix reading is finished. %A pairs read" count
                            inbox.Post (EOSl ch)
                            return! loop files 0
                        else failwith "Something went totally wrong"
                    | file1 :: file2 :: files ->
                        if count = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOSl ch)
                            return! loop files 0
                        else
                            printfn "Loading: %A and %A" file1 file2
                            let cmToSend = mbMatrConf( (toCompMatr file1), (toCompMatr file2), file1, file2, glConfigs)
                            balancer.Post (СompMatrs cmToSend)
                            inbox.Post (Go ch)
                            return! loop files (count-1)
                    | _ -> failwith "Total amount of matrices should be even"
            }
        loop (listAllFiles inputPath) count
    )

let balancer (qtSeqMult : MailboxProcessor<multMsg>) (qtParMult : MailboxProcessor<multMsg>) (arSeqMult : MailboxProcessor<multMsg>) (arParMult: MailboxProcessor<multMsg>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSb ch ->
                    printfn "Matrix balancer is ready to finish!"
                    qtSeqMult.PostAndReply (fun ch -> EOSmult ch)
                    arSeqMult.PostAndReply (fun ch -> EOSmult ch)
                    qtParMult.PostAndReply (fun ch -> EOSmult ch)
                    arParMult.PostAndReply (fun ch -> EOSmult ch)
                    printfn "Balancer is finished!"
                    ch.Reply()
                | СompMatrs (cmToSend) ->
                    if cmToSend.m1.lines = cmToSend.m2.columns
                    then
                        printfn "Processing %A and %A" cmToSend.fname1 cmToSend.fname2
                        match mbMatrConf<int>.sizeSparsityCheck (cmToSend.m1) (cmToSend.m2) cmToSend.conf with
                            | true, true -> qtParMult.Post (Qtpar (mbMatrConf<int>.confCmToQt cmToSend glConfigs))
                            | true, false -> arParMult.Post (Ar2dpar (mbMatrConf<int>.confCmto2d cmToSend glConfigs))
                            | false, true -> qtSeqMult.Post (Qtseq (mbMatrConf<int>.confCmToQt cmToSend glConfigs))
                            | false, false -> arSeqMult.Post (Ar2dseq (mbMatrConf<int>.confCmto2d cmToSend glConfigs))
                        return! loop ()
                    else
                        printfn "Processing %A and %A" cmToSend.fname1 cmToSend.fname2
                        printfn "Matrices of these sizes cannot be multiplied"
                        return! loop ()
            }
        loop ()
    )

let multiplier =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSmult ch ->
                    printfn "Mullting is finished!"
                    ch.Reply()
                    return! loop ()
                | Qtseq qtToSend ->
                    printfn "Multing: %A, %A using qtSeqMult" qtToSend.fname1 qtToSend.fname2
                    let res = multInner qtToSend.m1 qtToSend.m2 standSemiring
                    return! loop ()
                | Qtpar qtToSend ->
                    printfn "Multing: %A, %A using qtParMult" qtToSend.fname1 qtToSend.fname2
                    let res = parMultQuadTree qtToSend.m1 qtToSend.m2 standSemiring 1
                    return! loop ()
                | Ar2dseq ar2dToSend ->
                    printfn "Multing: %A, %A using arSeqMult" ar2dToSend.fname1 ar2dToSend.fname2
                    let res = Ht3.matrixMult ar2dToSend.m1 ar2dToSend.m2
                    return! loop ()
                | Ar2dpar ar2dToSend ->
                    printfn "Multing: %A, %A using arParMult" ar2dToSend.fname1 ar2dToSend.fname2
                    let res = parMatrixMult ar2dToSend.m1 ar2dToSend.m2
                    return! loop ()

            }
        loop ()
        )

let arSeqMult = multiplier
let arParMult = multiplier
let qtSeqMult = multiplier
let qtParMult = multiplier

let processSomeFilesAsync inputPath amount =
    let balancer = balancer qtSeqMult qtParMult arSeqMult arParMult
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply(fun ch -> Go ch)

let processAllFilesAsync inputPath =
    let amount = (listAllFiles inputPath).Length/2
    let balancer = balancer qtSeqMult qtParMult arSeqMult arParMult
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply(fun ch -> Go ch)



