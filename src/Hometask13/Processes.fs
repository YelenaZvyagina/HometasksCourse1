module Hometask13.Ht13

open System
open Hometask13
open Quadtree
open AlgStcruct
open MyTask
open helpingFunctions

type loaderMsg =
    | Go of AsyncReplyChannel<unit>
    | EOSloader of AsyncReplyChannel<unit>

type balancerMsg =
    | EOSbalancer of AsyncReplyChannel<unit>
    | СompMatrs of mbMatrConf<CompMatrix>

type multMsg =
    | Qt of mbMatrConf<QuadTree<int>>
    | Ar2d of mbMatrConf<int [,]>
    | EOSmult of AsyncReplyChannel<unit>

let matrixLoader inputPath (balancer:MailboxProcessor<balancerMsg>) count =
    MailboxProcessor.Start(fun inbox ->
        let mutable remain = count
        let rec loop files count =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSloader ch ->
                    printfn "Matrix loader is ready to finish!"
                    balancer.PostAndReply (fun ch -> EOSbalancer ch)
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        if remain = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOSloader ch)
                            return! loop files 0
                        elif remain > 0 then
                            printfn "There are no more matrices to read. Matrix reading is finished. %A pairs read" (count - remain)
                            inbox.Post (EOSloader ch)
                            return! loop files 0
                        else failwith "Something went totally wrong"
                    | file1 :: file2 :: files ->
                        if remain = 0 then
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOSloader ch)
                            return! loop files 0
                        else
                            printfn "Loading: %A and %A" file1 file2
                            let cmToSend = mbMatrConf( (toCompMatr file1), (toCompMatr file2), file1, file2)
                            remain <- remain - 1
                            balancer.Post (СompMatrs cmToSend)
                            inbox.Post (Go ch)
                            return! loop files count
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
                | EOSbalancer ch ->
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
                        let qtToSend = Qt (mbMatrConf<int>.confCmToQt cmToSend)
                        let ar2dToSend = (Ar2d (mbMatrConf<int>.confCmto2d cmToSend))
                        match mbMatrConf<int>.sizeSparsityCheck cmToSend.m1 cmToSend.m2 with
                            | true, true -> qtParMult.Post qtToSend
                            | true, false -> arParMult.Post ar2dToSend
                            | false, true -> qtSeqMult.Post qtToSend
                            | false, false -> arSeqMult.Post ar2dToSend
                        return! loop ()
                    else
                        printfn "Processing %A and %A" cmToSend.fname1 cmToSend.fname2
                        printfn "Matrices of these sizes cannot be multiplied"
                        return! loop ()
            }
        loop ()
    )

let multiplier multfun =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSmult ch ->
                    printfn "%A is finished!" multfun
                    ch.Reply()
                | Qt qtToSend ->
                    printfn "Multing: %A, %A using %A" qtToSend.fname1 qtToSend.fname2 multfun
                    let res =
                        if multfun = "qtSeqMult" then multInner qtToSend.m1 qtToSend.m2 standSemiring
                        else parMultQuadTree qtToSend.m1 qtToSend.m2  standSemiring 1
                    return! loop ()
                | Ar2d ar2dToSend ->
                    printfn "Multing: %A, %A using %A" ar2dToSend.fname1 ar2dToSend.fname2 multfun
                    let res =
                        if multfun = "arSeqMult" then Ht3.matrixMult ar2dToSend.m1 ar2dToSend.m2
                        else parMatrixMult ar2dToSend.m1 ar2dToSend.m2
                    return! loop ()
            }
        loop ()
        )

let arSeqMult = multiplier "arSeqMult"
let arParMult = multiplier "arParMult"
let qtSeqMult = multiplier "qtSeqMult"
let qtParMult = multiplier "qtParMult"

let processSomeFilesAsync inputPath amount =
    let balancer = balancer qtSeqMult qtParMult arSeqMult arParMult
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply(fun ch -> Go ch)

let processAllFilesAsync inputPath =
    let amount = (listAllFiles inputPath).Length/2
    let balancer = balancer qtSeqMult qtParMult arSeqMult arParMult
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply(fun ch -> Go ch)



