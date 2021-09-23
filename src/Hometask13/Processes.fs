module Hometask13.Ht13

open System
open Hometask13
open Quadtree
open AlgStcruct
open MyTask
open helpingFunctions
open Configs

type ldMsg =
    | Go of AsyncReplyChannel<unit>
    | EOS of AsyncReplyChannel<unit>

type blcMltMsg =
    | EOS of AsyncReplyChannel<unit>
    | СompMatrs of mbMatrConf<CompMatrix>

let matrixLoader inputPath (balancer:MailboxProcessor<blcMltMsg>) count =
    MailboxProcessor.Start(fun inbox ->
        let rec loop files count =
            async{
                let! msg = inbox.Receive()
                match msg with
                | ldMsg.EOS ch ->
                    printfn "Matrix loader is ready to finish!"
                    balancer.PostAndReply blcMltMsg.EOS
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        if count = 0
                        then
                            printfn "Matrix reading is finished!"
                            inbox.Post (ldMsg.EOS ch)
                            return! loop files 0
                        elif count > 0
                        then
                            printfn "There are no more matrices to read. Matrix reading is finished. %A pairs read" count
                            inbox.Post (ldMsg.EOS ch)
                            return! loop files 0
                        else failwith "Something went totally wrong"
                    | file1 :: file2 :: files ->
                        if count = 0
                        then
                            printfn "Matrix reading is finished!"
                            inbox.Post (ldMsg.EOS ch)
                            return! loop files 0
                        else
                            printfn "Loading: %A and %A" file1 file2
                            let cmToSend = mbMatrConf( (toCompMatr file1), (toCompMatr file2), file1, file2)
                            balancer.Post (СompMatrs cmToSend)
                            inbox.Post (Go ch)
                            return! loop files (count-1)
                    | _ -> failwith "Total amount of matrices should be even"
            }
        loop (listAllFiles inputPath) count
    )

let balancer (qtSeqMult : MailboxProcessor<blcMltMsg>) (qtParMult : MailboxProcessor<blcMltMsg>) (arSeqMult : MailboxProcessor<blcMltMsg>) (arParMult: MailboxProcessor<blcMltMsg>) (cfg : configs<_>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | blcMltMsg.EOS ch ->
                    printfn "Matrix balancer is ready to finish!"
                    qtSeqMult.PostAndReply blcMltMsg.EOS
                    arSeqMult.PostAndReply blcMltMsg.EOS
                    qtParMult.PostAndReply blcMltMsg.EOS
                    arParMult.PostAndReply blcMltMsg.EOS
                    printfn "Balancer is finished!"
                    ch.Reply()
                | СompMatrs cmToSend ->
                    if cmToSend.m1.lines = cmToSend.m2.columns
                    then
                        printfn "Processing %A and %A" cmToSend.fname1 cmToSend.fname2
                        match mbMatrConf<int>.sizeSparsityCheck cmToSend.m1 cmToSend.m2 cfg with
                            | true, true -> qtParMult.Post (СompMatrs cmToSend)
                            | true, false -> arParMult.Post (СompMatrs cmToSend)
                            | false, true -> qtSeqMult.Post (СompMatrs cmToSend)
                            | false, false -> arSeqMult.Post (СompMatrs cmToSend)
                        return! loop ()
                    else
                        printfn "Processing %A and %A" cmToSend.fname1 cmToSend.fname2
                        printfn "Matrices of these sizes cannot be multiplied"
                        return! loop ()
            }
        loop ()
    )

let arMulter multfun =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | blcMltMsg.EOS ch ->
                    printfn "Array multing is finished!"
                    ch.Reply()
                    return! loop ()
                | СompMatrs cmToSend ->
                    printfn "Multing: %A, %A" cmToSend.fname1 cmToSend.fname2
                    let res = multfun (cmatrTo2d cmToSend.m1) (cmatrTo2d cmToSend.m2)
                    return! loop ()
            }
        loop ()
    )

let qtMulter multfun (cfg : configs<_>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | blcMltMsg.EOS ch ->
                    printfn "Quadtree multing is finished!"
                    ch.Reply()
                    return! loop ()
                | СompMatrs cmToSend ->
                    printfn "Multing: %A, %A" cmToSend.fname1 cmToSend.fname2
                    let res = multfun (cmatrToQt cmToSend.m1) (cmatrToQt cmToSend.m2) cfg.astr cfg.parLim
                    return! loop ()
            }
        loop ()
    )

let qtSeqMb qt1 qt2 (astr : AlStruct<_>) lim = multInner qt1 qt2 astr

let processFilesAsync inputPath amount =
    let balancer = balancer (qtMulter qtSeqMb mainCfg) (qtMulter parMultQuadTree mainCfg) (arMulter Ht3.matrixMult) (arMulter parMatrixMult) mainCfg
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply Go

let processSomeFilesAsync inputPath amount = processFilesAsync inputPath amount
let processAllFilesAsync inputPath = processFilesAsync inputPath ((listAllFiles inputPath).Length/2)





