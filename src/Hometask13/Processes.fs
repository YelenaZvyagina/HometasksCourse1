module Processes

open System
open Hometask13
open Quadtree
open AlgStcruct
open MyTask
open Configs
open CompMatrices

type ldMsg =
    | Go of AsyncReplyChannel<unit>
    | EOS of AsyncReplyChannel<unit>

type blcMltMsg =
    | EOS of AsyncReplyChannel<unit>
    | СompMatrs of mbMatrConf<CompMatrix>

let matrixLoader inputPath (balancer:MailboxProcessor<blcMltMsg>) numOfPairs =
    MailboxProcessor.Start(fun inbox ->
        let rec loop files =
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
                        printfn "Matrix reading is finished!"
                        inbox.Post (ldMsg.EOS ch)
                        return! loop files
                    | file1 :: file2 :: files ->
                        printfn "Loading: %A and %A" file1 file2
                        let cmToSend = mbMatrConf( (toCompMatr file1), (toCompMatr file2), file1, file2)
                        balancer.Post (СompMatrs cmToSend)
                        inbox.Post (Go ch)
                        return! loop files
                    | _ -> failwith "Total amount of matrices should be even"
            }
        let fls = (listAllFiles inputPath).[.. (2*numOfPairs - 1)]
        loop fls
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
                        match sizeSparsityCheck cmToSend.m1 cmToSend.m2 cfg with
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

let multiplier multfun transfun (cfg : configs<_>)  =
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
                    let res = multfun (transfun cmToSend.m1) (transfun cmToSend.m2) cfg.astr cfg.parLim
                    return! loop ()
            }
        loop ()
    )

let qtSeqMb qt1 qt2 (astr : AlStruct<_>) lim = multInner qt1 qt2 astr
let arSeqMb ar1 ar2 (astr : AlStruct<_>) lim = Ht3.matrixMult ar1 ar2
let arParMult ar1 ar2 (astr : AlStruct<_>) lim= parMatrixMult ar1 ar2

let processFilesAsync inputPath amount =
    let balancer = balancer
                       (multiplier qtSeqMb cmatrToQt mainCfg)
                       (multiplier parMultQuadTree cmatrToQt mainCfg)
                       (multiplier arSeqMb cmatrTo2d mainCfg)
                       (multiplier arParMult cmatrTo2d mainCfg)
                       mainCfg
    let matrixLoader = matrixLoader inputPath balancer amount
    matrixLoader.PostAndReply Go






