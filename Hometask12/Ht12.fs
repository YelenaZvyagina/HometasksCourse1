module Ht12

open System
open Quadtree
open System.Threading.Tasks
open AlgStcruct

let printStringAr (m:string [,]) path =
    for i = 0 to m.GetLength 0 - 1 do
        let mutable str = ""
        for j = 0 to m.GetLength 1 - 1 do
            str <- str + m.[i, j]
            if j <> (m.GetLength 1 - 1) then str <- str + " "
        str <- str + "\n"
        System.IO.File.AppendAllText(path, str)

let genRandMatr<'t> rows cols n (zeroPercent: float) (path: string) =
    let rand = new System.Random()
    for i = 0 to n-1 do
        let output = Array2D.zeroCreate rows cols
        for j = 0 to rows-1 do
            for k = 0 to cols-1 do
                let a = (new Random()).NextDouble() 
                if typeof<'t> = typeof<int32> then
                    if a > zeroPercent   
                    then output.[j, k] <- string (rand.Next(1, 50))
                    else output.[j, k] <- "0"
                elif typeof<'t> = typeof<float> then
                    if a > zeroPercent   
                    then output.[j, k] <- string ((new System.Random()).NextDouble() + double (rand.Next(1, 50))) 
                    else output.[j, k] <- "0"
                elif typeof<'t> = typeof<bool> then
                    if a > zeroPercent  
                    then output.[j, k] <- "1" 
                    else output.[j, k] <- "0"
        printStringAr output (path.[0..path.Length - 5] + string i + path.[path.Length - 4..path.Length - 1])

