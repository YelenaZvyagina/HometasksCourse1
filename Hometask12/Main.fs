
open System
open Ht12
open Quadtree
open AlgStcruct

[<EntryPoint>]
let main argv =

    Console.WriteLine("Enter the sizes of matrices")
    let rows = int (Console.ReadLine())
    let cols = int (Console.ReadLine())
    Console.WriteLine("Enter the type of martrices (int float or bool) ")
    let typ = Console.ReadLine()
    Console.WriteLine("Enter the path to a folder to store")
    let path = Console.ReadLine()
    Console.WriteLine("Enter the percent of zeros im the matrix (from 0 to 1)")
    let zeroPercent = float (Console.ReadLine())
    Console.WriteLine("Enter the number of matrices")
    let n = int (Console.ReadLine())
    match typ with
    | "int" -> genRandMatr<int> rows cols n zeroPercent path
    | "float" -> genRandMatr<float> rows cols n zeroPercent path
    | "bool" -> genRandMatr<bool> rows cols n zeroPercent path
    | _ -> failwith "this type is not supported, sorry"   
    


    0 
