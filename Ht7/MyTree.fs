namespace Ht7
module MyTree =

    open MyList

    type MyTree<'t> =
        | Leaf of 't
        | Node of 't * MyList.MyList<MyTree<'t>>

    let rec fold f acc tree =
        match tree with
        | Leaf x -> f acc x
        | Node (h, t) -> MyList.fold (fun acc t -> fold f acc t) (f acc h) t

    let maxInMyTree tr =
        fold (fun x y -> if x >= y then x else y) System.Int32.MinValue tr

    let averInMyTree tr =
        let (sum, kol) = fold (fun (sum, kol) elem -> (elem + sum, kol + 1)) (0, 0) tr
        sum/kol
