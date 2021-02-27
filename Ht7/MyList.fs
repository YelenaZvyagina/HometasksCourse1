namespace Ht7
module MyList =

    type MyList<'t> =
        | First of 't
        | Cons of 't * MyList<'t>

    let rec fold f acc l =
        match l with
        | First x -> f acc x
        | Cons(h, t) -> fold f (f acc h) t

    let length l =
        fold (fun acc _ -> acc + 1) 0 l

    let rec iter l f =
        match l with
        | First x -> f x
        | Cons(h, t) ->
            f h
            iter t f

    let rec sysListToMyList l =
        match l with
        | [] -> failwith "list shouldn't be empty"
        | [x] -> First x
        | h :: t -> Cons(h, sysListToMyList t)

    let rec myListToSystemList l =
        match l with
        | First x -> [x]
        | Cons(h, t) -> h :: myListToSystemList t

    let rec concat l1 l2 =
        match l1 with
        | First x -> Cons(x, l2)
        | Cons(h, t) -> Cons(h, concat t l2)

    let rec map f l =
       match l with
       | First x -> First(f x)
       | Cons(h, t) -> Cons(f h, map f t)

    let sort l =
        let rec _go lst =
            match lst with
            | First x -> First x
            | Cons(h, Cons(h1, t)) ->
                if h >= h1
                then Cons(h1, _go (Cons(h, t)))
                else Cons(h, _go (Cons(h1, t)))
            | Cons(h, First t) ->
                if h >= t
                then Cons(t, First h)
                else lst
        let rec go (ls:MyList<_>) n =
            if n <> length ls
            then go (_go ls) (n + 1)
            else ls
        go l 0
