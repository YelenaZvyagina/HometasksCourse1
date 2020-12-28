namespace Ht7Tests
module Ht7Tests = 

    open Expecto
    open Ht7
    
    [<Tests>]
    let propertyFoldTests =
        testList "Comparing MyList and System List fold functions"
            [
             let forFold (l:list<int>) f =
                if l.Length <> 0
                then
                    let h1 = MyList.fold f 10 (MyList.sysListToMyList l)
                    let h2 = List.fold f 10 l
                    Expect.equal h1 h2 "MyList fold and System fold should return the same list"

             testProperty  "Comparing MyList fold and System List fold for + " <| fun (l:list<int>) -> forFold l (+)
             testProperty  "Comparing MyList fold and System List fold for - " <| fun (l:list<int>) -> forFold l (-)
             testProperty  "Comparing MyList fold and System List fold for * " <| fun (l:list<int>) -> forFold l (*)
             testProperty "Comparing MyList fold and System List fold for / " <| fun (l:list<int>) ->
                let h0 = List.filter ((<>)0) l
                if h0.Length <> 0
                then
                    let h1 = MyList.fold (/) 10 (MyList.sysListToMyList h0)
                    let h2 = List.fold (/) 10 h0
                    Expect.equal h1 h2 "MyList fold and System fold should return the same list"
            ]

    [<Tests>]
    let propertyListTests =
        testList "Comparing MyList and System List other functions"
            [
             testProperty "Comparing MyList and System List concat" <| fun (l:list<int>) (l1:list<int>) ->
                if l.Length <> 0 && l1.Length <> 0
                then 
                    let h1 = MyList.concat (MyList.sysListToMyList l) (MyList.sysListToMyList l1)
                    let h2 = l @ l1
                    let h3 = MyList.myListToSystemList h1
                    Expect.equal h3 h2 "MyList fold and System concat should return the same list"

             testProperty "Comparing MyList and System List length" <| fun (l:list<int>) ->
                if l.Length <> 0
                then 
                    let h1 = MyList.length (MyList.sysListToMyList l) 
                    let h2 = l.Length
                    Expect.equal h1 h2 "MyList fold and System length should return the same value"
          
             testProperty "Comparing MyList and System List sort" <| fun (l:list<int>) ->
                if l.Length <> 0  
                then 
                    let h1 = MyList.sort (MyList.sysListToMyList l) 
                    let h2 = List.sort l
                    let h3 = MyList.myListToSystemList h1
                    Expect.equal h2 h3 "MyList fold and System sort should return the same list"

             testProperty "Comparing MyList and System List map" <| fun (l:list<_>) ->
                if l.Length <> 0 
                then
                    let f x = x + 1
                    let h1 = MyList.map f (MyList.sysListToMyList l)
                    let h2 = List.map f l 
                    let h3 = MyList.myListToSystemList h1
                    Expect.equal h2 h3 "MyList fold and System map should return the same list"

             testProperty "Comparing MyList and System List iter" <| fun (l:list<_>) ->
                if l.Length <> 0
                then
                    let l1 = []
                    let l2 = []
                    let f x (l:list<_>) =
                        (fun x -> l @ [x + 5])
                        ()
                    List.iter (f l1) l
                    MyList.iter (MyList.sysListToMyList l) (f l2)
                    Expect.sequenceEqual l1 l2 "MyList fold and System iter should return the same"
            ]

    [<Tests>]
    let MyStringTests =
        testList "Tests for MyString functions"
            [
             testCase "MyString concat" <| fun _ ->
                let s = MyList.Cons('a', MyList.Cons('b', MyList.First 'c'))
                let s1 = MyString.concat (MyList.Cons('a', MyList.First 'b')) (MyList.First 'c')
                Expect.equal s s1 "Results should be same"

             testCase "System string to MyString" <| fun _ ->
                let s = MyList.Cons('a', MyList.Cons('b', MyList.First 'c'))
                let s1 = MyString.stringToMystring("abc")
                Expect.equal s s1 "Results should be same"
            ]

    [<Tests>]
    let MyTreeTests =
        testList "Tests for MyTree Functions"
            [
             testCase "averInMyTree test" <| fun _ ->
                let t = (MyTree.Node ((3), MyList.Cons ((MyTree.Leaf (33)), MyList.First (MyTree.Leaf (3)))))
                Expect.equal (MyTree.averInMyTree t) 13 "average of 3, 33 and 3 is 13"

             testCase "maxInMyTree test" <| fun _ ->
                 let t = (MyTree.Node ((3), MyList.Cons ((MyTree.Leaf (33)), MyList.First (MyTree.Leaf (3)))))
                 Expect.equal (MyTree.maxInMyTree t) 33 "maximum of 3, 13, 33 is 33"
            ]
