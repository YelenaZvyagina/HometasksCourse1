namespace Ht7
module MyString =

    open MyList

    type MyString = MyList.MyList<char>

    let concat (s1:MyString) (s2:MyString) =
        MyList.concat s1 s2

    let stringToMystring (s:string) =
        MyList.sysListToMyList (Seq.toList s)
