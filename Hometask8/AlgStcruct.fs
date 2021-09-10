module AlgStcruct

type Monoid<'t> =
    val neutral : 't
    val plus : 't -> 't -> 't
    new (o, t) = {neutral = o; plus = t}

type Semiring<'t> =
    val times : 't -> 't -> 't
    val monoid: Monoid<'t>
    new (m, p) = {monoid = m; times = p}

type AlStruct<'t> =
    | Semiring of Semiring<'t>
    | Monoid of Monoid<'t>

let getSemiring (sem:Semiring<'t>) =
    sem.monoid.neutral, sem.monoid.plus, sem.times

let getMonoid (m:Monoid<'t>) =
    m.neutral, m.plus, m.plus

let getPars (a:AlStruct<'t>) =
    match a with
    | Semiring x -> getSemiring x
    | Monoid x -> getMonoid x

let standSemiring = Semiring(new Semiring<int>(new Monoid<int>(0, (+)), (*)))
