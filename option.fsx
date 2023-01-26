type OptionT<'a> =
    | NoneT
    | SomeT of 'a


let ret (x : 'a) : 'a OptionT = SomeT x

let bind (m : 'a OptionT) (f : 'a -> 'b OptionT) : 'b OptionT =
    match m with
    | NoneT -> NoneT
    | SomeT x -> f x

let (>>=) = bind

let sequence (munit : unit OptionT) (m : 'a OptionT) : 'a OptionT =
    munit >>= fun _ -> m

let ( *>) = sequence

type OptionBuilder() = // m = option
    member this.Return(x) = ret x // return :: a -> m a
    member this.ReturnFrom(m) = m // return! :: m a -> m a
    member this.Bind(m, f) = m >>= f // bind :: m a -> (a -> m b) -> m b
    member this.Combine(m1, m2) = m1 *> m2
    member this.Zero = NoneT
    member this.Delay(f) = f()

let Option = new OptionBuilder()

// Utilities
let p x = printfn "%A" x

// Usage of Option

// return
p "return"
let r (x: 'a) : 'a OptionT = Option { return x }
r 4 |> p
r 1 |> p
r "big monads" |> p
let n : float OptionT OptionT = r NoneT
n |> p
//n.GetType() |> p

// return!
p "return!"
let rbang (x: 'a OptionT) : 'a OptionT = Option { return! x }
SomeT "bigger monads" |> rbang |> p
SomeT 2 |> rbang |> p
NoneT |> rbang |> p

// bind, (>>=), let!
p "(>>=) bind"
let rbind (x: 'a OptionT) = Option {
    p x
    let! y = x
    p y
    let z = r y
    p z
    let! z2 = z
    p z2
    return z2
}
SomeT "biggest monads" |> rbind |> p


let rbind3 (x: 'a OptionT) = Option { return! (x >>= r) }
SomeT "bind me" |> rbind3 |> p

let rbind4 (x: 'a OptionT) = x >>= r >>= r
SomeT "bind it" |> rbind3 |> p
SomeT 1 |> rbind3 |> p

// combine, *>, { ce1; ce2 }
let c : int OptionT = Option {
    return! SomeT ()
    return! NoneT
}
p c

let c2 = Option {
    return! SomeT ()
    return! SomeT 3
}
p c2



