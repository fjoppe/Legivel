
exception ParseException of string

type Plain =
    private {
        ``fixed`` : string
    }
    override this.ToString() =
        sprintf "(%s)" this.``fixed``
    static member (+) (r1:Plain, r2:Plain) =
        {``fixed`` = r1.``fixed`` + r2.``fixed``}
    static member Create r =
        {``fixed`` = r}

type OneInSet =
    private {
        mainset  : string
        subtractset : string
    }
    override this.ToString() =
        if this.subtractset <> "" then
            sprintf "[%s-[%s]]" (this.mainset) (this.subtractset)
        else
            sprintf "[%s]" (this.mainset)
    static member (-) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.mainset}
    static member (+) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset + r2.mainset; subtractset = r1.subtractset}
    static member (+) (r1:OneInSet, r2:Plain) =
        {mainset = r1.mainset + r2.``fixed``; subtractset = r1.subtractset}
    static member Create r =
         {mainset= r; subtractset = ""}

type RGXType =
    |   Plain of Plain
    |   OneInSet of OneInSet
    |   Or       of RGXType list
    override this.ToString() =
        match this with
        |   Plain    r -> r.ToString()
        |   OneInSet r ->r.ToString()
        |   Or       l ->
                let l = l |> List.rev
                l.Tail |> List.fold(fun s e -> sprintf "%s|%O" s e) (sprintf "%O" l.Head)
    static member (|||) (r1:RGXType, r2:RGXType) =
        match r1 with
        | Or    l ->    Or(r2 :: l)
        | _       ->    Or([r2; r1])

    static member (-) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (OneInSet o1, OneInSet o2)  -> OneInSet(o1 - o2)
        |   _   -> raise (ParseException "These cannot be subtracted")

    static member (+) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (Plain p1   , Plain p2)     -> Plain(p1 + p2)
        |   (OneInSet o1, OneInSet o2)  -> OneInSet(o1 + o2)
        |   (OneInSet o1,    Plain p1)  -> OneInSet(o1 + p1)
        |   _   -> raise (ParseException "These cannot be concatenated")


let RGP c = Plain(Plain.Create c)
let RGO c = OneInSet(OneInSet.Create c)

let y = RGP "yaml"
let s = RGO "a-zA-Z"
let u = RGO "bml"
let t = RGP "stoppy"

s + u
y + t

let c = y+t ||| s-u ||| t
let d = c ||| (RGP "yuk")

d.ToString()


