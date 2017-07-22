module SpookyHash

//  Based on Rob Jenkins code:
//  http://burtleburtle.net/bob/hash/spooky.html
//  http://burtleburtle.net/bob/c/SpookyV2.h
//  http://burtleburtle.net/bob/c/SpookyV2.cpp
//
//  Note that this code does not support partial hashing

open System

// number of uint64's in internal state
let sc_numVars = 12

// size of the internal state
let sc_blockSize = (sc_numVars * 8)

// size of buffer of unhashed data, in bytes
let sc_bufSize = (2*sc_blockSize)

//
// sc_const: a constant which:
//  * is not zero
//  * is odd
//  * is a not-very-regular mix of 1's and 0's
//  * does not need any other special mathematical properties
//
let sc_const = 16045690984833335023UL // 0xdeadbeefdeadbeef


let getUInt64 index bytes =
    let source = bytes |> Array.skip(index * 8) |> Array.take(8) 
    BitConverter.ToUInt64(source, 0)

let getUInt32 index bytes =
    let source = bytes |> Array.skip(index * 4) |> Array.take(4) 
    BitConverter.ToUInt32(source, 0)

let byteArrayToUInt64Array (bytes:byte array) =
    [0 .. ((bytes.Length-1)/8)]
    |> List.map(fun i -> getUInt64 i bytes)
    |> Array.ofList

//
// left rotate a 64-bit value by k bytes
//
let Rot64 (x:uint64) (k:int) =
    (x <<< k) ||| (x >>> (64 - k));

//
// This is used if the input is 96 bytes long or longer.
//
// The internal state is fully overwritten every 96 bytes.
// Every input bit appears to cause at least 128 bits of entropy
// before 96 other bytes are combined, when run forward or backward
//   For every input bit,
//   Two inputs differing in just that input bit
//   Where "differ" means xor or subtraction
//   And the base value is random
//   When run forward or backwards one Mix
// I tried 3 pairs of each; they all differed by at least 212 bits.
//
let  Mix (data:uint64 array) 
        ((s0:uint64), (s1:uint64), (s2:uint64), (s3:uint64),
         (s4:uint64), (s5:uint64), (s6:uint64), (s7:uint64),
         (s8:uint64), (s9:uint64), (s10:uint64), (s11:uint64)) =
      let s0 = s0 + data.[0]
      let s2 = s2 ^^^ s10
      let s11 = s11 ^^^ s0
      let s0 = Rot64 s0 11
      let s11 = s11 + s1

      let s1 = s1 + data.[1]
      let s3 = s3 ^^^ s11
      let s0 = s0 ^^^ s1
      let s1 = Rot64 s1 32
      let s0 = s0 + s2

      let s2 = s2 + data.[2]
      let s4 = s4 ^^^ s0
      let s1 = s1 ^^^ s2
      let s2 = Rot64 s2 43
      let s1 = s1 + s3

      let s3 = s3 + data.[3]
      let s5 = s5 ^^^ s1
      let s2 = s2 ^^^ s3
      let s3 = Rot64 s3 31
      let s2 = s2 + s4

      let s4 = s4 + data.[4]
      let s6 = s6 ^^^ s2
      let s3 = s3 ^^^ s4
      let s4 = Rot64 s4 17
      let s3 = s3 + s5

      let s5 = s5 + data.[5]
      let s7 = s7 ^^^ s3
      let s4 = s4 ^^^ s5
      let s5 = Rot64 s5 28
      let s4 = s4 + s6

      let s6 = s6 + data.[6]
      let s8 = s8 ^^^ s4
      let s5 = s5 ^^^ s6
      let s6 = Rot64 s6 39
      let s5 = s5 + s7

      let s7 = s7 + data.[7]
      let s9 = s9 ^^^ s5
      let s6 = s6 ^^^ s7
      let s7 = Rot64 s7 57
      let s6 = s6 + s8

      let s8 = s8 + data.[8]
      let s10 = s10 ^^^ s6
      let s7 = s7 ^^^ s8
      let s8 = Rot64 s8 55
      let s7 = s7 + s9

      let s9 = s9 + data.[9]
      let s11 = s11 ^^^ s7
      let s8 = s8 ^^^ s9
      let s9 = Rot64 s9 54
      let s8 = s8 + s10

      let  s10 = s10 + data.[10]
      let s0 = s0 ^^^ s8
      let s9 = s9 ^^^ s10
      let s10 = Rot64 s10 22
      let s9 = s9 + s11

      let s11 = s11 + data.[11]
      let s1 = s1 ^^^ s9
      let s10 = s10 ^^^ s11
      let s11 = Rot64 s11 46
      let s10 = s10 + s0

      (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11)

//
// Mix all 12 inputs together so that h0, h1 are a hash of them all.
//
// For two inputs differing in just the input bits
// Where "differ" means xor or subtraction
// And the base value is random, or a counting value starting at that bit
// The final result will have each bit of h0, h1 flip
// For every input bit,
// with probability 50 +- .3%
// For every pair of input bits,
// with probability 50 +- 3%
//
// This does not rely on the last Mix() call having already mixed some.
// Two iterations was almost good enough for a 64-bit result, but a
// 128-bit result is reported, so End() does three iterations.
//
let EndPartial
        ((h0:uint64), (h1:uint64), (h2:uint64), (h3:uint64),
         (h4:uint64), (h5:uint64), (h6:uint64), (h7:uint64),
         (h8:uint64), (h9:uint64), (h10:uint64), (h11:uint64)) =

        let h11 = h11 + h1
        let h2 =  h2 ^^^ h11
        let h1 = Rot64 h1 44

        let h0 = h0 + h2
        let h3 = h3 ^^^ h0
        let h2 = Rot64 h2 15

        let h1 = h1 + h3
        let h4 = h4 ^^^ h1
        let h3 = Rot64 h3 34

        let h2 = h2 + h4
        let h5 =  h5 ^^^ h2
        let h4 = Rot64 h4 21

        let h3 = h3 + h5
        let h6 = h6 ^^^ h3
        let h5 = Rot64 h5 38

        let h4 = h4 + h6
        let h7 = h7 ^^^ h4
        let h6 = Rot64 h6 33

        let h5 = h5 + h7
        let h8 = h8 ^^^ h5
        let h7 = Rot64 h7 10

        let h6 = h6 + h8
        let h9 = h9 ^^^ h6
        let h8 = Rot64 h8 13

        let h7 = h7 + h9
        let h10 = h10 ^^^ h7
        let h9 = Rot64 h9 38

        let h8 = h8 + h10
        let h11 = h11 ^^^ h8
        let h10 = Rot64 h10 53

        let h9 = h9 + h11
        let h0 = h0 ^^^ h9
        let h11 = Rot64 h11 42

        let h10 = h10 + h0
        let h1 = h1 ^^^ h10
        let h0 = Rot64 h0 54

        (h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)



let End (data:uint64 array)
        ((h0:uint64), (h1:uint64), (h2:uint64), (h3:uint64),
         (h4:uint64), (h5:uint64), (h6:uint64), (h7:uint64),
         (h8:uint64), (h9:uint64), (h10:uint64), (h11:uint64)) =

    let h0 = h0 + data.[0]
    let h1 = h1 + data.[1]
    let h2 = h2 + data.[2]
    let h3 = h3 + data.[3]
    let h4 = h4 + data.[4]
    let h5 = h5 + data.[5]
    let h6 = h6 + data.[6]   
    let h7 = h7 + data.[7]
    let h8 = h8 + data.[8]
    let h9 = h9 + data.[9]
    let h10 = h10 + data.[10]
    let h11 = h11 + data.[11]

    (h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)
    |>  EndPartial
    |>  EndPartial
    |>  EndPartial


//
// The goal is for each bit of the input to expand into 128 bits of 
//   apparent entropy before it is fully overwritten.
// n trials both set and cleared at least m bits of h0 h1 h2 h3
//   n: 2   m: 29
//   n: 3   m: 46
//   n: 4   m: 57
//   n: 5   m: 107
//   n: 6   m: 146
//   n: 7   m: 152
// when run forwards or backwards
// for all 1-bit and 2-bit diffs
// with diffs defined by either xor or subtraction
// with a base of all zeros plus a counter, or plus another bit, or random
//
let ShortMix ((h0:uint64), (h1:uint64), (h2:uint64), (h3:uint64)) =
        let h2 = Rot64 h2 50
        let h2 = h2 + h3
        let h0 = h0 ^^^ h2

        let h3 = Rot64 h3 52
        let h3 = h3 + h0
        let h1 = h1 ^^^ h3

        let h0 = Rot64 h0 30
        let h0 = h0 + h1
        let h2 = h2 ^^^ h0

        let h1 = Rot64 h1 41
        let h1 = h1 + h2
        let h3 = h3 ^^^ h1

        let h2 = Rot64 h2 54
        let h2 = h2 + h3
        let h0 = h0 ^^^ h2

        let h3 = Rot64 h3 48
        let h3 = h3 + h0
        let h1 = h1 ^^^ h3

        let h0 = Rot64 h0 38
        let h0 = h0 + h1
        let h2 = h2 ^^^ h0
        
        let h1 = Rot64 h1 37
        let h1 = h1 + h2
        let h3 = h3 ^^^ h1
        
        let h2 = Rot64 h2 62
        let h2 = h2 + h3
        let h0 = h0 ^^^ h2
        
        let h3 = Rot64 h3 34
        let h3 = h3 + h0
        let h1 = h1 ^^^ h3
        
        let h0 = Rot64 h0 5
        let h0 = h0 + h1
        let h2 = h2 ^^^ h0
        
        let h1 = Rot64 h1 36
        let h1 = h1 + h2
        let h3 = h3 ^^^ h1

        (h0, h1, h2, h3)


//
// Mix all 4 inputs together so that h0, h1 are a hash of them all.
//
// For two inputs differing in just the input bits
// Where "differ" means xor or subtraction
// And the base value is random, or a counting value starting at that bit
// The final result will have each bit of h0, h1 flip
// For every input bit,
// with probability 50 +- .3% (it is probably better than that)
// For every pair of input bits,
// with probability 50 +- .75% (the worst case is approximately that)
//
let ShortEnd((h0:uint64), (h1:uint64), (h2:uint64), (h3:uint64)) =

    let h3 = h3 ^^^ h2
    let h2 = Rot64 h2 15
    let h3 = h3 + h2

    let h0 = h0 ^^^ h3
    let h3 = Rot64 h3 52
    let h0 = h0 + h3

    let h1 = h1 ^^^ h0
    let h0 = Rot64 h0 26
    let h1 = h1 + h0

    let h2 = h2 ^^^ h1
    let h1 = Rot64 h1 51
    let h2 = h2 + h1

    let h3 = h3 ^^^ h2
    let h2 = Rot64 h2 28
    let h3 = h3 + h2

    let h0 = h0 ^^^ h3
    let h3 = Rot64 h3 9
    let h0 = h0 + h3

    let h1 = h1 ^^^ h0
    let h0 = Rot64 h0 47
    let h1 = h1 + h0

    let h2 = h2 ^^^ h1
    let h1 = Rot64 h1 54
    let h2 = h2 + h1

    let h3 = h3 ^^^ h2
    let h2 = Rot64 h2 32
    let h3 = h3 + h2

    let h0 = h0 ^^^ h3
    let h3 = Rot64 h3 25
    let h0 = h0 + h3

    let h1 = h1 ^^^ h0
    let h0 = Rot64 h0 63
    let h1 = h1 + h0
   
    (h0, h1, h2, h3)


let Short (message:byte array) (hash1:UInt64) (hash2:UInt64) =
    let length = message.Length

    let rec mixitLowerThan16 ((a:uint64), (b:uint64), (c:uint64), (d:uint64)) length (remain:byte array) =
        match length with
        |   15  ->
            let d = d + (uint64(remain.[14]) <<< 48)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   14  ->
            let d = d + (uint64(remain.[13]) <<< 40)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   13  ->
            let d = d + (uint64(remain.[12]) <<< 32)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   12  ->
            let d = d + (uint64(remain |> getUInt32 2))
            let c = c + (remain |> getUInt64 0)
            (a,b,c,d)
        |   11  ->
            let d = d + (uint64(remain.[10]) <<< 16)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   10  ->
            let d = d + (uint64(remain.[9]) <<< 8)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   9  ->
            let d = d + (uint64(remain.[8]))
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   8  ->
            let c = c + (remain |> getUInt64 0)
            (a,b,c,d)
        |   7  ->
            let c = c + (uint64(remain.[6]) <<< 48)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   6  ->
            let c = c + (uint64(remain.[5]) <<< 40)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   5  ->
            let c = c + (uint64(remain.[4]) <<< 32)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   4  ->
            let c = c + (uint64(remain |> getUInt32 0))
            (a,b,c,d)
        |   3  ->
            let c = c + (uint64(remain.[2]) <<< 16)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   2  ->
            let c = c + (uint64(remain.[1]) <<< 8)
            mixitLowerThan16 (a,b,c,d) (length-1) (remain) // |> Array.skip 1)
        |   1  ->
            let c = c + (uint64(remain.[0]))
            (a,b,c,d)
        |   0  ->
            let c = c + sc_const
            let d = d + sc_const
            (a,b,c,d)
        |   _  -> failwith "Impossible error, righto. This loop is only for arrays with lengths lower than 16, and currently this isnot the case"

    let rec mixitAll ((a:uint64), (b:uint64), (c:uint64), (d:uint64)) (remain:byte array) =
        match remain.Length with
        |   len   when len >= 32 -> 
            let c = c + (remain |> getUInt64 0)
            let d = d + (remain |> getUInt64 1)
            let (a,b,c,d) = ShortMix(a,b,c,d)
            let a = a + (remain |> getUInt64 2)
            let b = b + (remain |> getUInt64 3)
            mixitAll (a,b,c,d) (remain |> Array.skip 32)
        |   len   when len >= 16 ->
            let c = c + (remain |> getUInt64 0)
            let d = d + (remain |> getUInt64 1)
            let (a,b,c,d) = ShortMix(a,b,c,d)
            mixitAll (a,b,c,d) (remain |> Array.skip 16)
        |   _ -> 
            let d = d + (uint64(length) <<< 56)
            mixitLowerThan16 (a,b,c,d) (remain.Length) (remain)

    let (a,b,c,d) = (hash1, hash2, sc_const, sc_const)
    let (a,b,c,d) = mixitAll (a,b,c,d) message
    let (a,b,_,_) = ShortEnd(a,b,c,d)
    (a,b)


// do the whole hash in one call
let Hash128 (message:byte array) (hash1:UInt64) (hash2:UInt64) =
    let length = message.Length

    if (length < sc_bufSize) then 
        Short message hash1 hash2
    else
        let h0,h3,h6,h9 = hash1,hash1,hash1,hash1
        let h1,h4,h7,h10= hash2,hash2,hash2,hash2
        let h2,h5,h8,h11= sc_const,sc_const,sc_const,sc_const

        let mainSize = int32(message.Length/sc_blockSize) * sc_numVars
    
        let mainData  = message |> Array.take (mainSize * 8)
        let remainder = message |> Array.skip (mainSize * 8)

        let mixValues = (h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)

        let rec mixLoop (curr:UInt64 array) acc =
            if curr.Length = 0 then acc
            else
                let mix = Mix curr acc
                mixLoop (curr |> Array.skip sc_numVars) mix

        let mixValues = mixLoop (byteArrayToUInt64Array mainData) mixValues

        let remainData = 
            Array.init sc_blockSize (fun i -> 
                if i < remainder.Length then remainder.[i] 
                else
                    if i = (sc_blockSize-1) then byte(remainder.Length)
                    else (byte 0)
            )
            |> byteArrayToUInt64Array

        let (h0, h1, _, _, _, _, _, _, _, _, _, _) = End remainData mixValues

        (h0, h1)

//
// Hash64: hash a single message in one call, return 64-bit output
//
let Hash64 (message:byte array) (seed:UInt64) =
    Hash128 message seed seed |> fst


//
// Hash32: hash a single message in one call, produce 32-bit output
//
let Hash32 (message:byte array) (seed:UInt32) =
    let hash1, hash2 = uint64(seed), uint64(seed)
    Hash128 message hash1 hash2 |> fst |> uint32

