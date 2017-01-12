module SpookyHash

open System

val Hash128 : message : byte array -> hash1 : UInt64 -> hash2 : UInt64 -> uint64 * uint64

val Hash64 : message : byte array -> seed : UInt64 -> uint64 

val Hash32 : message : byte array -> seed : UInt32 -> uint32 

