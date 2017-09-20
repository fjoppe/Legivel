module YamlParser.Utilities.SpookyHash

open System

/// Hash byte array into a 128 bit hash
val Hash128 : message : byte array -> hash1 : UInt64 -> hash2 : UInt64 -> uint64 * uint64

/// Hash byte array into a 64 bit hash
val Hash64 : message : byte array -> seed : UInt64 -> uint64 

/// Hash byte array into a 32 bit hash
val Hash32 : message : byte array -> seed : UInt32 -> uint32 

