//#I __SOURCE_DIRECTORY__ 
//#I "../../packages"
//
//#r @"bin/Debug/YamlParser.dll"
//#r @"NLog/lib/net45/NLog.dll"
//
//open RepresentationGraph
//open TagResolution
//
//let tri = TagResolutionInfo.Create "?" [] (Node.ScalarNode(NodeData.Create (TagKind.NonSpecific ))) NodeKind.Scalar
//
//JSONSchema.TagResolution tri
