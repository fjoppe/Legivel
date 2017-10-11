#I __SOURCE_DIRECTORY__ 

#load "YamlParserFuncs.fsx"

open Yaml


YamlParse "
#%RAML 1.0
title: New API
mediaType: [ application/json, application/xml ]
types:
  Person:
  Another:
/books:
  get:
  put:
  post:
  /{bookTitle}:
    get:
    put:
    delete:
    /author:
      get:
    /publisher:
      get:
"

