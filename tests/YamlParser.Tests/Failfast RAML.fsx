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
/list:
  get:
    responses:
      200:
        body: Person[]
/send:
  post:
    body:
      application/json:
        type: Another
"

