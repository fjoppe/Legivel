﻿#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#time

//#r @"bin/Debug/Legivel.Common.dll"
//#r @"bin/Debug/Legivel.RepresentationGraph.dll"
//#r @"bin/Debug/Legivel.Parser.dll"

#r @"bin/Release/Legivel.Common.dll"
#r @"bin/Release/Legivel.RepresentationGraph.dll"
#r @"bin/Release/Legivel.Parser.dll"

#r @"test/NLog/lib/net45/NLog.dll"
open System
open System.Globalization
open Legivel.Parser
open Legivel.TagResolution
open Legivel.Serialization
open Legivel.RepresentationGraph
open Legivel.Common
open NLog


#load "nlog.fsx"

open System
open System.Globalization

NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser(Failsafe.Schema, fun s -> () (*logger.Trace(s)*))

let WarnMsg (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
let ErrMsg  (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column) (s.Message))
let TotLns (ps:DocumentLocation) = printfn "Total lines: %d" ps.Line

let PrintNode crr =
    match crr with
    |   NoRepresentation rr ->
        //printfn "Cannot parse: \"%s\"" rr.RestString
        rr.StopLocation |>  TotLns
        rr.Error |> ErrMsg
        rr.Warn |> WarnMsg
    |   PartialRepresentaton rr ->
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg
        printfn "%s" (SerializeToCanonical rr.Document (rr.TagShorthands))
    |   CompleteRepresentaton rr ->
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg
        printfn "%s" (SerializeToCanonical rr.Document (rr.TagShorthands))
    |   EmptyRepresentation rr ->
        printfn "Document was empty"
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg


let YamlParse s =
    try
        let repr = (engine.``l-yaml-stream`` s)
        let crr = repr.Head
        PrintNode crr
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseList s =
    try
        let repr = (engine.``l-yaml-stream`` s)
        printfn "Total Documents: %d" (repr.Length)
        repr |> List.iter(fun crr ->
            PrintNode crr
            printfn "..."
        )
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e




YamlParse "
swagger: '2.0'
schemes:
  - https
host: 6-dot-authentiqio.appspot.com
basePath: /
info:
  contact:
    email: hello@authentiq.com
    name: Authentiq team
    url: 'http://authentiq.io/support'
  description: 'Strong authentication, without the passwords.'
  license:
    name: Apache 2.0
    url: 'http://www.apache.org/licenses/LICENSE-2.0.html'
  termsOfService: 'http://authentiq.com/terms/'
  title: Authentiq
  version: '6'
  x-apisguru-categories:
    - security
  x-logo:
    backgroundColor: '#F26641'
    url: 'https://api.apis.guru/v2/cache/logo/https_www.authentiq.com_theme_images_authentiq-logo-a-inverse.svg'
  x-origin:
    - format: swagger
      url: 'https://raw.githubusercontent.com/AuthentiqID/authentiq-docs/master/docs/swagger/issuer.yaml'
      version: '2.0'
  x-preferred: true
  x-providerName: 6-dot-authentiqio.appspot.com
parameters:
  AuthentiqID:
    description: Authentiq ID to register
    in: body
    name: body
    required: true
    schema:
      $ref: '#/definitions/AuthentiqID'
  JobID:
    description: Job ID (20 chars)
    in: path
    name: job
    required: true
    type: string
  PK:
    description: Public Signing Key - Authentiq ID (43 chars)
    in: path
    name: PK
    required: true
    type: string
  PushToken:
    description: Push Token.
    in: body
    name: body
    required: true
    schema:
      $ref: '#/definitions/PushToken'
  Scope:
    description: Claims of scope
    in: body
    name: body
    required: true
    schema:
      $ref: '#/definitions/Claims'
responses:
  ErrorResponse:
    description: Error response
    schema:
      $ref: '#/definitions/Error'
paths:
  /key:
    delete:
      description: |
        Revoke an Authentiq ID using email & phone.

        If called with `email` and `phone` only, a verification code 
        will be sent by email. Do a second call adding `code` to 
        complete the revocation.
      operationId: key_revoke_nosecret
      parameters:
        - description: primary email associated to Key (ID)
          in: query
          name: email
          required: true
          type: string
        - description: 'primary phone number, international representation'
          in: query
          name: phone
          required: true
          type: string
        - description: verification code sent by email
          in: query
          name: code
          required: false
          type: string
      produces:
        - application/json
      responses:
        '200':
          description: Successfully deleted
          schema:
            properties:
              status:
                description: pending or done
                type: string
            type: object
        '401':
          description: Authentication error `auth-error`
          schema:
            $ref: '#/definitions/Error'
        '404':
          description: Unknown key `unknown-key`
          schema:
            $ref: '#/definitions/Error'
        '409':
          description: Confirm with code sent `confirm-first`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - delete
    post:
      consumes:
        - application/jwt
      description: |
        Register a new ID `JWT(sub, devtoken)`

        v5: `JWT(sub, pk, devtoken, ...)`

        See: https://github.com/skion/authentiq/wiki/JWT-Examples
      operationId: key_register
      parameters:
        - $ref: '#/parameters/AuthentiqID'
      produces:
        - application/json
      responses:
        '201':
          description: Successfully registered
          schema:
            properties:
              secret:
                description: revoke key
                type: string
              status:
                description: registered
                type: string
            type: object
        '409':
          description: Key already registered `duplicate-key`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - post
  '/key/{PK}':
    delete:
      description: Revoke an Identity (Key) with a revocation secret
      operationId: key_revoke
      parameters:
        - $ref: '#/parameters/PK'
        - description: revokation secret
          in: query
          name: secret
          required: true
          type: string
      produces:
        - application/json
      responses:
        '200':
          description: Successful response
          schema:
            properties:
              status:
                description: done
                type: string
            type: object
        '401':
          description: Key not found / wrong code `auth-error`
          schema:
            $ref: '#/definitions/Error'
        '404':
          description: Unknown key `unknown-key`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - delete
    get:
      description: |
        Get public details of an Authentiq ID.
      parameters:
        - $ref: '#/parameters/PK'
      produces:
        - application/json
      responses:
        '200':
          description: Successfully retrieved
          schema:
            properties:
              since:
                format: date-time
                type: string
              status:
                type: string
              sub:
                description: base64safe encoded public signing key
                type: string
            title: JWT
            type: object
        '404':
          description: Unknown key `unknown-key`
          schema:
            $ref: '#/definitions/Error'
        '410':
          description: Key is revoked (gone). `revoked-key`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - get
    head:
      description: |
        HEAD info on Authentiq ID
      parameters:
        - $ref: '#/parameters/PK'
      responses:
        '200':
          description: Key exists
        '404':
          description: Unknown key `unknown-key`
          schema:
            $ref: '#/definitions/Error'
        '410':
          description: Key is revoked `revoked-key`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - head
    post:
      consumes:
        - application/jwt
      description: |
        update properties of an Authentiq ID.
        (not operational in v4; use PUT for now)

        v5: POST issuer-signed email & phone scopes in
        a self-signed JWT

        See: https://github.com/skion/authentiq/wiki/JWT-Examples
      operationId: key_update
      parameters:
        - $ref: '#/parameters/PK'
        - $ref: '#/parameters/AuthentiqID'
      produces:
        - application/json
      responses:
        '200':
          description: Successfully updated
          schema:
            properties:
              status:
                description: confirmed
                type: string
            type: object
        '404':
          description: Unknown key `unknown-key`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - post
    put:
      consumes:
        - application/jwt
      description: |
        Update Authentiq ID by replacing the object.

        v4: `JWT(sub,email,phone)` to bind email/phone hash; 

        v5: POST issuer-signed email & phone scopes
        and PUT to update registration `JWT(sub, pk, devtoken, ...)`

        See: https://github.com/skion/authentiq/wiki/JWT-Examples
      operationId: key_bind
      parameters:
        - $ref: '#/parameters/PK'
        - $ref: '#/parameters/AuthentiqID'
      produces:
        - application/json
      responses:
        '200':
          description: Successfully updated
          schema:
            properties:
              status:
                description: confirmed
                type: string
            type: object
        '404':
          description: Unknown key `unknown-key`
          schema:
            $ref: '#/definitions/Error'
        '409':
          description: Already bound to another key `duplicate-hash`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - key
        - put
  /login:
    post:
      consumes:
        - application/jwt
      description: |
        push sign-in request
        See: https://github.com/skion/authentiq/wiki/JWT-Examples
      operationId: push_login_request
      parameters:
        - $ref: '#/parameters/PushToken'
        - description: URI App will connect to
          in: query
          name: callback
          required: true
          type: string
      produces:
        - application/json
      responses:
        '200':
          description: Successful response
          schema:
            properties:
              status:
                description: sent
                type: string
            type: object
        '401':
          description: Unauthorized for this callback audience `aud-error` or JWT should be self-signed `auth-error`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - login
        - post
  /scope:
    post:
      consumes:
        - application/jwt
      description: |
        scope verification request
        See: https://github.com/skion/authentiq/wiki/JWT-Examples
      operationId: sign_request
      parameters:
        - $ref: '#/parameters/Scope'
        - description: 'test only mode, using test issuer'
          in: query
          name: test
          required: false
          type: integer
      produces:
        - application/json
      responses:
        '201':
          description: Successful response
          schema:
            properties:
              job:
                description: 20-character ID
                type: string
              status:
                description: waiting
                type: string
            type: object
        '429':
          description: Too Many Requests on same address / number `rate-limit`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - scope
        - post
  '/scope/{job}':
    delete:
      description: delete a verification job
      operationId: sign_delete
      parameters:
        - $ref: '#/parameters/JobID'
      produces:
        - application/json
      responses:
        '200':
          description: Successfully deleted
          schema:
            properties:
              status:
                description: done
                type: string
            type: object
        '404':
          description: Job not found `unknown-job`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - scope
        - delete
    get:
      description: get the status / current content of a verification job
      operationId: sign_retrieve
      parameters:
        - $ref: '#/parameters/JobID'
      produces:
        - application/json
        - application/jwt
      responses:
        '200':
          description: Successful response (JWT)
          schema:
            properties:
              exp:
                type: integer
              field:
                type: string
              sub:
                description: base64safe encoded public signing key
                type: string
            title: JWT
            type: object
        '204':
          description: 'Confirmed, waiting for signing'
        '404':
          description: Job not found `unknown-job`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - scope
        - get
    head:
      description: HEAD to get the status of a verification job
      operationId: sign_retrieve_head
      parameters:
        - $ref: '#/parameters/JobID'
      produces:
        - application/json
      responses:
        '200':
          description: Confirmed and signed
        '204':
          description: 'Confirmed, waiting for signing'
        '404':
          description: Job not found `unknown-job`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - scope
        - head
    post:
      consumes:
        - application/json
      description: this is a scope confirmation
      operationId: sign_confirm
      parameters:
        - $ref: '#/parameters/JobID'
      produces:
        - application/json
      responses:
        '202':
          description: Successfully confirmed
          schema:
            properties:
              status:
                description: confirmed
                type: string
            type: object
        '401':
          description: Confirmation error `auth-error`
          schema:
            $ref: '#/definitions/Error'
        '404':
          description: Job not found `unknown-job`
          schema:
            $ref: '#/definitions/Error'
        '405':
          description: JWT POSTed to scope `not-supported`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - scope
        - post
    put:
      consumes:
        - application/jwt
      description: |
        authority updates a JWT with its signature
        See: https://github.com/skion/authentiq/wiki/JWT-Examples
      operationId: sign_update
      parameters:
        - $ref: '#/parameters/JobID'
      produces:
        - application/jwt
      responses:
        '200':
          description: Successfully updated
          schema:
            properties:
              jwt:
                description: result is JWT or JSON??
                type: string
              status:
                description: ready
                type: string
            type: object
        '404':
          description: Job not found `unknown-job`
          schema:
            $ref: '#/definitions/Error'
        '409':
          description: Job not confirmed yet `confirm-first`
          schema:
            $ref: '#/definitions/Error'
        default:
          $ref: '#/responses/ErrorResponse'
      tags:
        - scope
        - put
definitions:
  AuthentiqID:
    description: |
      Authentiq ID in JWT format, self-signed.
    properties:
      devtoken:
        description: device token for push messages
        type: string
      sub:
        description: UUID and public signing key
        type: string
    required:
      - sub
  Claims:
    description: |
      Claim in JWT format, self- or issuer-signed. 
    properties:
      email:
        description: ''
        type: string
      phone:
        description: ''
        type: string
      scope:
        description: claim scope
        type: string
      sub:
        description: UUID
        type: string
      type:
        description: ''
        type: string
    required:
      - sub
      - scope
  Error:
    properties:
      detail:
        type: string
      error:
        type: integer
      title:
        type: string
      type:
        description: unique uri for this error
        type: string
    required:
      - error
  PushToken:
    description: |
      PushToken in JWT format, self-signed. 
    properties:
      aud:
        description: audience (URI)
        type: string
      exp:
        type: integer
      iat:
        type: integer
      iss:
        description: issuer (URI)
        type: string
      nbf:
        type: integer
      sub:
        description: UUID and public signing key
        type: string
    required:
      - sub
      - iss
      - aud
"
