module Dynamo

import IdrisScript
import IdrisScript.Arrays
import IdrisScript.Objects

AWSLib : Type
AWSLib = JSValue (JSObject "Object")

AWSLibIO : Type
AWSLibIO = JS_IO (AWSLib)


MkAWS : AWSLibIO
MkAWS = do
  lib <- jscall "require('aws-sdk')" (JS_IO Ptr)
  pure $ MkJSObject lib

getConfig : AWSLib -> JS_IO (Maybe (JSValue (JSObject "Config")))
getConfig lib = 
    case !(getProperty "config" lib) of
        Just ((JSObject "Config") ** config) => pure $ Just config
        _                                    => pure Nothing

AWSConfigSet : (cfg : (JSValue (JSObject "Config"))) -> (key : String) -> (val : String) -> JS_IO (JSValue (JSObject "Config"))
AWSConfigSet config key val = setProperty key (toJS {to=JSString} val) config

DynamoDB : AWSLib -> JS_IO (JSValue JSFunction)
DynamoDB lib = do
  db <- jscall "%0.DynamoDB" (Ptr -> JS_IO Ptr) (unpack lib)
  pure $ MkJSFunction db

putItem : (db : DB) -> (table : String) -> (item : DBItem) -> JS_IO ()

namespace Main
  main : JS_IO ()
  main = do
    aws <- MkAWS
    maybe_cfg <- getConfig aws
    case maybe_cfg of
        Just cfg => do AWSConfigSet "region" "us-west-2" cfg
                       dynamo <- DynamoDB aws
                       args <- with Arrays empty
                       mydb <- new dynamo args
                       case mydb of
                         (c ** k) => log k
                         _        => log (toJS {to=JSString} "Nope")
        Nothing  => pure ()
