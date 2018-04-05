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

MkToken : JS_IO String
MkToken = pure "HHEHEHE"

putItem : (db : JSValue (JSObject _)) -> (table : String) -> (name : String) -> JS_IO ()
putItem db table name = do
    item <- IdrisScript.Objects.empty
    token <- MkToken
    setProperty "token" (toJS {to=JSString} token) item
    setProperty "name" (toJS {to=JSString} name) item 
    params <- IdrisScript.Objects.empty
    setProperty "TableName" (toJS {to=JSString} table) params
    setProperty "Item" item params
    res <- jscall "%0.put(%1, function(err) { console.log(err); })" (Ptr -> Ptr -> JS_IO ()) (unpack db) (unpack params)
    pure ()

export
putItemInDB : String -> JS_IO ()
putItemInDB name = do
  aws <- MkAWS
  maybe_cfg <- getConfig aws
  case maybe_cfg of
      Just cfg => do AWSConfigSet cfg "region" "us-west-2"
                     dynamo <- DynamoDB aws
                     args <- with Arrays empty
                     mydb <- new dynamo args
                     case mydb of
                       (c ** k) => putItem k "lockeditems" name
                       _        => log (toJS {to=JSString} "Nope")
      Nothing  => pure ()
