module Dynamo

import IdrisScript
import IdrisScript.Arrays
import IdrisScript.Objects

import Control.ST
import Control.ST.Random

import Data.Vect

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
  db <- jscall "new %0.DynamoDB.DocumentClient({apiVersion: '2012-08-10'})" (Ptr -> JS_IO Ptr) (unpack lib)
  pure $ MkJSFunction db

letters : Vect 4 Char
letters = with Vect ['a','b','c','d']

MkTokenL : Integer -> ST m Char []
MkTokenL init = do var <- new init
                   val <- rndSelect' var letters
                   delete var
                   pure val

MkToken : Integer -> String
MkToken init = concat $ map (\x => singleton $ runPure (MkTokenL (init+x))) [0..20]

putItem : (db : JSValue JSFunction) -> (table : String) -> (name : String) -> JS_IO ()
putItem db table name = do
    item <- IdrisScript.Objects.empty
    setProperty "token" (toJS {to=JSString} token) item
    setProperty "name" (toJS {to=JSString} name) item 
    params <- IdrisScript.Objects.empty
    setProperty "TableName" (toJS {to=JSString} table) params
    setProperty "Item" item params
    res <- jscall "%0.put(%1, function(err) { console.log(err); })" (Ptr -> Ptr -> JS_IO ()) (unpack db) (unpack params)
    pure ()
  where
      token : String
      token = MkToken 4949

export
putItemInDB : String -> JS_IO ()
putItemInDB name = do
  aws <- MkAWS
  maybe_cfg <- getConfig aws
  case maybe_cfg of
      Just cfg => do AWSConfigSet cfg "region" "us-east-2"
                     dynamo <- DynamoDB aws
                     putItem dynamo "lockeditems" name
      Nothing  => pure ()
