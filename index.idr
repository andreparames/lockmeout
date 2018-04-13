module Lockmeout

import IdrisScript
import IdrisScript.Objects

import dynamo

record Secret where
    constructor MkSecret
    token: String

export
LambdaCallback : Type
LambdaCallback = String -> JS_IO()

export
CallbackType : Type
CallbackType = String

export
Event : Type
Event = JSValue (JSObject "Object")

getName : (JSValue (JSObject c)) -> JS_IO String
getName event = case !(getProperty "name" event) of
                     Just (JSString ** res) => pure (fromJS res)
                     _               => pure "N/A"

export
MkCallback : CallbackType -> LambdaCallback
MkCallback ref = \x => jscall callingcode (CallbackType -> String -> JS_IO ()) ref x
                 where
                   callingcode : String
                   callingcode = """%0(null, %1)"""

ToEvent : JSRef -> Event
ToEvent = \x => MkJSObject x

MkToken : (len : Nat) -> JS_IO (String)
MkToken len = jscall "require('crypto').randomBytes(%0).toString('hex')" (Int -> JS_IO (String)) (toIntNat len)

export
handler : JSRef -> JSRef -> String -> JS_IO ()
handler event context callfunc = do name <- getName (ToEvent (event))
                                    case !(GetDynamoDB US_EAST_2) of
                                      Just db => do token <- MkToken 32
                                                    let items = [(MkDBItem "token" token), (MkDBItem "name" name)]
                                                    putItem db "lockitems" items
                                                    callback ("The name is " ++ name)
                                      Nothing => callback ("Err")
                                 where
                                    callback : LambdaCallback
                                    callback = MkCallback callfunc

exports : FFI_Export FFI_JS "" []
exports = Data Event "Event" $
          Fun handler "handler" $
          End

