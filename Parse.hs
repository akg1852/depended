module Parse where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Vector as V
import qualified Data.Aeson as JSON
import qualified Text.XML.Light as XML


-- json

type JObject = HM.HashMap T.Text JSON.Value

parseJSON :: LazyChar8.ByteString -> JSON.Value
parseJSON s = case JSON.decode s of
    Just x -> x
    _ -> error "invalid json"

jObject :: JSON.Value -> JObject
jObject v = case v of
    JSON.Object x -> x
    _ -> error "invalid json object"

jArray :: JSON.Value -> V.Vector JSON.Value
jArray v = case v of
    JSON.Array x -> x
    _ -> error "invalid json array"

jArrayOfObject :: JSON.Value -> V.Vector JObject
jArrayOfObject = fmap jObject . jArray

jString :: JSON.Value -> T.Text
jString v = case v of
    JSON.String x -> x
    _ -> error "invalid json string"

jLookup :: T.Text -> JObject -> Maybe JSON.Value
jLookup key jObject = HM.lookup key jObject


-- xml

parseXML :: LazyChar8.ByteString -> XML.Element
parseXML s = case XML.parseXMLDoc s of
    Just x -> x
    _ -> error "invalid xml"

qNameIs :: T.Text -> (XML.QName -> Bool)
qNameIs n q = let qn = T.pack (XML.qName q) in qn == n

xGetAttribute :: T.Text -> XML.Element -> Maybe T.Text
xGetAttribute name = fmap T.pack . XML.findAttrBy (qNameIs name)

xGetChild :: T.Text -> XML.Element -> Maybe T.Text
xGetChild name = fmap (T.pack . XML.strContent) . XML.filterChildName (qNameIs name)

xGetElements :: T.Text -> XML.Element -> [XML.Element]
xGetElements name = XML.filterElementsName (qNameIs name)
