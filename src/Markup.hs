module Markup where

import Lucid (Html, toHtml)
import Lucid.Html5

homepage :: Html ()
homepage = withHeader $
  form_ [method_ "POST", action_ "/upload", enctype_ "multipart/form-data"] $ do
    input_ [type_ "file", name_ "upload", accept_ "image/png"]
    br_ []
    input_ [type_ "submit", value_ "compress"]

uploadNotFound :: Html ()
uploadNotFound = withHeader "uploaded file not found"

withHeader :: Html () -> Html ()
withHeader x = do
  doctype_
  head_ $ do
    title_ "crusher"
    link_ [rel_ "stylesheet", href_ "/stylesheet.css", type_ "text/css"]
  body_ $
    div_ [id_ "main"] $ do
      a_ [href_ "/"] $ h1_ "crusher"
      x

error :: String -> Html ()
error err = withHeader $ do
  "an error occurred:"
  pre_ $
    toHtml err
