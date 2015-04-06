module Markup where

import Lucid (Html, toHtml)
import Lucid.Html5

homepage :: Html ()
homepage = withHeader $
  form_ [method_ "POST", action_ "/upload", enctype_ "multipart/form-data"] $ do
    input_ [type_ "file", name_ "upload", accept_ "image/png"]
    input_ [type_ "submit"]

uploadNotFound :: Html ()
uploadNotFound = withHeader "uploaded file not found"

withHeader :: Html () -> Html ()
withHeader x = do
  doctype_
  head_ $
    title_ "crusher"
  body_ $ do
    a_ [href_ "/"] $ h1_ "crusher"
    x

error :: String -> Html ()
error err = withHeader $ do
  "an error occurred:"
  pre_ $
    toHtml err
