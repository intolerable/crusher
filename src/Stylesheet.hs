module Stylesheet
  ( style ) where

import Stitch
import Stitch.Combinators
import Data.Text (Text)

type Color = Text

midnight :: Color
midnight = "rgba(44, 62, 80, 1)"

cloud :: Color
cloud = "rgba(236, 240, 241, 1)"

amethyst :: Color
amethyst = "rgba(155, 89, 182, 1)"

nephritis :: Color
nephritis = "rgba(39, 174, 96, 1)"

white :: Color
white = "white"

style :: CSS
style = do
  cssImport "url(http://fonts.googleapis.com/css?family=Lato:400,700)"
  reset
  "html" ? "font-size" .= "62.5%"
  "body" ? do
    "h1" ? do
      "font" -: do
        "size" .= "1.5rem"
        "weight" .= "bold"
      "margin-bottom" .= "0.5rem"
      "padding" .= "0.5rem"
      "display" .= "inline-block"
      "background-color" .= amethyst
      "color" .= white
    "font-family" .= "Lato, Helvetica, sans"
    "background-color" .= cloud
    "color" .= midnight
  "a" ? do
    "color" .= midnight
    "text-decoration" .= "none"
    "&:hover" ? "text-decoration" .= "underline"
  "#main" ? do
    "max-width" .= "40rem"
    "margin" .= "0 auto"
    "padding" .= "1rem"
    "font" .= "inherit"
  "input" ? do
    "font" .= "inherit"
    "border" .= "none"
    "&[type=submit]" ? do
      "background-color" .= nephritis
      "cursor" .= "pointer"
      "color" .= white
      "margin" .= "0.25rem"
    "&[type=file]" ? "margin" .= "0.25rem"

reset :: CSS
reset = do
  "html, body, div, span, applet, object, iframe,\
    \ h1, h2, h3, h4, h5, h6, p, blockquote, pre,\
    \ a, abbr, acronym, address, big, cite, code,\
    \ del, dfn, em, img, ins, kbd, q, s, samp,\
    \ small, strike, strong, sub, sup, tt, var,\
    \ b, u, i, center, dl, dt, dd, ol, ul, li,\
    \ fieldset, form, label, legend,\
    \ table, caption, tbody, tfoot, thead, tr, th, td,\
    \ article, aside, canvas, details, embed,\
    \ figure, figcaption, footer, header, hgroup,\
    \ menu, nav, output, ruby, section, summary,\
    \ time, mark, audio, video" ? do
    comment "http://meyerweb.com/eric/tools/css/reset/\n\
            \   v2.0 | 20110126\n\
            \   License: none (public domain)"
    "margin" .= "0"
    "padding" .= "0"
    "border" .= "none"
    "font" -: do
      "weight" .= "inherit"
      "size" .= "100%"
    "vertical-align" .= "baseline"
  "footer, header, hgroup, menu, nav, section" ? do
    comment "HTML5 display-role reset for older browsers"
    "display" .= "block"
  "body" ?
    "line-height" .= "1"
  "ol, ul" ?
    "list-style" .= "none"
  "blockquote, q" ?
    "quotes" .= "none"
  "blockquote:before, blockquote:after, q:before, q:after" ? do
    "content" .= "\"\""
    "content" .= "none"
  "table" ?
    "border" -: do
      "collapse" .= "collapse"
      "spacing" .= "0"
