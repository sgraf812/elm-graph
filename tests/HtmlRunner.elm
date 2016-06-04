module Main exposing (..)

import Html exposing (Html)
import Element exposing (Element, leftAligned)
import ElmTest exposing (stringRunner)
import Text exposing (fromString)
import Test

main : Html Element
main =
  Element.toHtml (leftAligned (fromString (stringRunner Test.tests)))
