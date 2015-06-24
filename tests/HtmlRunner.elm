module Main where

import Graphics.Element exposing (Element)
import ElmTest.Runner.Element as Element
import Test

main : Element
main = Element.runDisplay Test.tests