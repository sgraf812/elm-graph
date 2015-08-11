module Main where

import Basics exposing (..)
import Signal exposing (..)

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console as Console
import ElmTest.Test exposing (..)
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

import Test

port requests : Signal Request
port requests =
  Run.run responses (Console.runDisplay Test.tests)

port responses : Signal Response
