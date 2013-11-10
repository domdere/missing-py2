{- arch-tag: Interpreter tests main file
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Interpretertest(tests) where

import Testutil (testMedInt)

import Test.HUnit
import Python.Interpreter
import Foreign.C.Types
import Python.Objects

testBase :: [Test]
testBase =
    let f msg t = TestLabel msg $ TestCase t in
    [   f "longs" $ do
            pyo <- toPyObject (10::CLong)
            newl <- fromPyObject pyo
            (10::CLong) @=? newl
    ,   f "print" $ pyRun_SimpleString "print \"Hi from Python\\n\""
    ]

testCallByName :: [Test]
testCallByName =
    let
        f msg func inp expression = TestLabel msg $ TestCase $ do
            r <-callByNameHs func inp noKwParms
            expression @=? r
    in
        [   f "repr" "repr" [5::Integer] "5L"
        ,   f "repr2" "repr" [5::CLong] "5"
        ,   f "pow" "pow" [2::CInt, 32::CInt] testMedInt

        ,   TestLabel "import" $ TestCase $ do
                pyImport "base64"
                r <- callByNameHs "base64.encodestring" ["hi"] noKwParms
                "aGk=\n" @=? r
        ]

testArgs :: [Test]
testArgs =
    let
        f msg code inp expression = TestLabel msg $ TestCase $ do
            let testhdict = [("testval", inp)]
            retobj <- pyRun_StringHs code Py_eval_input testhdict
            expression @=? retobj
    in
        [   f "addition" "testval + 3" (2::CLong) (5::CLong)
{-
        ,   TestLabel "m1" $ TestCase $
                   do testpydict <- toPyObject [(5::CInt, 2::CInt)]
                      retobj <- pyRun_String "testval + 3" 0 Nothing (Just testpydict)
                      retval <- fromPyObject retobj
                      5 @=? retval
-}
        ]


tests :: Test
tests = TestList    [   TestLabel "base" (TestList testBase)
                    ,   TestLabel "args" (TestList testArgs)
                    ,   TestLabel "callByName" (TestList testCallByName)
                    ]
