{- arch-tag: Object tests main file
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

module Objectstest(tests) where

import Testutil (   testBigInt
                ,   testBigDouble
                )

import Test.HUnit
import Python.Objects
import Foreign.C.Types
import Python.Types
import Control.Monad ((>=>))
import Data.List
import Python.Interpreter

f :: (ToPyObject a, Eq b, Show b) => String -> a -> (PyObject -> IO b) -> b -> Test
f msg inp code expression = TestLabel msg $ TestCase $ do
    pyo <- toPyObject inp
    r <- code pyo
    expression @=? r


testBase :: [Test]
testBase =
    [   f "showPyObject" (5::CInt) showPyObject "<type 'int'>: 5"
    ]

testLists :: [Test]
testLists =
    [   f "empty" ([]::[CInt]) fromPyObject ([]::[CInt])
    ,   f "repr empty" ([]::[CInt]) reprOf "[]"
    ,   f "some cints" [1::CInt, 2, 3] fromPyObject [1::CInt, 2, 3]
    ,   f "some cints repr" [1::CInt, 2, 3] reprOf "[1, 2, 3]"
    ,   f "strings" ["foo", "bar"] fromPyObject ["foo", "bar"]
    ,   f "strings repr" ["foo", "bar"] reprOf "['foo', 'bar']"
    ]

testAl :: [Test]
testAl =
    [   f "emptypyo" ([]::[(PyObject, PyObject)]) fromPyObject
       ([]::[(PyObject, PyObject)])
    ,   f "cint to cint" [(1::CInt, 2::CInt), (3, 4)]
            (fromPyObject >=> (return . sort))
            [(1::CInt, 2::CInt), (3, 4)]
    ]

testFunctions :: [Test]
testFunctions =
    [   f "typestr" (5::CInt) (typeOf >=> strOf) "<type 'int'>"
    ,   f "repr" ["foo", "bar"] reprOf "['foo', 'bar']"
    ]

testStrings :: [Test]
testStrings =
    [   f "empty" ([]::String) fromPyObject ([]::String)
    ,   f "basic" "foo" fromPyObject "foo"
    ,   f "dquotes" "foo\"" fromPyObject "foo\""
    ,   f "squotes" "foo'" fromPyObject "foo'"
    ,   f "embedded null" "foo\0bar" fromPyObject "foo\0bar"
    ,   f "null only" "\0" fromPyObject "\0"
    ,   f "quotes" "\"'\"" fromPyObject "\"'\""
    ]

testInts :: [Test]
testInts =
    [   f "0L" (0::CLong) fromPyObject (0::CLong)
    ,   f "-5L" (-5::CLong) fromPyObject (-5::CLong)
    ,   f "5L" (5::CLong) fromPyObject (5::CLong)
    ,   f "max long" (maxBound::CLong) fromPyObject (maxBound::CLong)
    ,   f "min long" (minBound::CLong) fromPyObject (minBound::CLong)
    ,   f "0i" (0::CInt) fromPyObject (0::CInt)
    ,   f "-5i" (-5::CInt) fromPyObject (-5::CInt)
    ,   f "5i" (5::CInt) fromPyObject (5::CInt)
    ,   f "min int" (minBound::CInt) fromPyObject (minBound::CInt)
    ,   f "max int" (maxBound::CInt) fromPyObject (maxBound::CInt)
    ,   f "long/int" (12345::CLong) fromPyObject (12345::CInt)
    ,   f "int/long" (12354::CInt) fromPyObject (12354::CInt)
    ,   f "repr max" (maxBound::CLong) reprOf (show (maxBound::CLong))
    ,   f "str min" (minBound::CLong) strOf (show (minBound::CLong))
    ]

testLongs :: [Test]
testLongs =
    [   f "0" (0::Integer) fromPyObject (0::Integer)
    ,   f "-5" (-5::Integer) fromPyObject (-5::Integer)
    ,   f "5" (5::Integer) fromPyObject (5::Integer)
    ,   f "2^384" testBigInt fromPyObject testBigInt
    ,   f "2^384*-1" (( testBigInt * (-1))::Integer) fromPyObject ((testBigInt * (-1))::Integer)
    ,   f "str 2^384" testBigInt strOf (show testBigInt)
    ]

testDoubles :: [Test]
testDoubles =
    [   f "0" (0::CDouble) fromPyObject (0::CDouble)
    ,   f "-5" (-5::CDouble) fromPyObject (-5::CDouble)
    ,   f "5.1234" (5.1234::CDouble) fromPyObject (5.1234::CDouble)
    ,   f "str 5.1234" (5.1234::CDouble) strOf "5.1234"
    ,   f "2^384" testBigDouble fromPyObject testBigDouble
    ,   f "2^384*-1" (testBigDouble * (-1)) fromPyObject (testBigDouble * (-1))
    ,   f "1/(2^384)" (1 / testBigDouble) fromPyObject (1 / testBigDouble)
    ]

testDicts :: [Test]
testDicts =
    [   f "empty" ([]::[(String, String)]) fromPyObject ([]::[(String, String)])
    ,   f "one s" [("foo", "bar")] fromPyObject [("foo", "bar")]
    ,   f "mult s" [("foo", "bar"), ("quux", "baz")]
           (fromPyObject >=> (return . sort))
           [("foo", "bar"), ("quux", "baz")]
    ,   f "s2i" [("foo", 1::CLong), ("quux", 2)]
           (fromPyObject >=> (return.sort))
           [("foo", 1::CLong), ("quux", 2)]
    ]

testCalls :: [Test]
testCalls =
    [   TestCase $ do
            func <- pyRun_String "repr" Py_eval_input []
            r <- pyObject_CallHs func [5::Integer] ([]::[(String, String)])
            "5L" @=? r
    ]

testDir :: [Test]
testDir =
    [   TestCase $ do
            dv <- toPyObject ([]::String) >>= dirPyObject
            assertBool "replace" $ "replace" `elem` dv
            assertBool "rindex" $ "rindex" `elem` dv
    ]

testAttr :: [Test]
testAttr =
    [   TestCase $ do
            pyImport "md5"
            md5 <- pyRun_String "md5.md5()" Py_eval_input []
            fupdate <- getattr md5 "update"
            fhexdigest <- getattr md5 "hexdigest"
            pyObject_RunHs fupdate ["hi"] noKwParms
            pyObject_RunHs fupdate ["there"] noKwParms
            r <- pyObject_CallHs fhexdigest noParms noKwParms
            "a8b767bb9cf0938dc7f40603f33987e5" @=? r
    ,   TestCase $ do
            pyImport "md5"
            md5 <- pyRun_String "md5.md5()" Py_eval_input []
            runMethodHs md5 "update" ["hi"] noKwParms
            runMethodHs md5 "update" ["there"] noKwParms
            r <- callMethodHs md5 "hexdigest" noParms noKwParms
            "a8b767bb9cf0938dc7f40603f33987e5" @=? r

    ]

tests :: Test
tests = TestList    [   TestLabel "base" (TestList testBase)
                    ,   TestLabel "lists/tuples" (TestList testLists)
                    ,   TestLabel "al" (TestList testAl)
                    ,   TestLabel "functions" (TestList testFunctions)
                    ,   TestLabel "strings" (TestList testStrings)
                    ,   TestLabel "ints" (TestList testInts)
                    ,   TestLabel "longs" (TestList testLongs)
                    ,   TestLabel "doubles" (TestList testDoubles)
                    ,   TestLabel "dir" (TestList testDir)
                    ,   TestLabel "call" (TestList testCalls)
                    ,   TestLabel "attr" (TestList testAttr)
                    ,   TestLabel "dict" (TestList testDicts)
                    ]
