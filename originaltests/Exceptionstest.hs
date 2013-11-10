{- arch-tag: Exceptions tests main file
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

module Exceptionstest(tests) where
import Test.HUnit
import Python.Objects
import Python.Exceptions
import Python.Exceptions.ExcTypes
import Python.Interpreter
import Python.Types
import Control.Exception (catch)

--f :: (ToPyObject a, Eq b, Show b) => String -> a -> (PyObject -> IO b) -> b -> Test
--f msg inp code expression = TestLabel msg $ TestCase $ do
--    pyo <- toPyObject inp
--    r <- code pyo
--    expression @=? r

testBase :: [Test]
testBase =
    let handler = const $ return ()
        in
        [   TestCase $ catchPy (do
                r <- pyRun_StringHs " 2 + None" Py_eval_input noKwParms
                r @=? "no exception raised"
                ) handler
        ,   TestCase $ catchSpecificPy typeError (do
                r <- pyRun_StringHs "2 + None" Py_eval_input noKwParms
                r @=? "no exception raised"
                ) handler
        ,   TestCase $ catchSpecificPy valueError (
                catch (do
                    r <- pyRun_StringHs "2 + None" Py_eval_input noKwParms
                    r @=? "no exception raised"
                    )
                (\PyException {} -> return () )
                ) (const $ fail "Incorrectly caught exception")
        ]

tests :: Test
tests = TestList [  TestLabel "base" (TestList testBase)
                 ]
