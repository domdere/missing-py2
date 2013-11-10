{- arch-tag: AnyDBM/Dict Python Tests Main File
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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

module Dicttest(mf, genericTest, tests) where
import Test.HUnit
import Data.List.Utils
import System.IO.HVFS
import System.IO.HVFS.InstanceHelpers
import Database.AnyDBM
import Database.AnyDBM.StringDBM
import Database.AnyDBM.MapDBM
import Data.HashTable
import Data.List (sort)
import Control.Exception (finally)
import Control.Monad (liftM)
import Python.Objects
import Python.Objects.Dict

mf :: AnyDBM a => IO b -> (b -> IO a) -> String -> (a -> Assertion) -> Test
mf initfunc openfunc msg code =
    TestLabel msg $ TestCase $ do
        i <- initfunc
        h <- openfunc i
        finally (code h) (closeA h)

infix 1 @>=?
(@>=?) :: (Eq a, Show a) => a -> IO a -> Assertion
(@>=?) exp res = do r <- res
                    exp @=? r

deleteall h = do k <- keysA h
                 mapM_ (deleteA h) k
                 [] @>=? keysA h

weirdl = sort [("", "empty"),
                 ("foo\nbar", "v1\0v2"),
                 ("v3,v4", ""),
                 ("k\0ey", "")]

genericTest initfunc openfunc =
    let
        f = mf initfunc openfunc
    in
        [   f "empty" $ \h -> do
                [] @>=? keysA h
                [] @>=? valuesA h
                [] @>=? toListA h
                Nothing @>=? lookupA h "foo"
        ,   f "basic" $ \h -> do
                insertA h "key" "value"
                Just "value" @>=? lookupA h "key"
                [("key", "value")] @>=? toListA h
                insertA h "key" "v2"
                [("key", "v2")] @>=? toListA h
                deleteA h "key"
                [] @>=? toListA h
        ,   f "mult" $ \h -> do
                insertListA h [("1", "2"), ("3", "4"), ("5", "6")]
                [("1", "2"), ("3", "4"), ("5", "6")] @>=?
                    liftM sort (toListA h)
                ["1", "3", "5"] @>=? liftM sort (keysA h)
                ["2", "4", "6"] @>=? liftM sort (valuesA h)
                deleteall h
        ,   f "weirdchars" $ \h -> do
                insertListA h weirdl
                weirdl @>=? liftM sort (toListA h)
                deleteall h
        ]

genericPersistTest initfunc openfunc =
    let f = mf initfunc openfunc in
        [
         f "empty" deleteall 
        ,f "weirdpop" $ \h -> insertListA h weirdl
        ,f "weirdcheck" $ \h -> do weirdl @>=? liftM sort (toListA h)
                                   deleteall h
                                   insertA h "key" "value"
        ,f "step3" $ \h -> do [("key", "value")] @>=? liftM sort (toListA h)
                              insertA h "key" "v2"
                              insertA h "z" "y"
        ,f "step4" $ \h -> [("key", "v2"), ("z", "y")] @>=?
                                 liftM sort (toListA h)
        ,f "cleanup" deleteall
        ]

testDict = genericTest (return ())
             (\_ -> toPyObject ([]::[(String, String)]) >>= return . mkPyDict)



tests = TestList [TestLabel "Basic Dict" (TestList testDict)
                 ]



