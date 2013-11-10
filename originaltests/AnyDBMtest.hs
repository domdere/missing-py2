{- arch-tag: AnyDBM tests main file
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

module AnyDBMtest(mf, genericPersistTest, genericTest, tests) where
import Test.HUnit
import System.IO.HVFS
import Control.Monad (liftM)
import Database.AnyDBM
import Database.AnyDBM.StringDBM
import Database.AnyDBM.MapDBM
import System.Directory
import System.IO.HVFS.Utils
import Data.HashTable
import Data.List (sort)
import Control.Exception (finally)

mf :: AnyDBM a => IO b -> (b -> IO a) -> String -> (a -> Assertion) -> Test
mf initfunc openfunc msg code =
    TestLabel msg $ TestCase $ do i <- initfunc
                                  h <- openfunc i
                                  finally (code h) (closeA h)

infix 1 @>=?
(@>=?) :: (Eq a, Show a) => a -> IO a -> Assertion
(@>=?) exp res = do r <- res
                    exp @=? r

deleteall h = do
    k <- keysA h
    mapM_ (deleteA h) k
    [] @>=? keysA h

weirdl = sort   [   ("", "empty")
                ,   ("foo\nbar", "v1\0v2")
                ,   ("v3,v4", "")
                ,   ("k\0ey", "\xFF")
                ]

createdir = TestCase $ createDirectory "testtmp"
removedir = TestCase $ recursiveRemove SystemFS "testtmp"

genericTest initfunc openfunc =
    let f = mf initfunc openfunc in
        [   createdir
        ,   f "empty" $ \h -> do
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
        ,   removedir
        ]

genericPersistTest initfunc openfunc =
    let f = mf initfunc openfunc in
        [   createdir
        ,   f "empty" deleteall
        ,   f "weirdpop" $ \h -> insertListA h weirdl
        ,   f "weirdcheck" $ \h -> do
                weirdl @>=? liftM sort (toListA h)
                deleteall h
                insertA h "key" "value"
        ,   f "step3" $ \h -> do
                [("key", "value")] @>=? liftM sort (toListA h)
                insertA h "key" "v2"
                insertA h "z" "y"
        ,   f "step4" $ \h -> [("key", "v2"), ("z", "y")] @>=?
                liftM sort (toListA h)
        ,   f "cleanupdb" deleteall
        ,   removedir
        ]

testHashtable = genericTest (return ())
                  (\_ -> new (==) hashString :: IO (HashTable String String))

testMap = genericTest (return ())
    (const newMapDBM)

testStringdbm = genericPersistTest (return SystemFS)
                   (\f -> openStringVDBM f "testtmp/StringDBM" ReadWriteMode)
                 ++
                 genericTest (return SystemFS)
                   (\f -> openStringVDBM f "testtmp/StringDBM" ReadWriteMode)

tests = TestList    [   TestLabel "HashTable" (TestList testHashtable)
                    ,   TestLabel "StringDBM" (TestList testStringdbm)
                    ,   TestLabel "Map" (TestList testMap)
                    ]



