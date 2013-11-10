{- arch-tag: BZip2 tests main file
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

module BZip2test (tests) where
import Test.HUnit
import Python.Exceptions
import MissingPy.FileArchive.BZip2
import Control.Monad (unless)
import System.IO.HVIO
import System.IO
import System.IO.Error
import Testutil
import System.Directory
import Control.Exception (finally)

f' :: (a -> String -> IO b) -> String -> a -> Test
f' mf fn expression = TestLabel fn $ TestCase $ handlePy exc2ioerror $ do 
    bzf <- openBz2 ("testfiles/bz2files/" ++ fn) ReadMode 9
    c <- vGetContents bzf
    _ <- mf expression c
    vClose bzf

f :: String -> String -> Test
f = f' (@=?)

f2 :: String -> String -> Test
f2 = f' nodisptest

-- @=? loads the whole thing into memory.  ick.  This is much better.
nodisptest :: (Eq a) => a -> a -> IO ()
nodisptest x y = unless (x == y) $ assertFailure "Data mismatch"

testBunzip2 :: [Test]
testBunzip2 =
    [   f "t1.bz2" "Test 1"
    ,   f "t2.bz2" "Test 1Test 2"
    ,   f "empty.bz2" ""
    ,   TestCase $ do
            bzf <- openBz2 "testfiles/bz2files/zeros.bz2" ReadMode 1
            c <- vGetContents bzf
            10485760 @=? length c
            vClose bzf
    ,   f2 "zeros.bz2" (replicate 10485760 '\0')
    ]

testBzip2 :: Test
testBzip2 = TestCase $
    handlePy exc2ioerror $ do
        bzf <- openBz2 "testfiles/bz2files/deleteme.bz2" WriteMode 9
        finally (do
            vPutStr bzf "Test 2\n"
            vFlush bzf
            vPutStr bzf "Test 3\n"
            vPutStr bzf (replicate 1048576 't')
            vPutChar bzf '\n'
            vClose bzf
            bzf2 <- openBz2 "testfiles/bz2files/deleteme.bz2" ReadMode 9
            vGetLine bzf2 >>= (@=? "Test 2")
            vGetLine bzf2 >>= (@=? "Test 3")
            _ <- vGetLine bzf2
            assertRaises "eof" (mkIOError eofErrorType "" Nothing Nothing) (vGetLine bzf2)

            vRewind bzf2
            c <- vGetContents bzf2
            ("Test 2\nTest 3\n" ++ replicate 1048576 't' ++ "\n")
                @=? c
            assertRaises "closed" (mkIOError illegalOperationErrorType "" Nothing Nothing) (vGetLine bzf2)
            vClose bzf2
            ) $ removeFile "testfiles/bz2files/deleteme.bz2"

tests :: Test
tests = TestList    [   TestLabel "bzip2" testBzip2
                    ,   TestLabel "bunzip2" (TestList testBunzip2)
                    ]
