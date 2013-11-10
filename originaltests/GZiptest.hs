{- arch-tag: GZip tests main file
Copyright (C) 2005-2008 John Goerzen <jgoerzen@complete.org>

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

module GZiptest(tests) where
import Test.HUnit
import Python.Exceptions
import MissingPy.FileArchive.GZip
import System.IO.HVIO
import System.IO
import System.IO.Error
import Testutil
import System.Directory
import Control.Exception (finally)

f :: String -> String -> Test
f fn expression = TestCase $ do
    gzf <- openGz ("testfiles/gzfiles/" ++ fn) ReadMode 9
    c <- vGetContents gzf
    expression @=? c
    vClose gzf

testGunzip :: [Test]
testGunzip =
    [   f "t1.gz" "Test 1"
    ,   f "empty.gz" ""
     --    , "t1bad has errors
    ,   f "t2.gz" "Test 1Test 2"
    ,   TestCase $ handlePy exc2ioerror $
                do gzf <- openGz "testfiles/gzfiles/t1bad.gz" ReadMode 1
                   assertRaises "crc" (userError "Python <type 'exceptions.IOError'>: CRC check failed")
                      (handlePy exc2ioerror $ do c <- vGetContents gzf
                                                 "nonexistant bad data" @=? c
                      )
                   vClose gzf
    ,   TestCase $ do
            gzf <- openGz "testfiles/gzfiles/zeros.gz" ReadMode 1
            c <- vGetContents gzf
            10485760 @=? length c
            vClose gzf
    --,   f "zeros.gz" (replicate 10485760 '\0')
    ]

testGzip :: Test
testGzip = TestCase $
    handlePy exc2ioerror $
    do gzf <- openGz "testfiles/gzfiles/deleteme.gz" ReadWriteMode 9
       finally (do vPutStr gzf "Test 2\n"
                   vSeek gzf AbsoluteSeek 7
                   vFlush gzf
                   vPutStr gzf "Test 3\n"
                   vPutStr gzf (replicate 1048576 't')
                   vPutChar gzf '\n'
                   vClose gzf
                   gzf2 <- openGz "testfiles/gzfiles/deleteme.gz" ReadMode 9
                   vGetLine gzf2 >>= (@=? "Test 2")
                   vGetLine gzf2 >>= (@=? "Test 3")
                   vRewind gzf2
                   c <- vGetContents gzf2
                   ("Test 2\nTest 3\n" ++ replicate 1048576 't' ++ "\n")
                      @=? c
                   assertRaises "eof" (mkIOError eofErrorType "" Nothing Nothing) (vGetLine gzf2)
                   vClose gzf2
               ) (removeFile "testfiles/gzfiles/deleteme.gz")

tests :: Test
tests = TestList    [   TestLabel "gzip" testGzip
                    ,   TestLabel "gunzip" (TestList testGunzip)
                    ]
