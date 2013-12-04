{-
   PlinesMain.hs : Plines program starting point

   Copyright (c) 2013 Micah Cowan
 
   [MIT license terms:]
   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:
 
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
 
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import System.Environment (getArgs)

import OptParse
import qualified OptBuiltins as B

main = do
    args <- getArgs
    let ctx = (newOptParseCtx args) {
        shortOptTable = B.shortOptions,
        longOptTable  = B.longOptions
    }
    processArgs ctx

processArgs :: OptParseCtx (String -> B.OptAction) -> IO ()
processArgs ctx =
  let (newCtx, opt) = getNextOpt ctx
  in do
    putStrLn $ case opt of 
        ShortOptNoArg _ x -> show $ x undefined
        LongOptNoArg  _ x -> show $ x undefined
        ShortOptArg   _ a x -> show $ x a
        LongOptArg    _ a x -> show $ x a
        RemainingArgs args -> "Remaining args: " ++ show args
    case opt of
        RemainingArgs _ -> return ()
        _  -> processArgs newCtx
