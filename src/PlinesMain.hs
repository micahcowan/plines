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
