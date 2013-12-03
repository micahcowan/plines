{-
    OptsParse module

    One-at-a-time option parser, allowing for modifications to the
    options lookup table between option lookups (i.e., allowing options
    to modify what options are available.
-}

module OptParse
    (
        OptDesc(..),
        ArgDesc(..),
        takesArg,
        OptTable,       -- useful?
        OptParseCtx,    -- but not constructors
        newOptParseCtx,
        shortOptTable,
        longOptTable,
        getNextOpt,
    ) where

-- public
data OptDesc a =
    ShortOptNoArg Char a
    | LongOptNoArg String a
    | ShortOptArg Char String a
    | LongOptArg String String a
    | RemainingArgs [String]    -- Indicates end of processing.
    -- The rest are error signals
    | ShortOptMissingArg Char
    | LongOptMissingArg String
    | UnknownShortOpt Char
    | UnknownLongOpt String
    deriving (Show)

-- public
data ArgDesc = TakesNoArg | TakesArg deriving (Show)

-- public
takesArg :: ArgDesc -> Bool
takesArg TakesNoArg = False
takesArg TakesArg   = True

type OptTable key payload = [(key, (ArgDesc, payload))]

-- public type, PRIVATE constructor/(some) field accessors
data OptParseCtx a =
    OptParseCtx {
        shortOptTable   :: OptTable Char a,     -- public (for updates)
        longOptTable    :: OptTable String a,   -- public (for updates)
        partialShort    :: String,
        mixedArgs       :: [String],    -- args encountered btwn opts
        remainArgs      :: [String]
    }

-- public OptPraseCtx creator
newOptParseCtx :: [String] -> OptParseCtx a
newOptParseCtx args =
    OptParseCtx {
        shortOptTable = [],
        longOptTable  = [],
        partialShort  = "",
        mixedArgs     = [],
        remainArgs    = args
    }

-- public (crucial)
getNextOpt :: OptParseCtx a -> (OptParseCtx a, OptDesc a)

-- Handle any remaining short options from previous arg.
getNextOpt ctx@(OptParseCtx { partialShort = partial@(opt:opts) })
    = case foundOpt of
        Nothing                     -> (ctxNextOpt, UnknownShortOpt opt)
        Just (TakesNoArg, payload)  ->
            (ctxNextOpt, ShortOptNoArg opt payload)
        Just (TakesArg, payload) ->
            case opts of
                [] -> case remainArgs ctx of
                           (arg:args) ->
                               (
                                   ctxNextOpt {remainArgs=args},
                                   ShortOptArg opt arg payload
                               )
                           _ -> (ctxNextOpt, ShortOptMissingArg opt)
                _  -> (
                          ctx { partialShort = [] },
                          ShortOptArg opt opts payload
                      )
  where
    ctxNextOpt = ctx { partialShort = opts }
    foundOpt = lookup opt (shortOptTable ctx)

-- Handle no args remaining
getNextOpt ctx@(OptParseCtx { remainArgs = [] })
    = (
        ctx { mixedArgs = [] },
        RemainingArgs . reverse $ mixedArgs ctx
      )

-- Terminate when "--" arg reached
getNextOpt ctx@(OptParseCtx { remainArgs = ("--":args) })
    = (
        ctx { mixedArgs = [], remainArgs = [] },
        RemainingArgs $ reverse (mixedArgs ctx) ++ args
      )

-- Handle long options
getNextOpt ctx@(OptParseCtx { remainArgs = ('-':'-':opt):args })
    = case foundOpt of
        Nothing                     -> (ctxNextArg, UnknownLongOpt opt)
        Just (TakesNoArg, payload)  ->
            (ctxNextArg, LongOptNoArg opt payload)
        Just (TakesArg, payload)    ->
            case args of
                (next:trail) ->
                    (
                        ctx { remainArgs = trail },
                        LongOptArg opt next payload
                    )
                _ ->
                    (ctxNextArg, LongOptMissingArg opt)
  where
    ctxNextArg = ctx { remainArgs = args }
    foundOpt   = lookup opt (longOptTable ctx)

-- Handle short options
getNextOpt ctx@(OptParseCtx { remainArgs = ('-':opts):args })
    = getNextOpt ctx { remainArgs = args, partialShort = opts }

-- Handle args (possibly) mixed in with options
getNextOpt ctx@(OptParseCtx { remainArgs = arg:args })
    = getNextOpt ctx { remainArgs = args, mixedArgs = arg : mixedArgs ctx }
