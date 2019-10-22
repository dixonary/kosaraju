module Main where
    
import qualified Duvet

import Data.GVASS
import Data.GVASS.Examples

import Data.VASS.Read
import Data.VASS.Coverability
import Data.VASS.Coverability.Kosaraju
import Data.VASS.Coverability.KarpMiller.Duvet

import Data.VASS.Reachability.Kosaraju

import Options.Applicative

import System.IO.Silently


-- main :: IO ()
-- main = do
--     KReachOptions{..} <- execParser $ optParser `info` mempty

--     problem <- readAny file

--     let withVerbosity = if quiet then silence else id
--         runMode :: IO String
--         runMode       = case mode of
--             KReach -> reach problem
--             KCover -> cover problem

--     result <- withVerbosity $ runMode
--     putStrLn result


-- reach :: CovProblem -> IO String
-- reach problem = do
--     let gvass = generaliseSpec problem
--     res <- kosaraju gvass
--     return $ show res

-- cover :: CovProblem -> IO String
-- cover problem = do
--     res <- kosarajuCover problem
--     return $ show res

-- data KReachMode    
--     = KReach 
--     | KCover

-- data KReachOptions = KReachOptions 
--     { quiet :: Bool
--     , mode  :: KReachMode
--     , file  :: FilePath
--     }

-- optParser :: Parser KReachOptions
-- optParser = KReachOptions
--     <$> switch (long "quiet" <> short 'q' <> help "Hide intermediate output")
--     <*> (
--             flag' KReach (long "reach" <> short 'r' <> help "Treat as reachability problem (default)")
--             <|>
--             flag' KCover (long "cover" <> short 'c' <> help "Treat as coverability problem, by reduction to reachability")
--             <|>
--             pure KReach 
--         )
--     <*> strArgument (metavar "FILENAME" <> help "The file to run the checker on.")



main = silence $ kosaraju (ex5 currentVal) >>= print


currentVal = 8
