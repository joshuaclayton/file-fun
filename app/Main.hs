{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Exception as E
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Bifunctor as BF
import qualified Data.Bool as B
import qualified Data.Char as C
import           Options.Applicative

-- types

data Options = Options
    { oCapitalize :: Bool
    , oExcited :: Bool
    , oStdIn :: Bool
    , oFileToRead :: Maybe String
    }

type AppConfig = MonadReader Options
data AppError
    = IOError E.IOException

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

-- program

main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Options -> IO ()
runProgram o = either renderError return =<< runExceptT (runReaderT (runApp run) o)

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e

run :: App ()
run = liftIO . putStr
    =<< handleExcitedness
    =<< handleCapitalization
    =<< getSource

-- data retrieval and transformation

getSource :: App String
getSource = B.bool loadContents (liftIO getContents) =<< asks oStdIn

handleCapitalization :: AppConfig m => String -> m String
handleCapitalization s = B.bool s (map C.toUpper s) <$> asks oCapitalize

handleExcitedness :: AppConfig m => String -> m String
handleExcitedness s = B.bool s ("ZOMG " ++ s) <$> asks oExcited

loadContents :: App String
loadContents =
    maybe defaultResponse readFileFromOptions =<< asks oFileToRead
  where
    readFileFromOptions f = either throwError return =<< BF.first IOError <$> liftIO (safeReadFile f)
    defaultResponse = return "This is fun!"

-- CLI parsing

parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "capitalize")
    <*> (switch $ long "excited")
    <*> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")

-- safer reading of files

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
