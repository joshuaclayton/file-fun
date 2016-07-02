{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Exception as E
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Bifunctor as BF
import qualified Data.Bool as B
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Time.Clock as Cl
import qualified Data.Time.Format as Cl
import           Options.Applicative
import qualified System.Log.FastLogger as S

-- types

data Options = Options
    { oCapitalize :: Bool
    , oExcited :: Bool
    , oStdIn :: Bool
    , oFileToRead :: Maybe String
    , oVerbosity :: Maybe LogLevel
    }

type AppConfig = MonadReader Options
data AppError
    = IOError E.IOException

newtype App a = App {
    runApp :: LoggingT (ReaderT Options (ExceptT AppError IO)) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

instance MonadLogger App where
    monadLoggerLog = loggerFunc

-- program

main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Options -> IO ()
runProgram o = either renderError return =<< (runExceptT (runReaderT (runStdoutLoggingT $ runApp run) o))

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e

run :: App ()
run = do
    $logDebug "beginning run"

    liftIO . putStr
        =<< handleExcitedness
        =<< handleCapitalization
        =<< getSource

-- data retrieval and transformation

getSource :: App String
getSource = do
    $logInfo "retrieving source"

    B.bool loadContents (liftIO getContents) =<< asks oStdIn

handleCapitalization :: AppConfig m => String -> m String
handleCapitalization s = B.bool s (map C.toUpper s) <$> asks oCapitalize

handleExcitedness :: (MonadLogger m, AppConfig m) => String -> m String
handleExcitedness s = do
    $logInfo "handling excitedness"

    B.bool s ("ZOMG " ++ s) <$> asks oExcited

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
    <*> ((verbosity =<<) <$> optional (strOption $ long "verbosity"))

verbosity :: String -> Maybe LogLevel
verbosity "debug" = Just LevelDebug
verbosity "info"  = Just LevelInfo
verbosity "warn"  = Just LevelWarn
verbosity "error" = Just LevelError
verbosity _ = Nothing

-- safer reading of files

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile

-- logger

loggerFunc :: (AppConfig m, MonadIO m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> m ()
loggerFunc _ _ l msg = do
    lvl <- asks oVerbosity
    when (isLoggable lvl) $ do
        now <- Cl.formatTime Cl.defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> liftIO Cl.getCurrentTime

        liftIO $ putStrLn $ now ++ " " ++ printLevel l ++ ": " ++ msgTextRaw
  where
    printLevel = map C.toUpper . drop 5 . show
    isLoggable = maybe False (<= l)
    msgTextRaw = T.unpack $ T.decodeUtf8With T.lenientDecode msgBytes
    msgBytes = S.fromLogStr $ toLogStr msg
