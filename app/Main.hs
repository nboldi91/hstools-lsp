{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import Language.LSP.Server as LSP
import Language.LSP.Types as LSP
import Language.LSP.Types.Lens as LSP (uri, params, settings)
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isNothing)
import Control.Lens hiding (Iso)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Errors
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Vector (toList)


handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ \_ -> tryToConnectToDB
  , notificationHandler SWorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- LSP.getConfig
      when (isNothing (connection cfg))
        tryToConnectToDB
      
    -- config <- LSP.getConfig
    -- case loadConfig config cfg of
    --   Right updatedConfig -> LSP.setConfig updatedConfig
    --   Left err -> sendNotification SWindowShowMessage (ShowMessageParams MtError err)

  , requestHandler STextDocumentDefinition $ \req responder -> do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
          Position line column = pos
          lineNum = fromIntegral line + 1 :: Integer
          columnNum = fromIntegral column + 1 :: Integer
      cfg <- LSP.getConfig
      case (uriToFilePath uri, connection cfg) of
        (Just file, Just conn) -> do
          names <- liftIO $ query
              conn
              "SELECT dm.filePath, d.startRow, d.startColumn \
                  \FROM names AS n JOIN names AS d ON n.name = d.name JOIN modules nm ON n.module = nm.moduleId JOIN modules dm ON d.module = dm.moduleId \
                  \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ? \
                  \AND d.isDefined = TRUE"
              ( file, lineNum, lineNum, columnNum, columnNum )
          case names of
              [] -> responder $ Left $ responseError "Definition not found"
              (file, defLine :: Integer, defColumn :: Integer):_ -> 
                let pos = Position (fromIntegral defLine - 1) (fromIntegral defColumn - 1)
                in responder $ Right (InL (Location (filePathToUri file) (LSP.Range pos pos)))
        (Nothing, _) -> responder $ Left $ responseError "Can't go to definition: Document is not a file"
        (_, Nothing) -> responder $ Left $ responseError "Can't go to definition: No database connection"

  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
          Position line column = pos
          lineNum = fromIntegral line + 1 :: Integer
          columnNum = fromIntegral column + 1 :: Integer
      cfg <- LSP.getConfig
      case (uriToFilePath uri, connection cfg) of
        (Just file, Just conn) -> do
          names <- liftIO $ query
              conn
              "SELECT n.isDefined, n.name, n.startRow, n.startColumn, n.endRow, n.endColumn \
                  \FROM names n JOIN modules nm ON n.module = nm.moduleId \
                  \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?"
              ( file, lineNum, lineNum, columnNum, columnNum )
          case names of
              [] -> responder (Right Nothing)  
              (isDefined :: Bool, name :: String, startLine :: Integer, startColumn :: Integer, endLine :: Integer, endColumn :: Integer):_ -> 
                let ms = HoverContents $ markedUpContent "hstools" (T.pack $ name ++ if isDefined then " defined here" else "")
                    range = LSP.Range (Position (fromIntegral startLine - 1) (fromIntegral startColumn - 1)) 
                                      (Position (fromIntegral endLine - 1) (fromIntegral endColumn - 1))
                    rsp = Hover ms (Just range)
                in responder (Right $ Just rsp)  
        (Nothing, _) -> responder $ Left $ responseError "Can't hover: Document is not a file"
        (_, Nothing) -> responder $ Left $ responseError "Can't hover: No database connection"


  -- TODO: handle cancel

  , notificationHandler (SCustomMethod "CleanDB") $ \message -> do
      let NotificationMessage _ _ args = message
      cfg <- LSP.getConfig
      case (args, connection cfg) of
        (A.Array (toList -> [ A.Null ]), Just conn) -> do
          liftIO $ execute_ conn "TRUNCATE modules, names"
          sendMessage "DB cleaned"
        (A.Array (toList -> [ A.String s ]), Just conn)
          -> sendMessage $ "Cleaning DB for path: " <> s
        (_, Nothing) -> sendError "CleanDB needs DB connection"
        (_, _) -> sendError $ T.pack $ "Unrecognized CleanDB argument: " ++ show args
  ]

responseError m = ResponseError InvalidRequest m Nothing

sendMessage = sendNotification SWindowShowMessage . ShowMessageParams MtInfo
sendError = sendNotification SWindowShowMessage . ShowMessageParams MtError

tryToConnectToDB :: LspM Config ()
tryToConnectToDB = do
  config <- LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (postgresqlConnectionString config))
  case connOrError of
    Right conn -> do 
      sendMessage "Connected to DB"
      LSP.setConfig (config { connection = Just conn })
    Left (e :: SomeException) -> return () -- error is OK

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = loadConfig
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions { executeCommandCommands = Just ["cleanDB"] }
  , defaultConfig = Config "" Nothing
  }


loadConfig :: Config -> A.Value -> Either T.Text Config
loadConfig config (A.Object (A.lookup "hstools" -> (Just (A.Object assoc))))
  = Right $ config 
  { postgresqlConnectionString = fromMaybe (postgresqlConnectionString config) $
      T.unpack <$> (fromString =<< psqlstr)
  }
  where
    psqlstr = A.lookup "postgresqlConnectionString" assoc
    fromString (A.String t) = Just t
    fromString _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v
 
data Config = Config
  { postgresqlConnectionString :: String
  , connection :: Maybe Connection
  } deriving (Show)

instance Show Connection where
  show _ = "<Connection>"
