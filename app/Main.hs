{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Language.LSP.Server as LSP
import Language.LSP.Types as LSP
import Language.LSP.Types.Lens as LSP (uri, params, settings)
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isNothing)
import Data.String (IsString(..))
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
  [ notificationHandler SInitialized $ \_ -> runInContext "Initialization" tryToConnectToDB
  , notificationHandler SWorkspaceDidChangeConfiguration $ \msg -> runInContext "ChangeConfiguration" $ do
      cfg <- liftLSP LSP.getConfig
      when (isNothing (cfConnection cfg))
        tryToConnectToDB
      
  , requestHandler STextDocumentDefinition $ \req responder -> runInContext "Definition" $ do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
          Position line column = pos
          lineNum = fromIntegral line + 1 :: Integer
          columnNum = fromIntegral column + 1 :: Integer
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          names <- liftIO $ query
            conn
            "SELECT dm.filePath, d.startRow, d.startColumn \
                \FROM ast AS n JOIN names nn ON nn.astNode = n.astId \
                  \JOIN names AS dn ON nn.name = dn.name \
                  \JOIN ast AS d ON d.astId = dn.astNode \
                  \JOIN modules nm ON n.module = nm.moduleId \
                  \JOIN modules dm ON d.module = dm.moduleId \
                \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ? \
                \AND dn.isDefined = TRUE"
            ( file, lineNum, lineNum, columnNum, columnNum )
          case names of
            [] -> liftLSP $ responder $ Left $ responseError "Definition not found"
            (file, defLine :: Integer, defColumn :: Integer):_ -> 
              let pos = Position (fromIntegral defLine - 1) (fromIntegral defColumn - 1)
              in liftLSP $ responder $ Right (InL (Location (filePathToUri file) (LSP.Range pos pos)))

  , requestHandler STextDocumentReferences $ \req responder -> runInContext "References" $ do
      let RequestMessage _ _ _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) = req
          Position line column = pos
          lineNum = fromIntegral line + 1 :: Integer
          columnNum = fromIntegral column + 1 :: Integer
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          names <- liftIO $ query
            conn
            (fromString $ "SELECT dm.filePath, d.startRow, d.startColumn \
                \FROM ast AS n JOIN names nn ON nn.astNode = n.astId \
                  \JOIN names AS dn ON nn.name = dn.name \
                  \JOIN ast AS d ON d.astId = dn.astNode \
                  \JOIN modules nm ON n.module = nm.moduleId \
                  \JOIN modules dm ON d.module = dm.moduleId \
                \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?"
                  ++ (if not includeDefinition then "\n AND dn.isDefined = FALSE" else ""))
            ( file, lineNum, lineNum, columnNum, columnNum )
          let
            toPos line col = Position (fromIntegral line - 1) (fromIntegral col - 1) 
            lineToLoc :: (String, Integer, Integer) -> LSP.Location
            lineToLoc (file,line,col) = Location (filePathToUri file) (LSP.Range (toPos line col) (toPos line col))
          liftLSP $ responder $ Right $ LSP.List (map lineToLoc names)

  , requestHandler STextDocumentHover $ \req responder -> runInContext "Hover" $ do
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
          Position line column = pos
          lineNum = fromIntegral line + 1 :: Integer
          columnNum = fromIntegral column + 1 :: Integer
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          names <- liftIO $ query
            conn
            "SELECT tn.type, nn.isDefined, nn.name, n.startRow, n.startColumn, n.endRow, n.endColumn \
                \FROM ast n \
                \LEFT JOIN names nn ON nn.astNode = n.astId \
                \JOIN modules nm ON n.module = nm.moduleId \
                \LEFT JOIN types tn ON n.astId = tn.astNode \
                \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?"
            ( file, lineNum, lineNum, columnNum, columnNum )
          case names of
            [] -> liftLSP $ responder (Right Nothing)  
            (typ, isDefined, name, startLine :: Integer, startColumn :: Integer, endLine :: Integer, endColumn :: Integer):_ -> 
              let ms = HoverContents $ markedUpContent "hstools" $ T.pack
                          $ name ++ (if isDefined == Just True then " defined here" else "")
                              ++ (maybe "" ("\n  :: " ++) typ)
                  range = LSP.Range (Position (fromIntegral startLine - 1) (fromIntegral startColumn - 1)) 
                                    (Position (fromIntegral endLine - 1) (fromIntegral endColumn - 1))
                  rsp = Hover ms (Just range)
              in liftLSP $ responder (Right $ Just rsp)  


  , notificationHandler SCancelRequest $ const $ return ()

  , notificationHandler (SCustomMethod "CleanDB") $ \message -> runInContext "CleanDB" $ do
      let NotificationMessage _ _ args = message
      withConnection $ \conn ->
        case args of
          A.Array (toList -> [ A.Null ]) -> do
            liftIO $ execute_ conn "drop table modules, ast, names, types cascade"
            sendMessage "DB cleaned"
          A.Array (toList -> [ A.String s ])
            -> sendMessage $ "Cleaning DB for path: " <> s
          _ -> sendError $ T.pack $ "Unrecognized CleanDB argument: " ++ show args
  ]

data LspContext = LspContext { ctOperation :: String }

type LspMonad c = ReaderT LspContext (LspM c)

liftLSP :: LspM c a -> LspMonad c a
liftLSP = lift

runInContext :: String -> LspMonad c a -> LspM c a
runInContext operation action = runReaderT action (LspContext { ctOperation = operation })

withConnection :: (Connection -> LspMonad Config ()) -> LspMonad Config ()
withConnection act = do
  operation <- asks ctOperation
  cfg <- lift LSP.getConfig
  case cfConnection cfg of
    Just conn -> act conn
    Nothing -> sendError $ T.pack $ operation ++ " needs DB connection"

ensureFileLocation :: Uri -> (Either ResponseError a -> LspM Config ()) -> (FilePath -> LspMonad Config ()) -> LspMonad Config ()
ensureFileLocation location responder action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    liftLSP $ responder $ Left $ responseError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

responseError m = ResponseError InvalidRequest m Nothing

sendMessage :: T.Text -> LspMonad Config ()
sendMessage = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtInfo

sendError :: T.Text -> LspMonad Config ()
sendError = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtError

tryToConnectToDB :: LspMonad Config ()
tryToConnectToDB = do
  config <- liftLSP LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (cfPostgresqlConnectionString config))
  case connOrError of
    Right conn -> do 
      sendMessage "Connected to DB"
      liftLSP $ LSP.setConfig (config { cfConnection = Just conn })
    Left (e :: SomeException) -> return () -- error is OK

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = loadConfig
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) (liftIO)
  , options = defaultOptions { executeCommandCommands = Just ["cleanDB"] }
  , defaultConfig = Config "" Nothing Nothing
  }


loadConfig :: Config -> A.Value -> Either T.Text Config
loadConfig config (A.Object (A.lookup "hstools" -> (Just (A.Object assoc))))
  = Right $ config 
  { cfPostgresqlConnectionString = fromMaybe (cfPostgresqlConnectionString config) $
      T.unpack <$> (fromString =<< psqlstr)
  }
  where
    psqlstr = A.lookup "postgresqlConnectionString" assoc
    fromString (A.String t) = Just t
    fromString _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v
 
data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: Maybe Connection
  , cfOperation :: Maybe String
  } deriving (Show)

instance Show Connection where
  show _ = "<Connection>"
