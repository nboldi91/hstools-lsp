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
import Language.LSP.Types.Lens as LSP (uri, params, settings, changes)
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.String (IsString(..))
import Control.Lens hiding (Iso)
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Errors
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Vector (toList)
import Language.Haskell.HsTools.LinesDiff
import Text.Read (readMaybe)


handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ \_ -> runInContext "Initialization" tryToConnectToDB
  , notificationHandler SWorkspaceDidChangeConfiguration $ \_ -> runInContext "ChangeConfiguration" $ do
      cfg <- liftLSP LSP.getConfig
      when (isNothing (cfConnection cfg))
        tryToConnectToDB
      
  , requestHandler STextDocumentDefinition $ \req responder -> runInContext "Definition" $ do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          rewrites <- getRewrites file
          case newToOriginalPos rewrites (posToSP pos) of
            Right originalPos -> do
              names <- liftIO $ query
                conn
                "SELECT dm.filePath, d.startRow, d.startColumn, d.endRow, d.endColumn \
                    \FROM ast AS n JOIN names nn ON nn.astNode = n.astId \
                      \JOIN names AS dn ON nn.name = dn.name AND nn.namespace = dn.namespace \
                      \JOIN ast AS d ON d.astId = dn.astNode \
                      \JOIN modules nm ON n.module = nm.moduleId \
                      \JOIN modules dm ON d.module = dm.moduleId \
                    \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ? \
                    \AND dn.isDefined = TRUE"
                ( file, spLine originalPos, spLine originalPos, spCol originalPos, spCol originalPos )
              liftLSP $ responder $ Right $ InR $ InL $ LSP.List $ take 1 $ catMaybes $ map (lineToLoc rewrites) names
            Left _ -> liftLSP $ responder $ Right $ InR $ InL $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentReferences $ \req responder -> runInContext "References" $ do
      let RequestMessage _ _ _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) = req
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          rewrites <- getRewrites file
          case newToOriginalPos rewrites (posToSP pos) of
            Right originalPos -> do
              names <- liftIO $ query
                conn
                (fromString $ "SELECT dm.filePath, d.startRow, d.startColumn, d.endRow, d.endColumn \
                    \FROM ast AS n JOIN names nn ON nn.astNode = n.astId \
                      \JOIN names AS dn ON nn.name = dn.name AND nn.namespace = dn.namespace \
                      \JOIN ast AS d ON d.astId = dn.astNode \
                      \JOIN modules nm ON n.module = nm.moduleId \
                      \JOIN modules dm ON d.module = dm.moduleId \
                    \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?"
                      ++ (if not includeDefinition then "\n AND dn.isDefined = FALSE" else ""))
                ( file, spLine originalPos, spLine originalPos, spCol originalPos, spCol originalPos )
              liftLSP $ responder $ Right $ LSP.List $ catMaybes $ map (lineToLoc rewrites) names
            Left _ -> liftLSP $ responder $ Right $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentHover $ \req responder -> runInContext "Hover" $ do
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          rewrites <- getRewrites file
          case newToOriginalPos rewrites (posToSP pos) of
            Right originalPos -> do
              let lineNum = fromIntegral (spLine originalPos) :: Integer
                  columnNum = fromIntegral (spCol originalPos) :: Integer
              names <- liftIO $ query
                conn
                "SELECT tn.type, nn.isDefined, nn.name, n.startRow, n.startColumn, n.endRow, n.endColumn \
                    \FROM ast n \
                    \JOIN names nn ON nn.astNode = n.astId \
                    \JOIN modules nm ON n.module = nm.moduleId \
                    \LEFT JOIN types tn ON n.astId = tn.astNode \
                    \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?"
                ( file, lineNum, lineNum, columnNum, columnNum )
              case names of
                [] -> liftLSP $ responder (Right Nothing)  
                (typ, isDefined, name, startLine :: Int, startColumn :: Int, endLine :: Int, endColumn :: Int):_ -> 
                  let ms = HoverContents $ markedUpContent "hstools" $ T.pack
                              $ name ++ (if isDefined == True then " defined here" else "")
                                  ++ (maybe "" ("\n  :: " ++) typ)
                      origRange = SourceRange (SP startLine startColumn) (SP endLine endColumn)
                      newRange = originalToNewRangeStrict rewrites origRange
                      rsp = Hover ms (fmap rangeToLSP newRange)
                  in liftLSP $ responder (Right $ Just rsp)  
            Left _ -> liftLSP $ responder (Right Nothing) -- the source position was not in the compiled source code


  , notificationHandler SCancelRequest $ const $ return ()

  , notificationHandler (SCustomMethod "CleanDB") $ \message -> runInContext "CleanDB" $ withConnection $ \conn -> do
      let NotificationMessage _ _ args = message
      case args of
        A.Array (toList -> [ A.Null ]) -> do
          liftIO $ execute_ conn "DROP TABLE modules, ast, names, types, thRanges CASCADE"
          sendMessage "DB cleaned"
        A.Array (toList -> [ A.String s ])
          -> sendMessage $ "Cleaning DB for path: " <> s
        _ -> sendError $ T.pack $ "Unrecognized CleanDB argument: " ++ show args

  , notificationHandler SWorkspaceDidChangeWatchedFiles $ \message -> runInContext "FileChange" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidChangeWatchedFilesParams (LSP.List fileChanges)) = message
      forM_ fileChanges $ \(FileEvent uri _) -> ensureFileLocation' uri $ \filePath -> do
        cfg <- LSP.getConfig
        unless (isFileOpen filePath $ cfFileRecords cfg) $ do
          compiledSource <- liftIO $ query conn "SELECT compiledSource FROM modules WHERE filePath = ?" (Only filePath)
          case compiledSource of
            [[source]] -> do
              updatedSource <- liftIO $ readFile filePath
              let fileDiffs = sourceDiffs startSP source updatedSource
              modifyConfig' $ \cf -> cf { cfFileRecords = replaceSourceDiffs filePath fileDiffs $ cfFileRecords cf }
              currentTime <- liftIO getCurrentTime
              let serializedDiff = nothingIfEmpty $ serializeSourceDiffs fileDiffs
              void $ liftIO $ execute conn "UPDATE modules SET modifiedTime = ?, modifiedFileDiffs = ? WHERE filePath = ?" (currentTime, serializedDiff, filePath)
            _ -> return () -- the file is not compiled yet, nothing to do
  
  , notificationHandler STextDocumentDidChange $ \msg -> runInContext "STextDocumentDidChange" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _version) (LSP.List changes)) = msg
      ensureFileLocation' uri $ \filePath -> do
        let goodChanges = catMaybes $ map textDocChangeToSD changes
        void $ modifyConfig' $ \cf -> cf { cfFileRecords = updateSavedFileRecords filePath goodChanges (cfFileRecords cf) }

  , notificationHandler STextDocumentDidSave $ \msg -> runInContext "STextDocumentDidSave" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _reason) = msg
      ensureFileLocation' uri $ \filePath -> do
        cfg <- LSP.getConfig
        let fileDiffs = maybe Map.empty frDiffs $ Map.lookup filePath $ fromMaybe Map.empty $ cfFileRecords cfg
        currentTime <- liftIO getCurrentTime
        let serializedDiff = serializeSourceDiffs fileDiffs
        void $ liftIO $ execute conn "UPDATE modules SET modifiedTime = ?, modifiedFileDiffs = ? WHERE filePath = ?" (currentTime, serializedDiff, filePath)
  
  , notificationHandler STextDocumentDidOpen $ \msg -> runInContext "STextDocumentDidOpen" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _langId _version content)) = msg
      ensureFileLocation' uri $ \filePath ->
        void $ modifyConfig $ \cf -> recordFileOpened conn filePath content (cfFileRecords cf) >>= \frs -> return cf{cfFileRecords = frs}
  
  , notificationHandler STextDocumentDidClose $ \msg -> runInContext "STextDocumentDidClose" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = msg
      ensureFileLocation' uri $ \filePath ->
        void $ modifyConfig $ \cf -> recordFileClosed conn filePath (cfFileRecords cf) >>= \frs -> return cf{cfFileRecords = frs}
 
  ]

data LspContext = LspContext { ctOperation :: String }

type LspMonad = ReaderT LspContext (LspM Config)

liftLSP :: LspM Config a -> LspMonad a
liftLSP = lift

runInContext :: String -> LspMonad a -> LspM Config a
runInContext operation action = runReaderT action (LspContext { ctOperation = operation })

withConnection :: (Connection -> LspMonad ()) -> LspMonad ()
withConnection act = do
  operation <- asks ctOperation
  cfg <- lift LSP.getConfig
  case cfConnection cfg of
    Just conn -> act conn
    Nothing -> sendError $ T.pack $ operation ++ " needs DB connection"

ensureFileLocation :: Uri -> (Either ResponseError a -> LspM Config ()) -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocation location responder action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    liftLSP $ responder $ Left $ responseError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

ensureFileLocation' :: Uri -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocation' location action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    sendError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

responseError m = ResponseError InvalidRequest m Nothing

sendMessage :: T.Text -> LspMonad ()
sendMessage = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtInfo

sendError :: T.Text -> LspMonad ()
sendError = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtError

logMessage :: T.Text -> LspMonad ()
logMessage = liftLSP . sendNotification SWindowLogMessage . LogMessageParams MtInfo

getRewrites :: FilePath -> LspMonad SourceDiffs
getRewrites fp = do
  cfg <- liftLSP LSP.getConfig
  return $ maybe Map.empty frDiffs $ Map.lookup fp $ fromMaybe Map.empty $ cfFileRecords cfg

modifyConfig :: (Config -> LspMonad Config) -> LspMonad Config
modifyConfig f = do
  config <- liftLSP LSP.getConfig
  f config >>= LSP.setConfig
  return config

modifyConfig' :: (Config -> Config) -> LspMonad Config
modifyConfig' f = modifyConfig (pure . f)

tryToConnectToDB :: LspMonad ()
tryToConnectToDB = do
  config <- liftLSP LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (cfPostgresqlConnectionString config))
  case connOrError of
    Right conn -> do 
      sendMessage "Connected to DB"
      modifiedDiffs <- liftIO $ query_ conn "SELECT filePath, modifiedFileDiffs FROM modules WHERE modifiedFileDiffs IS NOT NULL"
      let convertDiff (fp, diffs) = (fp, FileRecord $ deserializeSourceDiffs diffs)
      liftLSP $ LSP.setConfig $
        config 
          { cfConnection = Just conn
          , cfFileRecords = Just (Map.fromList $ map convertDiff modifiedDiffs) 
          }
    Left (e :: SomeException) -> return () -- error is OK

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = loadConfig
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) (liftIO)
  , options = hstoolsOptions
  , LSP.defaultConfig = Main.defaultConfig
  }

hstoolsOptions :: Options
hstoolsOptions = defaultOptions
  { executeCommandCommands = Just ["cleanDB"]
  , textDocumentSync = Just syncOptions
  }
  where
    syncOptions =
      TextDocumentSyncOptions 
        (Just True) -- openClose notification
        (Just TdSyncIncremental) -- change notification
        Nothing -- willSave notification
        Nothing -- willSave request
        (Just $ InL True) -- save notification

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

data FileRecord
  = FileRecord { frDiffs :: SourceDiffs }
  | OpenFileRecord 
    { frDiffs :: SourceDiffs
    , frCompiledContent :: FileLines
    , frCurrentContent :: FileLines
    }
  deriving (Show)

-- Nothing means it is not loaded yet from database
type FileRecords = Maybe (Map.Map FilePath FileRecord)

recordFileOpened :: Connection -> FilePath -> T.Text -> FileRecords -> LspMonad FileRecords
recordFileOpened _ _ _ Nothing = error "recordFileOpened: records are not initialized"
recordFileOpened conn fp content (Just frs)
  = updateRecord (Map.lookup fp frs) >>= \r -> return $ Just $ Map.alter (const (Just r)) fp frs
  where
    updateRecord Nothing = return $ OpenFileRecord Map.empty contentLines contentLines
    updateRecord (Just (FileRecord diffs)) = do
      compiledSource <- liftIO $ query conn "SELECT compiledSource FROM modules WHERE filePath = ?" (Only fp)
      case compiledSource of
        [[src]] -> return $ OpenFileRecord diffs (lines src) contentLines
        _ -> error $ "recordFileOpened: file " ++ fp ++ " should have been in the database"
    updateRecord (Just r) = return r
    contentLines = lines $ T.unpack content

recordFileClosed :: Connection -> FilePath -> FileRecords -> LspMonad FileRecords
recordFileClosed _ _ Nothing = error "recordFileClosed: records are not initialized"
recordFileClosed conn fp (Just frs)
  = updateRecord (Map.lookup fp frs) >>= \r -> return $ Just $ Map.insert fp r frs
  where
    updateRecord (Just (OpenFileRecord diffs compiledContent currentContent)) = do
      modifiedDiffs <- liftIO $ query conn "SELECT modifiedFileDiffs FROM modules WHERE filePath = ?" (Only fp)
      case modifiedDiffs of
        [[src]] -> return $ FileRecord $ Map.fromAscList $ map read $ lines src
        _ -> return $ FileRecord Map.empty
    updateRecord _ = error $ "recordFileClosed: file " ++ fp ++ " should have been on record"

updateSavedFileRecords :: FilePath -> [SourceRewrite] -> FileRecords -> FileRecords
updateSavedFileRecords _ _ Nothing = error "updateSavedFileRecords: records are not initialized"
updateSavedFileRecords fp newDiffs (Just frs) = Just $ Map.adjust updateDiffs fp frs
  where
    updateDiffs (OpenFileRecord diffs compiledContent currentContent) =
      let (currentContent', diffs') = foldr (addExtraChange compiledContent) (currentContent, diffs) newDiffs
      in OpenFileRecord diffs' compiledContent currentContent'
    updateDiffs _ = error $ "updateSavedFileRecords: file not open: " ++ fp

replaceSourceDiffs :: FilePath -> SourceDiffs -> FileRecords -> FileRecords
replaceSourceDiffs _ _ Nothing = error "replaceSourceDiffs: records are not initialized"
replaceSourceDiffs fp diffs (Just frs) = Just $ Map.adjust (\fr -> fr{frDiffs = diffs}) fp frs

isFileOpen :: FilePath -> FileRecords -> Bool
isFileOpen fp Nothing = error "isFileOpen: records are not initialized"
isFileOpen fp (Just frs) = case Map.lookup fp frs of
  Just OpenFileRecord{} -> True
  _ -> False

data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: Maybe Connection
  , cfOperation :: Maybe String
  , cfFileRecords :: FileRecords
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cfPostgresqlConnectionString = ""
  , cfConnection = Nothing
  , cfOperation = Nothing
  , cfFileRecords = Nothing
  }

instance Show Connection where
  show _ = "<Connection>"

textDocChangeToSD :: LSP.TextDocumentContentChangeEvent -> Maybe SourceRewrite
textDocChangeToSD (LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text)
  = Just $ SourceRewrite st (posToSP end) (T.unpack text)
  where st = posToSP start
textDocChangeToSD _ = Nothing

posToSP :: LSP.Position -> SP
posToSP (LSP.Position line char) = SP (fromIntegral line + 1) (fromIntegral char + 1)

spToPos :: SP -> LSP.Position
spToPos (SP line char) = LSP.Position (fromIntegral line - 1) (fromIntegral char - 1)

rangeToLSP :: SourceRange -> LSP.Range
rangeToLSP (SourceRange start end) = LSP.Range (spToPos start) (spToPos end)

lineToLoc :: SourceDiffs -> (String, Int, Int, Int, Int) -> Maybe LSP.Location
lineToLoc rewrites (file, startLine, startCol, endLine, endCol)
  = fmap (LSP.Location (filePathToUri file) . rangeToLSP) 
      $ originalToNewRangeStrict rewrites 
      $ SourceRange (SP startLine startCol) (SP endLine endCol)

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty "" = Nothing
nothingIfEmpty str = Just str

serializeSourceDiffs :: SourceDiffs -> String
serializeSourceDiffs = unlines . map show . Map.toAscList 

deserializeSourceDiffs :: String -> SourceDiffs
deserializeSourceDiffs str
  = Map.fromAscList $ map (\s -> fromMaybe (error $ "Can't deserialize source diff: " ++ show s) $ readMaybe s) $ lines str
