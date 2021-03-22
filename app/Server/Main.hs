{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import           Language.LSP.Diagnostics (partitionBySource)
import           Language.LSP.Server
import           Language.LSP.Types

changeHandler :: Handlers (LspM ())
changeHandler = notificationHandler STextDocumentDidChange $ \msg -> do
  let NotificationMessage
        text _
        (DidChangeTextDocumentParams
          (VersionedTextDocumentIdentifier uri ver) _changes)
        = msg

  -- TODO: try to parse and get errors

  let message =
        Diagnostic
          (Range (Position 0 1) (Position 0 6))
          (Just DsError)
          Nothing
          (Just "banana-lsp")
          "Example error message"
          Nothing
          (Just (List []))
  publishDiagnostics 100 (toNormalizedUri uri) ver (partitionBySource [message])

hoverHandler :: Handlers f
hoverHandler = requestHandler STextDocumentHover $ \req responder -> do
  let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
      Position _l _c' = pos
      rsp = Hover ms (Just range)
      ms = HoverContents $ markedUpContent "banana-lsp" "I am a hover message!"
      range = Range pos pos
  responder (Right $ Just rsp)


handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_notification -> pure ()
  , changeHandler
  , hoverHandler
  -- , requestHandler STextDocumentDocumentSymbol $ \req responder -> do
  --     let DocumentSymbolParams _ _ doc = req ^. params
  --         loc = Location (doc ^. uri) (Range (Position 0 0) (Position 0 0))
  --         sym = SymbolInformation "lsp-hello" SkFunction Nothing Nothing loc Nothing
  --         rsp = InR (List [sym])
  --     responder (Right rsp)
  ]

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = const . pure . Right
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions {serverInfo = Just (ServerInfo "banana-lsp" (Just "v1"))}
  }
