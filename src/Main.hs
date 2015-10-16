{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Data.ByteString.Lazy          as L (writeFile)
import           Data.Char
import           Data.Foldable
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Pandoc
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Network.Mime
import Network.HTTP.Types.Header
import Network.Wai


data Compiler = Compiler { compilerName    :: String
                         , eligibilityTest :: FilePath -> Bool
                         , invocation      :: FilePath -> FilePath -> IO ()
                         , newExtension    :: String
                         }


buildDir :: String
buildDir = "build"


sourceDirs :: [FilePath]
sourceDirs =
  [ "ws15-16"
  , "ss15"
  ]


compilers :: [Compiler]
compilers =
  [ Compiler
      "Asciidoc"
      ((== ".adoc") . takeExtension)
      asciidoctor
      "html"
  , Compiler
      "Markdown"
      ((`elem` [".md", ".markdown"]) . takeExtension)
      markdown
      "html"
  ]


compileMessage :: String -> String -> String -> String
compileMessage compiler from to =
  "Compiling " <> compiler <> ": " <> from <> " -> " <> to


asciidoctor :: String -> String -> IO ()
asciidoctor in' out = callProcess "asciidoctor" ["-o", out, in']


markdown :: String -> FilePath -> IO ()
markdown in' out = do
  rawFile <- readFile in'
  case readMarkdown def rawFile of
    Left err -> print err
    Right pandoc ->
      getDefaultTemplate mempty "html5" >>= \case
        Left err -> print err
        Right template ->
          let
            writerOptions = def { writerStandalone = True, writerTemplate = template }
            document = writeHtml writerOptions pandoc
          in
            L.writeFile out $ renderHtml document


compile :: FilePath -> FilePath -> IO ()
compile sourceDir outDir = do
  files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents sourceDir
  for_ files $ \file -> do
    isDir <- doesDirectoryExist (sourceDir </> file)
    case find (($ file) . eligibilityTest) compilers of
      Just (Compiler { compilerName, newExtension, invocation }) -> do
        let sourceFile = sourceDir </> file
            targetFile = outDir </> file -<.> newExtension
        needsRecompile <- do
          targetExists <- doesFileExist targetFile
          if targetExists
            then
              (>) <$> getModificationTime sourceFile <*> getModificationTime targetFile
            else return True
        if needsRecompile
          then do
            createDirectoryIfMissing True outDir
            putStrLn $ compileMessage compilerName sourceFile targetFile
            invocation sourceFile targetFile
          else putStrLn $ "Skipping " <> sourceFile <> " (no update required)."
      Nothing | isDir -> compile (sourceDir </> file) (outDir </> file)
      _ -> return ()


cleanBuildDir :: IO ()
cleanBuildDir = removeDirectoryRecursive buildDir


compileDir :: FilePath -> IO ()
compileDir sourceDir = compile sourceDir (buildDir </> sourceDir)


serve :: IO ()
serve = run 8080 app
  where
    app request respond = do
      let path = T.intercalate "/" $ pathInfo request
      respond $
        responseFile
          ok200
          [(hContentType, defaultMimeLookup path)]
          (T.unpack path)
          Nothing


main :: IO ()
main =
  getArgs >>= traverse_ (switch . map toLower)
  where
    switch "clean" = cleanBuildDir
    switch "compile" = traverse_ compileDir sourceDirs
    switch "serve" = serve
    switch a = putStrLn $ "Unrecognized command " <> a
