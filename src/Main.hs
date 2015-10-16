{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Text.Pandoc as P
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types.Status
import Network.Mime
import Network.HTTP.Types.Header
import Network.Wai
import System.Console.CmdArgs
import Data.List
import Control.Monad
import Data.Traversable
import Text.Mustache (localAutomaticCompile, substitute, (~>), object)


data Compiler = Compiler { compilerName    :: String
                         , eligibilityTest :: FilePath -> Bool
                         , invocation      :: DocbuilderOpts -> FilePath -> FilePath -> IO ()
                         , newExtension    :: String
                         }


data DocbuilderOpts = DocbuilderOpts { commands :: [String]
                                     , sourceFolders :: [FilePath]
                                     , port :: Int
                                     , pandocTemplate :: Maybe String
                                     } deriving (Data)


docArgs = DocbuilderOpts
  { commands = def &= args
  , sourceFolders = def
  , port = 8080 &= name "p"
  , pandocTemplate = def &= name "t"
  }



buildDir :: String
buildDir = "build"

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


asciidoctor :: DocbuilderOpts -> String -> String -> IO ()
asciidoctor _ in' out = callProcess "asciidoctor" ["-o", out, in']


markdown :: DocbuilderOpts -> String -> FilePath -> IO ()
markdown (DocbuilderOpts { pandocTemplate }) in' out = do
  rawFile <- readFile in'
  case P.readMarkdown P.def rawFile of
    Left err -> print err
    Right pandoc -> do
      let go template =
            let
              writerOptions = P.def { P.writerStandalone = True, P.writerTemplate = template }
              document = P.writeHtml writerOptions pandoc
            in
              L.writeFile out $ renderHtml document
      case pandocTemplate of
        Nothing ->
          P.getDefaultTemplate mempty "html5" >>= \case
            Left err -> print err
            Right template -> go template
        Just tName -> readFile tName >>= go


discoverTargets :: DocbuilderOpts -> FilePath -> FilePath -> IO [(FilePath, Compiler)]
discoverTargets opts sourceDir outDir = do
  files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents sourceDir
  l <- for files $ \file -> do
    isDir <- doesDirectoryExist (sourceDir </> file)
    case find (($ file) . eligibilityTest) compilers of
      Just c@(Compiler { compilerName, invocation }) -> do
        let sourceFile = sourceDir </> file
        return [(sourceFile, c)]
      Nothing | isDir -> discoverTargets opts (sourceDir </> file) (outDir </> file)
      _ -> return mzero
  return $ join l


compile :: DocbuilderOpts -> IO ()
compile opts = do
  files <- join <$> traverse (\dir -> discoverTargets opts dir (buildDir </> dir)) (sourceFolders opts)
  createDirectoryIfMissing True buildDir
  indexTemplate <- localAutomaticCompile "index.html"
  case indexTemplate of
    Left err -> print err
    Right t ->
      TIO.writeFile "build/index.html" $ substitute t
        $ object ["documents" ~> map (\(a, Compiler {newExtension}) -> "/" <> a -<.> newExtension) files]

  for_ files $ \(source, Compiler { newExtension , invocation, compilerName }) -> do
    let target = buildDir </> source -<.> newExtension
    needsRecompile <- do
      targetExists <- doesFileExist target
      if targetExists
        then
          (>) <$> getModificationTime source <*> getModificationTime target
        else return True
    if needsRecompile
      then do
        createDirectoryIfMissing True (takeDirectory target)
        putStrLn $ compileMessage compilerName source target
        invocation opts source target
      else putStrLn $ "Skipping " <> source <> " (no update required)."



cleanBuildDir :: IO ()
cleanBuildDir = doesDirectoryExist buildDir >>= flip when (removeDirectoryRecursive buildDir)


serve :: Int -> IO ()
serve = flip run app
  where
    app request respond = do
      let path = T.intercalate "/" $ pathInfo request
          spath = T.unpack path
          docPath = "build" </> spath
          isAsset = "assets/" `isPrefixOf` spath
          serve path' = respond $
                          responseFile
                            ok200
                            [(hContentType, defaultMimeLookup path)]
                            path'
                            Nothing
      if isAsset
        then serve spath
        else do
          exists <- doesFileExist docPath
          if exists
            then serve docPath
            else respond $ responseLBS notFound404 [] "File not Found"


main :: IO ()
main = cmdArgs docArgs >>= ($) <$> for_ . commands <*> switch
  where
    switch _ "clean" = cleanBuildDir
    switch a "compile" = compile a
    switch (DocbuilderOpts { port }) "serve" = serve port
    switch _ a = putStrLn $ "Unrecognized command " <> a