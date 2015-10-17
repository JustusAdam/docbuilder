{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where


import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy          as L (writeFile)
import           Data.Char
import           Data.Foldable
import           Data.Function                 (on)
import           Data.List
import           Data.Maybe                    (isJust)
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Traversable
import           Debug.Trace
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Mime
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FSNotify               as F
import           System.IO                     (hPutStrLn, stderr)
import           System.Process
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Mustache                 (localAutomaticCompile, object,
                                                substitute, (~>))
import qualified Text.Pandoc                   as P


data Compiler = Compiler { compilerName    :: String
                         , eligibilityTest :: FilePath -> Bool
                         , invocation      :: DocbuilderOpts -> FilePath -> FilePath -> IO ()
                         , newExtension    :: String
                         }


data DocbuilderOpts = DocbuilderOpts { commands       :: [String]
                                     , sourceFolders  :: [FilePath]
                                     , port           :: Int
                                     , pandocTemplate :: Maybe String
                                     , indexTemplate  :: Maybe String
                                     } deriving (Data)


docArgs :: DocbuilderOpts
docArgs = DocbuilderOpts
  { commands = def &= args
  , sourceFolders = def &= typ "DIRECTORY"
  , port = 8080 &= name "p" &= typ "INT"
  , pandocTemplate = def &= name "t" &= typ "FILE.html"
  , indexTemplate = def &= name "i" &= typ "FILE.html"
  }


logm :: String -> IO ()
logm = hPutStrLn stderr
logsm :: Show s => s -> IO ()
logsm = logm . show


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


findCompiler :: FilePath -> Maybe Compiler
findCompiler file = find (($ file) . eligibilityTest) compilers


makeTargetName :: Compiler -> FilePath -> FilePath
makeTargetName (Compiler { newExtension }) = (buildDir </>) . (-<.> newExtension)


compileMessage :: String -> String -> String -> String
compileMessage compiler from to =
  "Compiling " <> compiler <> ": " <> from <> " -> " <> to


asciidoctor :: DocbuilderOpts -> String -> String -> IO ()
asciidoctor _ in' out = callProcess "asciidoctor" ["-o", out, in']


markdown :: DocbuilderOpts -> String -> FilePath -> IO ()
markdown (DocbuilderOpts { pandocTemplate }) in' out = do
  rawFile <- readFile in'
  case P.readMarkdown P.def rawFile of
    Left err -> logsm err
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
            Left err -> logsm err
            Right template -> go template
        Just tName -> readFile tName >>= go


discoverTargets :: DocbuilderOpts -> FilePath -> FilePath -> IO [(FilePath, Compiler)]
discoverTargets opts sourceDir outDir = do
  files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents sourceDir
  l <- for files $ \file -> do
    isDir <- doesDirectoryExist (sourceDir </> file)
    case findCompiler file of
      Just c@(Compiler { compilerName, invocation }) -> do
        let sourceFile = sourceDir </> file
        return [(sourceFile, c)]
      Nothing | isDir -> discoverTargets opts (sourceDir </> file) (outDir </> file)
      _ -> return mzero
  return $ join l


scanDirectories :: DocbuilderOpts -> IO [(FilePath, Compiler)]
scanDirectories opts = join <$> traverse (\dir -> discoverTargets opts dir (buildDir </> dir)) (sourceFolders opts)


compile :: DocbuilderOpts -> IO ()
compile opts = do
  files <- scanDirectories opts
  createDirectoryIfMissing True buildDir

  makeIndex opts files

  for_ files $ \(source, c@(Compiler { invocation, compilerName })) -> do
    let target = makeTargetName c source
    needsRecompile <- do
      targetExists <- doesFileExist target
      if targetExists
        then
          (>) <$> getModificationTime source <*> getModificationTime target
        else return True
    if needsRecompile
      then do
        createDirectoryIfMissing True (takeDirectory target)
        logm $ compileMessage compilerName source target
        invocation opts source target
      else logm $ "Skipping " <> source <> " (no update required)."


makeIndex :: DocbuilderOpts -> [(FilePath, Compiler)] -> IO ()
makeIndex = maybe (const $ return ()) makeIndex' . indexTemplate
  where
    makeIndex' name files = do
      indexTemplate <- localAutomaticCompile name
      case indexTemplate of
        Left err -> logsm err
        Right t -> do
          logm $ "Building index with template " <> name
          TIO.writeFile "build/index.html" $ substitute t
            $ object
              [ "documents" ~> (sortBy (compare `on` length) . map (\(name, Compiler { newExtension }) -> name -<.> newExtension)) files
              ]


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
          indexPath = docPath </> "index.html"
          serveWithMime mime path' = respond $
            responseFile ok200 [(hContentType, mime)] path' Nothing
          serve = serveWithMime (defaultMimeLookup path)
      if isAsset
        then serve spath
        else do
          isFile <- doesFileExist docPath
          isDir <- (&&) <$> doesDirectoryExist docPath <*> doesFileExist indexPath
          if
            | isFile -> serve docPath
            | isDir -> serveWithMime (defaultMimeLookup "index.html") indexPath
            | otherwise -> respond $ responseLBS notFound404 [] "File not Found"


watch :: DocbuilderOpts -> IO () -> IO ()
watch opts inner = do
  cwd <- getCurrentDirectory
  F.withManager $ \manager -> do
    for (filter (/= buildDir) $ sourceFolders opts) $ \dir ->
      watchTree manager dir isCompileable (go cwd)
    inner
  where
    isCompileable = isJust . findCompiler . eventPath
    go cwd (Removed p _) = do
      let path = cwd </> buildDir </> makeRelative cwd p
      exists <- doesFileExist path
      when exists $ removeFile path
    go cwd e = do
      let path = makeRelative cwd $ eventPath e
      case findCompiler path of
        Just c@(Compiler { compilerName, invocation }) -> do
          logm $ "Recompiling " <> compilerName <> ": " <> path
          invocation opts (cwd </> path) (cwd </> makeTargetName c path)
          case e of
            (Added _ _) -> scanDirectories opts >>= makeIndex opts
            _ -> return ()
        Nothing -> logm $ "No compiler found for " <> path



main :: IO ()
main = cmdArgs docArgs >>= ($) <$> for_ . commands <*> switch
  where
    switch _ "clean" = cleanBuildDir
    switch a "compile" = compile a
    switch a "watch" = watch a (forever (threadDelay 1000000))
    switch a@(DocbuilderOpts { port }) "serve" = compile a >> watch a (serve port)
    switch _ a = logm $ "Unrecognized command " <> a
