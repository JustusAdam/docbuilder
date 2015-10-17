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
import           Data.List
import           Data.Maybe                    (isJust)
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Traversable
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


findCompiler file = find (($ file) . eligibilityTest) compilers


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
    case findCompiler file of
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
          indexPath = docPath </> "index.html"
          serve path' = respond $
                          responseFile
                            ok200
                            [(hContentType, defaultMimeLookup path)]
                            path'
                            Nothing
      if isAsset
        then serve spath
        else do
          isFile <- doesFileExist docPath
          isDir <- (&&) <$> doesDirectoryExist docPath <*> doesFileExist indexPath
          if
            | isFile -> serve docPath
            | isDir -> serve indexPath
            | otherwise -> respond $ responseLBS notFound404 [] "File not Found"


watch :: DocbuilderOpts -> IO () -> IO ()
watch opts inner = do
  cwd <- getCurrentDirectory
  F.withManager $ \manager -> do
    for (filter (/= buildDir) $ sourceFolders opts) $ \dir ->
      watchTree
        manager
        dir
        isCompileable
        (go cwd)
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
        Just (Compiler { compilerName, invocation, newExtension }) -> do
          putStrLn $ "Recompiling " <> compilerName <> ": " <> path
          invocation opts (cwd </> path) (cwd </> buildDir </> path -<.> newExtension)
        Nothing -> putStrLn $ "No compiler found for " <> path



main :: IO ()
main = cmdArgs docArgs >>= ($) <$> for_ . commands <*> switch
  where
    switch _ "clean" = cleanBuildDir
    switch a "compile" = compile a
    switch a "watch" = watch a (forever (threadDelay 1000000))
    switch a@(DocbuilderOpts { port }) "serve" = compile a >> watch a (serve port)
    switch _ a = putStrLn $ "Unrecognized command " <> a
