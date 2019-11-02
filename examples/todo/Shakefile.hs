{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.List.Extra
import Data.Typeable
import Development.Shake
import Development.Shake.FilePath
import qualified Distribution.PackageDescription.Parsec as D
import qualified Distribution.Pretty as D
import qualified Distribution.Types.GenericPackageDescription as D
import qualified Distribution.Types.PackageDescription as D
import qualified Distribution.Types.PackageId as D
import qualified Distribution.Verbosity as D
import System.Info.Extra

target :: String
target = "todo"

-- | Unfortunately, Cabal is missing programatic querying of build outputs
-- https://www.haskell.org/cabal/users-guide/nix-local-build.html#where-are-my-build-products
jsexePath :: String -> String -> String -> String -> String
jsexePath ghcjsVersion project projVersion tgt = "dist-newstyle/build/x86_64-"
    <> if isWindows then "windows" else "linux"
    <> "/ghcjs-" <> ghcjsVersion <> "/"
    <> project <> "-" <> projVersion <> "/x/"
    <> tgt <> "/build/"
    <> tgt <> "/"
    <> tgt <> ".jsexe"

-- | convert the jsexe all.js from ghcjs compilation into a node module
jsexeModule :: String -> String
jsexeModule str = "module.exports = function(){\n"
    <> "var global = globalThis;\n"
    <> str
    <> "\n}\n"

newtype GhcjsVersion = GhcjsVersion () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcjsVersion = String
oracleGhcjsVersion :: Rules (GhcjsVersion -> Action String)
oracleGhcjsVersion = addOracle $ \(GhcjsVersion _) -> (trim . fromStdout) <$> cmd "ghcjs --numeric-version" :: Action String

newtype ProjectName = ProjectName () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ProjectName = String
oracleProjectName :: Rules (ProjectName -> Action FilePath)
oracleProjectName = addOracle $ \(ProjectName _) -> do
    Stdout cs <- cmd "find . -maxdepth 1 -name *.cabal"
    let cabal' = case words cs of
            [cabal] -> cabal
            cs' -> error "Didn't find unique cabal file. Found: " <> show cs'
    pure $ dropExtension cabal'

newtype ProjectVersion = ProjectVersion String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ProjectVersion = String
oracleProjectVersion :: Rules (ProjectVersion -> Action String)
oracleProjectVersion = addOracle $ \(ProjectVersion project) -> do
    desc <- liftIO $ D.readGenericPackageDescription D.normal $ project <.> "cabal"
    pure $ D.prettyShow $ D.pkgVersion $ D.package $ D.packageDescription desc

main :: IO ()
main = shakeArgs shakeOptions $ do

    phony "clean" $ do
        putNormal $ "Deleting " <> build
        removeFilesAfter build ["//"]

    want [build </> "hsMain.js", "node_modules/.npm_install.done" ]

    npm_install <- newCache $ \() -> cmd_ "npm install"

    "node_modules/*/package.json" %> \_ -> npm_install ()

    "node_modules/.npm_install.done" %> \_ -> do
        npm_install ()
        ls <- liftIO $ getDirectoryFilesIO "." ["node_modules/*/package.json"]
        needed ls
        copyFile' "package.json" "node_modules/.npm_install.done"

    getGhcjsVersion <- oracleGhcjsVersion
    getProjectName <- oracleProjectName
    getProjectVersion <- oracleProjectVersion
    let getProjectJsexe = do
            ghcVer <- getGhcjsVersion $ GhcjsVersion ()
            proj <- getProjectName $ ProjectName ()
            projectVer <- getProjectVersion $ ProjectVersion proj
            pure $ jsexePath ghcVer proj projectVer target

    build </> "hsMain.js" %> \out -> do
        jsexe <- getProjectJsexe
        let alljs = jsexe </> "all.js"
        allsrc <- readFile' alljs
        writeFile' out $ jsexeModule allsrc

    jsexePath "*" "*" "*" target </> "all.js" %> \_ -> do
         alwaysRerun
         cmd_ "cabal" ["--ghcjs", "v2-build", target]

  where
    build = shakeFiles shakeOptions
