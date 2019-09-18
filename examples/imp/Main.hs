{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 

module Main where

import Control.Monad.Identity ( runIdentity )

import qualified Language.Imp        as Imp ( parser, printer )
import qualified Language.Imp.Syntax as Orig

import Data.Comp.Multi
import Data.Comp.Multi.Strategic ( Rewrite, GRewrite, allbuR, promoteR, addFail )

import qualified Cubix.Language.Imp.Parametric.Common.Types as Common
import qualified Cubix.Language.Imp.Parametric.Common.Trans as Common
import Cubix.Language.Imp.Parametric.Full.Trans
import Cubix.Language.Imp.Parametric.Full.Types
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import Data.Monoid

parser :: FilePath -> IO (Maybe (ImpTerm CompUnitL))
parser path = do
  contents <- TIO.readFile path
  case Imp.parser contents of
    Left errors -> print errors >> return Nothing
    Right tree  -> return $ Just $ translate tree


pretty :: ImpTerm CompUnitL -> String
pretty = T.unpack . Imp.printer . untranslate

pattern PIdent s <- (project -> (Just (Ident s)))

vandalize' :: Rewrite ImpTerm IdentL
vandalize' (PIdent s) = return $ iIdent (s <> "_foo")

vandalize :: GRewrite ImpTerm
vandalize = allbuR $ promoteR $ addFail vandalize'

main = do
  Just tree <- parser "/home/sreenidhi/Work/cubix/langs/imp/examples/first.imp"
  let tree' = Common.translate tree
  print tree'
  putStrLn $ pretty $ runIdentity $ vandalize tree
