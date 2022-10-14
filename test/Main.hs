{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main ( main ) where

import Test.Framework
-- Import modules defining HTF tests like this:
import {-@ HTF_TESTS @-} LibSpec
import {-@ HTF_TESTS @-} X.Control.Parser.AST.Token.IdentifierSpec
import {-@ HTF_TESTS @-} X.Control.Parser.AST.ArithmeticExpressionSpec
import {-@ HTF_TESTS @-} X.Control.Parser.AST.AssignmentSpec
import {-@ HTF_TESTS @-} X.Control.Parser.AST.StatementSpec
import {-@ HTF_TESTS @-} X.Control.Parser.Combinator.Branch.CheckSpec
import {-@ HTF_TESTS @-} X.Control.ParserSpec
import {-@ HTF_TESTS @-} X.Control.TerminalSpec
import {-@ HTF_TESTS @-} X.Control.TrySpec
import {-@ HTF_TESTS @-} X.Data.LeftAssociativeInfixListSpec
import {-@ HTF_TESTS @-} X.Data.Value.SimplifySpec
import {-@ HTF_TESTS @-} X.Shell.MainSpec

main :: IO ()
main = htfMain htf_importedTests
