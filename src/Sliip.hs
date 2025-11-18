{-|
Module      : Sliip
Description : Main module for the Sliip Lisp interpreter
Copyright   : (c) Sliip Contributors
License     : MIT
Maintainer  : haruki7049
Stability   : experimental

This is the main entry point module for the Sliip Lisp interpreter.
It re-exports the parser and evaluator functionality.

Sliip is a joke Lisp interpreter written in Haskell, providing
basic Lisp functionality including:

* S-expression parsing
* Lambda functions
* Define forms
* Let bindings (let, let*, letrec)
* Conditional expressions (if)
* Pattern matching
* Type annotations
-}
module Sliip where
