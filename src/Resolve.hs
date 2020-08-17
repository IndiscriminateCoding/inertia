module Resolve where

import Ast

resolve :: Host -> IO Host
resolve h@(Static _ _) = pure h
resolve (Dns h p) = pure (Static undefined p)
resolve (DnsSrv name) = undefined
