-- декларация модуля

-- | This module is in file ./Prefix1/Prefix2/Name.(hs|lhs|chs|hsc)
module Prefix1.Prefix2.Name where... -- экспортируется всё, что определено в этом модуле

-------------------------------------------------------------------

module Name
    ( exported1
    , exported2
    ) where...

-------------------------------------------------------------------

module Name
    ( Email -- только тип

    , Email (Email) -- тип и конструктор
    , Either (Left) -- тип и только 1 конструктор
    , Creature (Creature, position) -- только тип и указанные элементы

    -- экспортировать конструктор без типа нельзя

    , Email (..)
    , Creature (..) -- re-export
    , position -- геттер и ...

    , module Haskarium.Types -- re-export всех импортированных сюда символов оттуда

    ) where

import Haskarium.Types (Creature (..))

newtype Email = Email Text

data Creature species = Creature
    { position :: !Point
    , targetDir :: !Angle
    , currentDir :: !Angle
    , turnRate :: !RadiansPerSecond
    , species :: !species
    , size :: !Distance
    }

-------------------------------------------------------------------

-- | Extra module pattern
module Foo.Extra where
    ( module Foo
    , module Foo.Extra
    , Qux.bla
    ) where

import Foo
import qualified Qux

bar1, bar2, bar3 :: Bar

--------------------------------------------------------------------

import Foo -- всё содержимое
foo
Foo.foo

import Foo (baz)
-- foo      -- error
-- Foo.foo  -- error

import qualified Foo
-- foo      -- error
Foo.foo

import Data.Graph.Algorithm.Foo
Data.Graph.Algorithm.Foo.foo -- ok

import Data.Graph.Algorithm.Foo as Foo
Foo.foo -- ok

--------------------------------------------------------------------

module Imports (module X) where

import Data.Foo as X
import Data.Bar as X
import Data.Baz as X

--------------------------------------------------------------------

module Name
    ( Email -- только тип
    ) where

import Haskarium.Types (Creature (..))

newtype Email = Email Text

makeEmail :: Text -> Maybe Email
makeEmail t | "@" `isInfixOf` t = Just $ Email t
            | otherwise         = Nothing

toText :: Email -> Text
toText (Email t) = t

--------------------------------------------------------------------

data Set a
  = _.Bin Size a (Set a) (Set a)
  | _.Tip

fromList :: Ord a => [a] -> Set a

--------------------------------------------------------------------

data Config = Config
    { path    :: FilePath
    , useKey  :: Maybe Text
    }

simpleConfig :: FilePath -> Config
simpleConfig path = Config
    { path
    , useKey = Nothing
    }

realConfig1 = simpleConfig "./foo"
realConfig2 = (simpleConfig "./foo"){useKey = Just "key"}
