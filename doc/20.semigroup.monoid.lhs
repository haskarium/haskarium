Классы `Semigroup` и `Monoid` реализуют концепцию
«взять много штук и слепить в одну штуку».

Рассмотрим функцию `concat`:

> -- | Взять много списков и склеить в один список.
> concat :: [[a]] -> [a]
> concat = foldr (++) []

Эта функция работает с любыми списками, даже со `String`,
потому что `String` на самом деле тоже списки.

< type String = [Char]

Но с `Text` она работать не будет, потому что (++) умеет сцеплять только списки.

Нам нужно обобщить (++) на `Text`.
Более того, её можно обобщить на любой тип с ассоциативной функцией.

В математике множество с ассоциативной функцией называется полугруппой
(semigroup).
В Хаскелле это вот такой класс:

> -- associativity:
> --   ∀ x y z. x <> (y <> z) == (x <> y) <> z
> class Semigroup a where
>     (<>) :: a -> a -> a

Чтобы называться полугруппой,
тип должен предоставить реализацию функции (<>) (ромбик, diamond, append).
Кроме того, функция должна быть ассоциативной.
К сожалению,
компилятор в общем случае не может проверить свойство ассоциативности.
Эта обязанность перекладывается на программистов и рецензентов их кода.

Известные полугруппы:

> instance Semigroup [a]  where  (<>)  = (++)
> instance Semigroup Text where  (<>)  = Text.append
> instance Semigroup ()   where _ <> _ = ()

С числами чуть сложнее.
Математики хорошо знают, что числа образуют полугуппу и по сложению,
и по умножению.
Но хаскеллисты выбрали такой интерфейс класса `Semigroup`,
который позволяет для одного типа определить только ассоциативную операцию.
Не беда! Создадим по типу-обёртке на каждую ассоциативную операцию.

> -- instance Semigroup Int -- no way
> newtype Sum     a = Sum     a
> newtype Product a = Product a
> instance Num a => Semigroup (Sum a) where Sum x <> Sum     y = Sum     (x + y)
> instance Num a => Semigroup (Product a) where
>                                       Product x <> Product y = Product (x * y)

Аналогичная ситуация с минимумом и максимумом

> -- instance Semigroup Int -- no way
> newtype Min a = Min a
> newtype Max a = Max a
> instance Ord a => Semigroup (Min a) where Min x <> Min y = Min (min x y)
> instance Ord a => Semigroup (Max a) where Max x <> Max y = Max (max x y)

А также с конъюнкцией и дизъюнкцией

> -- instance Semigroup Bool -- no way
> newtype All = All Bool
> newtype Any = Any Bool
> instance Semigroup All where All x <> All y = All (x && y)
> instance Semigroup Any where Any x <> Any y = Any (x || y)

Больше полугрупп можно увидеть в GHCi, введя команды

< import Data.Semigroup
< :info Semigroup

Имея экземпляр класса `Semigroup` с функцией (<>),
мы можем обобщить `concat` следующим образом:

< -- | Semigroup concat
< sconcat :: Semigroup a => [a] -> a
< sconcat (x : xs) = x <> sconcat xs
< sconcat [] = ...

Упс! Мы не можем реализовать `sconcat` для случая пустого списка!
Хвала богам, мы можем создать тип, элементы которого — только непустые списки
(точнее, объекты, изоморфные непустым спискам).

> data NonEmpty a = a :| [a]
>
> -- | Semigroup concat
> sconcat :: Semigroup a => NonEmpty a -> a
> sconcat (x :|   []  ) = x
> sconcat (x :| y : ys) = x <> sconcat (y :| ys)

Например, можно вычислить минимум по набору данных таким образом:

> λ> sconcat $ NonEmpty.map Min $ 3 :| [15, 9, 20]
> Min 3

Возможно, это не самый красивый способ, мы рассмотрим другие способы далее,
но выгода уже видна — нам не пришлось писать отдельные функции для минимума,
максимума, суммы, произведения и т. д. всех элементов списка,
оказалось достаточно написать такие функции только для 2 элементов
и функцию `sconcat`,
распространяющую их на список.

Увы, обобщить `concat` на `[Text]` нам пока не удалось.
Осталось разобрать случай с пустым списком.

Для этого существует ещё одна математическая абстакция —
полугруппа с единицей, или моноид.
В математике это — полугруппа с нейтральным (единичным) элементом,
обладающим свойством идентичности (левой и правой).

В Хаскелле мы пишем ещё один класс:

> -- identity:
> --   ∀ x. mempty <> x == x
> --   ∀ x. x <> mempty == x
> class Semigroup a => Monoid a where
>     mempty :: a

(Определение в стандартной библиотеке отличается в силу исторических причин.)

Известные моноиды:

> instance          Semigroup [a]         where mempty = []
> instance          Semigroup Text        where mempty = Text.empty
> instance          Semigroup ()          where mempty = ()
> instance Num a => Semigroup (Sum     a) where mempty = Sum 0
> instance Num a => Semigroup (Product a) where mempty = Product 1
> instance          Semigroup All         where mempty = All True
> instance          Semigroup Any         where mempty = Any False

`Min` и `Max` при тех условиях, что сформулированы выше, не будут моноидами,
поскольку чтобы предоставить элемент меньше (больше) всех остальных в типе,
необходимо ввести дополнительные ограничения.

Больше моноидов можно увидеть в GHCi, введя команды

< :info Monoid

Наконец, мы можем написать обобщение `concat` на произвольный список объектов
типа `Text`, а также произвольного типа, обладающего свойствами моноида.

> -- | Monoid concat
> mconcat :: Monoid a => [a] -> a
> mconcat = foldr (<>) mempty
