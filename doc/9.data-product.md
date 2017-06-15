# Тип-произведение

## Определение

В Хаскелле очень легко создавать типы на основе существующих
(и даже вообще из ничего).

```haskell
data ИмяТипа = Конструктор типы полей через пробел
```

Пример:

```haskell
data Point = Point Float Float
-- тип Point состоит из значений Point с двумя позиционными полями Float

data JsonObjectItem = JsonObjectItem Text JsonValue
-- каждое значение типа JsonObjectItem — это
-- символ JsonObjectItem,
-- значение типа Text и
-- значение типа JsonValue
```

Пользовательские значения состоят из специального символа
(конструктора значения) и нескольких полей.

Определение типа создаёт новый тип (конструктор типа)
и новый конструктор значений.

Имена обоих конструкторов (типа и значения) должны начинаться с большой буквы.

Имена конструктора типов и конструктора значений могут совпадать и часто
совпадают, потому что пространства типов и значений не пересекаются.
(Они могут пересекаться при использовании разных ухищрений,
но в первом приближении можно считать, что не пересекаются.)

Полей может не быть совсем.

```haskell
data Unit = Unit
```

Значение этого типа состоит только из конструктора,
поэтому у этого типа может быть только одно значение — Unit.

Типы, населённый ровно одним значением, — настолько интересная вещь,
что для одного из них, определённого в стандартной библиотеке,
даже придумали свой синтаксис:

```haskell
data () = () -- читается «юнит»
```

## Создание значения

```haskell
data JsonObjectItem = JsonObjectItem Text JsonValue
data JsonValue = JsonNumber Decimal

object =
    [ JsonObjectItem "width"  (JsonNumber 800)
    , JsonObjectItem "height" (JsonNumber 600)
    ]
```

## Разбор значения (паттернматчинг)

```haskell
printObjectItem :: JsonObjectItem -> IO ()
printObjectItem (JsonObjectItem key value) = do
    print key
    print value
```

## Значение-запись (record)

Когда полей больше одного, их лучше именовать.

```haskell
data Ant = Ant Point Angle
-- превращаем в
data Ant = Ant{position :: Point, direction :: Angle}

-- Рекомендуется перед типами добавлять знак строгости.
-- Это чаще помогает, чем мешает.
data Ant = Ant{position :: !Point, direction :: !Angle}
```

Создавать и разбирать такие значения ещё проще.

## Создание значения-записи

```haskell
ant = Ant{direction = 3, position = (1, 3)}
```

Порядок полей не имеет значения.

## Разбор значения-записи (паттернматчинг)

```haskell
move :: Float -> Ant -> Ant
move dt Ant{position = (x, y), direction = dir} =
    Ant{position = (x + dx, y + dy), direction = dir}
  where
    dx = speed * dt * cos dir
    dy = speed * dt * sin dir
```

Порядок полей при разборе тоже не имеет значения.

## Обновление значения-записи

Когда надо не с нуля создать значение, а на основе существующего,
заменив только некоторые поля, используют синтаксис обновления
(record update syntax).

```haskell
move :: Float -> Ant -> Ant
move dt ant@Ant{position = (x, y), direction = dir} =
    ant{position = (x + dx, y + dy)}
  where
    dx = speed * dt * cos dir
    dy = speed * dt * sin dir
```
