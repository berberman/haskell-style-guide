Haskell 风格指南
===================

本文是一个简短的文档，用于描述项目首选的代码风格。<!--
-->作者尝试涵盖主要的格式及命名，如果该指南中未出现<!--
-->某些内容时，你应该与其他模块的代码<!--
-->保持一致。

格式化
----------

### 行长

最大行长为*80 字符*。

### 缩进

Tab 是不合法的，应当使用空格。用 *4 个空格* 来缩进代码块。<!--
-->缩进 `where` 关键字两个空格，将其与<!--
-->代码的其余部分分开，并且<!--
-->缩进 `where` 子句定义四个空格。一些例子：

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

### 空行

顶层定义间应该空一行。<!--
-->函数类型签名和定义间不应该空行。<!--
-->类型类实例中的函数体如果很大，这些函数定义之间应该空行。<!--
-->具体情况自行判断~

### 空格

二元运算符的左右两侧都应空一格。<!--
-->对于算术运算符周围的空格可以自行判断，<!--
-->不过需要保证左右两边空格相等。<!--
-->不要在 lambda 后面插入空格。

### 数据声明

对齐数据类型定义中的构造器。举个例子：

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

对于很长的类型名，以下格式也可是以的：

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

记录语法应遵照如下格式：

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### 列表定义

对齐列表中的每个元素。举个例子：

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

你也可以跳过首行，具体情况自行判断。

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### 编译指示

将编译指示放在它们应用的函数之后。
例子：

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

数据类型定义时，编译指示需要放在类型之前。<!--
-->例子：

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### 悬挂 Lambda

在“悬挂”的 lambda 后的代码是否缩进均可，请自行判断。<!--
-->一些例子：

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### 导出清单：

按照如下格式化导出清单：

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

### If-then-else 子句

通常情况下，如果可能的话模式匹配和守卫模式应该优先考虑，而不是 if-then-else 子句。<!--
-->较短的分支通常应该放在一行<!--
-->（如果行长允许的话）。

当编写非单子风格代码（即没有用 `do`）并且没有用守卫模式或<!--
-->模式匹配时，你可以像正常表达式一样<!--
-->把 if-then-else 子句对齐：

```haskell
foo = if ...
      then ...
      else ...
```

否则，你应该与四空格缩进法则保持一致，并且<!--
-->`then` 和 `else` 关键字应该对齐。例子：

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

同样的规则适用于嵌套的 do 块：

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

### Case 表达式

case 表达式中的分支可以<!--
-->使用两种缩进风格的任何一种：

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

或者这样

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

对齐 `->` 箭头可以提高可读性。

导入
-------

导入应按以下顺序分组：

1. 标准库的导入
2. 相关第三方的导入
3. 本地应用程序/库的特定导入

在每组导入间插入一个空行。<!--
-->每组的导入应该按照模块名的字母顺序排序。

对于标准库和第三方的库应使用 `qualified` 导入，<!--
-->以便代码在库变动时更加健壮。<!--
-->例外：Prelude。

注释
--------

### 标点

书写正确的句子；以大写字母开头，<!--
-->使用正确标点。

### 顶层定义

对每个顶层函数进行注释（特别是要进行导出的），<!--
-->并且提供类型签名；在注释中使用 Haddock 句法。<!--
-->注释每个将要导出的数据类型。关于函数的例子：

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

对于函数，文档应给出足够的信息，以便在能够在不看函数的<!--
-->定义时使用函数。

关于记录语法的例子：

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

对于需要较长注释的字段，请按如下格式进行注释：

```haskell
data Record = Record
    { -- | 这是一个非常非常非常长的注释，
      -- 分成了许多行。
      field1 :: !Text

      -- | 这又是一个非常非常非常长的注释，
      -- 分成了许多行。
    , field2 :: !Int
    }
```

### 行尾注释

使用两个空格将行尾注释与代码分开。将数据类型的注释<!--
-->对齐。一些例子：

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### Links

Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy.  This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file.  Including this flag in the file itself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`).  If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

Misc
----

### Point-free style ###

Avoid over-using point-free style.  For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

### Warnings ###

Code should be compilable with `-Wall -Werror`.  There should be no
warnings.
