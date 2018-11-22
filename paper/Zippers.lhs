%if False
\begin{code}
  -- Rather standard extensions
  {-# LANGUAGE BangPatterns #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE MultiParamTypeClasses #-}
  {-# LANGUAGE NoMonomorphismRestriction #-}
  {-# LANGUAGE StandaloneDeriving #-}
  {-# LANGUAGE UnicodeSyntax #-}
  -- Slightly more advanced, but also very well known
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE RankNTypes #-}
  -- Newer extensions
  {-# LANGUAGE TypeApplications #-} -- GHC8.0
  {-# LANGUAGE TypeOperators #-} -- TODO: When introduced?
  -- and a bit more advanced
  {-# LANGUAGE ConstraintKinds #-} -- GHC7.4.1
  {-# LANGUAGE TypeInType #-} -- GHC8.0.1
  -- Debatable extensions
  {-# LANGUAGE UndecidableInstances #-}

  module Zippers
    ( modify
    , modifyExample

    , ListZipper(..)
    , ListContext(..)
    , lzLeft
    , lzRight
    , lzEnter
    , lzLeave
    , lzModify

    , Tree(..)
    , TreeZipper(..)
    , TreeContext(..)
    , tzDown
    , tzUp
    , tzLeave

    , Dissectible(..)
    , Left(..)
    , Right(..)
    , LocalContext(..)
    , Context(..)
    , Zipper(..)

    , modifyTest
    , lzLeftTest
    , lzRightTest
    , tzDownTest
    , tzUpTest
    ) where

  import           Prelude.Unicode
  import           Control.Exception              ( evaluate )
  import           Control.Monad                  ( (>=>) )
  import           Data.Kind
  import           Test.Hspec                     ( SpecWith
                                                  , shouldBe
                                                  , shouldThrow
                                                  , describe
                                                  , it
                                                  , anyException
                                                  )

\end{code}
%endif

\section{Functional Zippers}
  Zipper is a data structure commonly used in functional programming for
  traversal with fast local updates. The zipper data structure was originally
  conceived by Huet\cite{huet1997zipper} in the context of trees. We will,
  however, first consider a simpler problem: a bidirectional list traversal.

  \paragraph{Lists} Suppose that we would like to update a list at a specific
  position:
\begin{code}
  modify :: (a -> [a]) -> Int -> [a] -> [a]
  modify f i xs = helper [] xs 0
   where helper before (x : after) !j
           | j == i    = before ++ f x ++ after
           | otherwise = helper (before ++ [x]) after (j + 1)
         helper _ [] _ = error "Index out of bounds."
\end{code}
  Here |modify| takes an update action |f|\footnote{|f| returns a list rather
  than a single element to prevent curious readers from suggesting to use a
  boxed array instead of a list.}, an index |i|, and a list |xs| and returns a
  new list with the |i|'th element replaced with the result of |f|.%
%if False
\begin{code}
  modifyTest :: SpecWith ()
  modifyTest = describe "Paper.modify" $ do
    it "Modifies the element at given index" $ do
      modify (\x -> [x - 1, x + 1]) 2 [(1 :: Int) .. 5]
        `shouldBe` [(1 :: Int), 2, 2, 4, 4, 5]
    it "Throws when an index is out of bounds" $ do
      evaluate (modify return (-1 :: Int) [(1 :: Int) .. 5])
        `shouldThrow` anyException
      evaluate (modify return (5 :: Int) [(1 :: Int) .. 5])
        `shouldThrow` anyException
\end{code}
%endif
  First, we ``unpack'' the list into $|before = [|\ttsub{|xs|}{|0|},\dots
  \ttsub{|xs|}{|i-1|}|]|$, $|x = |\ttsub{|xs|}{|i|}$, and
  $|after = [|\ttsub{|xs|}{|i+1|},\dots|]|$. Then replace |x| by |f(x)| and
  finally ``pack'' the result back into a single list. If we do a lot of
  updates, we end up unpacking and packing the list over and over again -- very
  time-consuming for long lists. We would thus like to benefit from fusion
  without explicitly working with the unpacked representation as it is
  bug-prone. A list zipper provides a way to achieve this.

  A zipper consists of a focus (alternatively called a hole) and surrounding
  context\footnote{`|!|' in front of the type of |_listZipperContext| is a
  strictness annotation enabled by the @BangPatterns@ extension. Context can be
  seen as a path from the to the current focus and is always finite, i.e. there
  is no reason for it to be lazy.}:
\begin{code}
  data ListZipper a  = ListZipper { _listZipperHole :: a, _listZipperContext :: !(ListContext a) }
  data ListContext a = ListContext [a] [a]
\end{code}
%if False
\begin{code}
  deriving instance Show a => Show (ListZipper a)
  deriving instance Eq a => Eq (ListZipper a)
  deriving instance Show a => Show (ListContext a)
  deriving instance Eq a => Eq (ListContext a)
\end{code}
%endif
  where the |ListContext| keeps track of elements to the left (|before| in
  |modify|) and to the right (|after| in |modify|) of the focus. We can now
  define movements:
\begin{code}
  lzLeft :: ListZipper a -> Maybe (ListZipper a)
  lzLeft (ListZipper _    (ListContext [] _))        = Nothing
  lzLeft (ListZipper hole (ListContext (l : ls) rs)) = Just $
    ListZipper l $ ListContext ls (hole : rs)

  lzRight :: ListZipper a -> Maybe (ListZipper a)
  lzRight (ListZipper _    (ListContext _ []))        = Nothing
  lzRight (ListZipper hole (ListContext ls (r : rs))) = Just $
    ListZipper r $ ListContext (hole : ls) rs
\end{code}
  and functions for entering and leaving the zipper:
\begin{code}
  lzEnter :: [a] -> Maybe (ListZipper a)
  lzEnter []       = Nothing
  lzEnter (x : xs) = Just $ ListZipper x (ListContext [] xs)

  lzLeave :: ListZipper a -> [a]
  lzLeave (ListZipper hole (ListContext ls rs)) = reverse ls ++ hole : rs
\end{code}

%if False
\begin{code}
  lzLeftTest :: SpecWith ()
  lzLeftTest = describe "Paper.lzLeft" $ do
    it "Moves the focus to the left" $ do
      lzLeft @Int (ListZipper 3 (ListContext [2, 1] [4, 5, 6])) `shouldBe`
        Just (ListZipper 2 (ListContext [1] [3, 4, 5, 6]))
      (lzLeft >=> lzLeft @Int) (ListZipper 3 (ListContext [2, 1] [4, 5, 6])) `shouldBe`
        Just (ListZipper 1 (ListContext [] [2, 3, 4, 5, 6]))
    it "Returns Nothing when an invalid move is attempted" $ do
      lzLeft @Int (ListZipper 1 (ListContext [] [2, 3])) `shouldBe` Nothing

  lzRightTest :: SpecWith ()
  lzRightTest = describe "Paper.lzRight" $ do
    it "Moves the focus to the right" $ do
      lzRight @Int (ListZipper 3 (ListContext [2, 1] [4, 5, 6])) `shouldBe`
        Just (ListZipper 4 (ListContext [3, 2, 1] [5, 6]))
      (lzRight >=> lzRight @Int) (ListZipper 3 (ListContext [2, 1] [4, 5, 6])) `shouldBe`
        Just (ListZipper 5 (ListContext [4, 3, 2, 1] [6]))
    it "Returns Nothing when an invalid move is attempted" $ do
      lzRight @Int (ListZipper 1 (ListContext [2] [])) `shouldBe` Nothing
\end{code}
%endif

  Finally, we define a local version of our |modify| function (\TODO{Boy, is
  this function ugly...})
\begin{code}
  lzModify :: (a -> [a]) -> ListZipper a -> Maybe (ListZipper a)
  lzModify f (ListZipper hole (ListContext ls rs)) = case f hole of
    (x : xs) -> Just $ ListZipper x (ListContext ls (xs ++ rs))
    []       -> case rs of
      (r : rs') -> Just $ ListZipper r (ListContext ls rs')
      []        -> case ls of
        (l : ls') -> Just $ ListZipper l (ListContext ls' rs)
        []        -> Nothing
\end{code}
  using which we can perform multiple updates efficiently and with minimal code
  bloat\footnote{%
  Operator @>=>@ comes from |Control.Monad| module in @base@
  and has the following signature:%
\begin{spec}
  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
\end{spec}%
  }:
\begin{code}
  modifyExample :: IO ()
  modifyExample = print $
    lzEnter @Int >=> lzRight
                 >=> lzRight
                 >=> lzModify (const [])
                 >=> lzModify (return ∘ (+1))
                 >=> lzLeft
                 >=> lzModify (return ∘ negate)
                 >=> return ∘ lzLeave $ [1, 2, 3, 4, 5]
\end{code}

  \paragraph{Trees} Consider now a binary tree data structure:
\begin{code}
  data Tree a = Leaf !a | Fork (Tree a) (Tree a)
\end{code}
%if False
\begin{code}
  deriving instance Show a => Show (Tree a)
  deriving instance Eq a => Eq (Tree a)
\end{code}
%endif
  A binary tree zipper is slightly more insteresting than the list zipper,
  because we can move up and down the tree as well as left and right. The zipper
  again consists of a hole (a subtree we are focused on) and its surrounding
  context (a path from the hole to the root of the tree):
\begin{code}
  data TreeZipper a  = TreeZipper { _treeZipperHole :: Tree a, _treeZipperContext :: !(TreeContext a) }
  data TreeContext a = TreeTop
                 | TreeLeft !(TreeContext a) (Tree a)
                 | TreeRight (Tree a) !(TreeContext a)
\end{code}
%if False
\begin{code}
  deriving instance Show (Tree a) => Show (TreeZipper a)
  deriving instance Eq (Tree a) => Eq (TreeZipper a)
  deriving instance Show (Tree a) => Show (TreeContext a)
  deriving instance Eq (Tree a) => Eq (TreeContext a)
\end{code}
%endif
  To move the zipper down, we ``unpack'' the current hole:
\begin{code}
  tzDown :: TreeZipper a -> Maybe (TreeZipper a)
  tzDown (TreeZipper (Leaf _)   _)   = Nothing
  tzDown (TreeZipper (Fork l r) cxt) = Just $ TreeZipper r (TreeRight l cxt)
\end{code}
  |TreeContext| stores everything we need to reconstruct the hole, and |tzUp| does
  exactly that:
\begin{code}
  tzUp :: TreeZipper a -> Maybe (TreeZipper a)
  tzUp (TreeZipper _ TreeTop) = Nothing
  tzUp (TreeZipper l (TreeLeft cxt r)) = Just $ TreeZipper (Fork l r) cxt
  tzUp (TreeZipper r (TreeRight l cxt)) = Just $ TreeZipper (Fork l r) cxt
\end{code}
  Implementations of |tzLeft|, |tzRight|, and |tzEnter| are very similar to the
  list zipper case and are left as an exercise for the reader. |tzLeave| differs
  slightly in that we now move all the way up rather than left:
\begin{code}
  tzLeave :: TreeZipper a -> Tree a
  tzLeave z = case tzUp z of
    Just z' -> tzLeave z'
    Nothing -> _treeZipperHole z
\end{code}

%if False
\begin{code}
  tzUpTest :: SpecWith ()
  tzUpTest = describe "Paper.tzUp" $ do
    it "Moves the focus upwards" $ do
      tzUp @Int
          (TreeZipper (Leaf 123) (TreeLeft (TreeRight (Leaf 5) TreeTop) (Leaf 8)))
        `shouldBe` Just
                     (TreeZipper (Fork (Leaf 123) (Leaf 8))
                                 (TreeRight (Leaf 5) TreeTop)
                     )
      (tzUp >=> tzUp @Int)
          (TreeZipper (Leaf 123) (TreeLeft (TreeRight (Leaf 5) TreeTop) (Leaf 8)))
        `shouldBe` Just
                     (TreeZipper
                       (Fork (Leaf 5) (Fork (Leaf 123) (Leaf 8)))
                       TreeTop
                     )
    it "Returns Nothing when at the top" $ do
      (tzUp >=> tzUp >=> tzUp @Int)
          (TreeZipper (Leaf 123) (TreeLeft (TreeRight (Leaf 5) TreeTop) (Leaf 8)))
        `shouldBe` Nothing

  tzDownTest :: SpecWith ()
  tzDownTest = describe "Paper.tzDown" $ do
    it "Moves the focus down" $ do
      tzDown @Int
          (TreeZipper (Fork (Leaf 123) (Fork (Leaf 1) (Leaf 2)))
                      (TreeLeft (TreeRight (Leaf 5) TreeTop) (Leaf 8))
          )
        `shouldBe` Just
                     (TreeZipper
                       (Fork (Leaf 1) (Leaf 2))
                       (TreeRight
                         (Leaf 123)
                         (TreeLeft (TreeRight (Leaf 5) TreeTop) (Leaf 8))
                       )
                     )
    it "Returns Nothing when at the bottom" $ do
      tzDown @Int
          (TreeZipper (Leaf 123) (TreeLeft (TreeRight (Leaf 5) TreeTop) (Leaf 8)))
        `shouldBe` Nothing
\end{code}
%endif

  \paragraph{Generic Zipper} The list and binary tree zippers we have considered
  so far are homogeneous zippers: the type of the focus does not change upon zipper
  movement. Such a zipper can be a very useful abstraction. For example, a
  well-known window manager XMonad\cite{xmonad} uses a rose tree zipper to track
  the window under focus. For other tasks, however, one might need to traverse
  heterogeneous structures. A zipper that can accomodate such needs is usually
  called a \textit{generic zipper} as it relies only on the generic structure of
  Algebraic Data Types (ADTs). One can view an ADT as an Abstract Syntax Tree
  (AST) where each node is a Haskell constructor rather than a syntax construct.

  The generic zipper we will use is very similar to the one presented
  in\cite{adams2010syz}. The most common technique in Haskell for supporting
  heterogeneous types is Existential Quantification. However, not every type can
  act as a hole. To support moving down the tree, we need the hole to be
  \textit{dissectible}, i.e. we would like to be able to dissect the value into
  the constructor and its arguments. Even though |Data.Data.gfoldl| allows us to
  achieve this, we define out own typeclass which additionally allows us to
  propagate down arbitrary constraints:
\begin{code}
  class Dissectible (c :: Type -> Constraint) (a :: Type) where
    dissect :: a -> Left c a

  data Left c expects where
    LOne  :: b -> Left c b
    LCons :: (c b, Dissectible c b)
          => Left c (b -> expects) -> b -> Left c expects
\end{code}
  For example, here is how we can make |Tree| an instance of |Disssectible|
\begin{code}
  instance c (Tree a) => Dissectible c (Tree a) where
    dissect (Fork l r) = LOne Fork `LCons` l `LCons` r
    dissect x = LOne x
\end{code}
  We can unpack a |Fork| and the zipper will thus be able to go down. |Leaf|s,
  however, are left untouched and trying to go down from a |Leaf| will return
  |Nothing|.

  To allow the zipper to move left and right, we need a means to encode
  arguments to the right of the hole. Following Adams et al, we define a GADT
  representing constructor arguments to the right of the hole:
\begin{code}
  data Right c provides r where
    RNil  :: Right c r r
    RCons :: (c b, Dissectible c b)
          => b -> Right c provides r -> Right c (b -> provides) r
\end{code}
  For example, for a tuple |(Int, Int, Int, Int, Int, Int)|, we can have
\begin{spec}
  lefts = LOne (,,,,,) `LCons` 1 `LCons` 2 `LCons` 3
  hole = 4
  rights = 5 `RCons` 6 `RCons` RNil
\end{spec}
  generalising this a little, we arrive at:
\begin{code}
  data LocalContext c hole rights parent =
    LocalContext !(Left c (hole -> rights)) !(Right c rights parent)

  data Context :: (Type -> Constraint) -> Type -> Type -> Type where
    RootContext :: forall c root. Context c root root
    (:>) :: forall c parent root hole rights. (c parent, Dissectible c parent)
         => !(Context c parent root)
         -> !(LocalContext c hole rights parent)
         -> Context c hole root
\end{code}
  And just like before the |Zipper| is a product of the hole and context:
\begin{code}
  data Zipper (c :: Type -> Constraint) (root :: Type) =
    forall hole. (c hole, Dissectible c hole) =>
      Zipper { _zHole :: !hole
             , _zCxt  :: !(Context c hole root)
             }
\end{code}

  Implementation of movements is quite straightforward and is left out. Please,
  refer to (\TODO{github repo}) for complete code. We now consider a rather
  interesting application of generic zipper: embedding of attribute grammars.
