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

  module AGs
    ( repmin
    ) where

  import           Data.Kind

  import           Language.AG.Constraint (type (&&&))

  import           Zippers
\end{code}
%endif


\section{Attribute Grammars}
  Attribute grammars (AGs) are an extension of context-free grammars that allow
  to specify context-sensitive syntax as well as the semantics. AGs achieve it
  by associating a set of attributes with each grammar symbol. These attributes
  are defined using evaluation rules assiciated with production rules of the
  context-free grammar.

  Attributes are then usually divided into two disjoint sets: synthesized
  attributes and the inherited attributes. Such distinction is required for the
  construction of a dependency graph. It is then used for specification of the
  evaluation order and detection of circularity. In the zipper-based embedding of
  attribute grammars we make no use of a dependency graph and thus do not divide
  attributes into classes.

  Let us consider the repmin problem as an example of a problem that requires
  multiple traversals:
  \begin{displayquote}
    Given a tree of integers, replace every integer with the
    minimum integer in the tree, in one pass.
  \end{displayquote}
  The classical solution is the following circular program:
\begin{code}
  repmin :: Tree Int -> Tree Int
  repmin t = t'
    where (t', m') = go t m'
          go (Leaf x    ) m = (Leaf m, x)
          go (Fork xs ys) m = (Fork xs' ys', min mx my)
            where (xs', mx) = go xs m
                  (ys', my) = go ys m
\end{code}

  Although quite elegant, the code lacks modularity and is very difficult to
  reason about. Attribute Grammars provide a more modular approach. Viera et
  al\cite{viera2009agsfly} identified three steps for solving repmin: computing
  the minimal value, passing it down from the root to the leaves, and
  constructing the resulting tree. We can associate each step with an
  attribute\cite{swierstra1998designing}:
  \begin{itemize}
  \item A synthesized attribute |localMin :: Int| represents the minimum value
        of a subtree. Computing the minimal value thus corresponds to evaluation
        of the |localMin| attribute for the root tree.
  \item An inherited attribute |globalMin :: Int| is used to pass down the
        minimal value.
  \item Finally, a synthesized attribute |updated :: Tree Int| is the subtree
        with leaf values replaced by values of their |globalMin| attributes. The
        solution is thus the value of |updated| attribute for the root tree.
  \end{itemize}
  The obtained AG is presented in figure~\ref{fig:repmin-AG}. \TODO{Do we
  actually need to explain the algorithms here? It seems a little bit childish
  to explain how to compute the minimum of a binary tree... If we absolutely
  have to explain stuff, maybe just put it in the caption.}

\begin{figure}
\centering
\fbox{\parbox{\linewidth}{%
\begin{spec}
  SYN Tree Int [localMin : Int]
  SEM Tree Int | Leaf lhs.localMin = @value
               | Fork lhs.localMin = min @left.localMin
                                         @right.localMin

  SYN Tree Int [updated : Tree Int]
  SEM Tree Int | Leaf lhs.updated = Leaf @lhs.globalMin
               | Fork lhs.updated = Fork @left.updated
                                         @right.updated


  INH Tree Int [ globalMin : Int ]
  SEM Tree Int | Fork left.globalMin  = @lhs.globalMin
                      right.globalMin = @lhs.globalMin

  DATA Root | Root tree : Tree Int
  SEM  Root | Root tree.globalMin = @tree.localMin
\end{spec}%
}}
\caption{%
  Attribute grammar for repmin. The syntax is closely mirrors the one used
  in\cite{swierstra1998designing}. |SYN| and |INH| introduce synthesized
  and inherited attributes respectively. |SEM| is used for defining semantic
  rules. A new data type |Root| is introduced as it is common in the AG setting
  to ``connect'' |localMin| with |globalMin|.
}
\label{fig:repmin-AG}
\end{figure}

  We now move on to embed this attribute grammar into Haskell. Semantic rules
  simply become functions, which, given a zipper, return values of the
  attributes. For example,
\begin{code}
  localMin :: Zipper (WhereAmI Position) (Tree Int) -> Int
  localMin z@(Zipper hole _) = case whereami hole of
    C_Leaf -> let Leaf x = hole in x
    C_Fork -> let Just l = child 0 z; Just r = child 1 z
              in min (localMin l) (localMin r)
\end{code}

  Apart from the type signature, the code is pretty straightforward and closely
  mirrors the AG we defined earlier. |whereami| function allows us to ``look
  around'' and returns the position of the zipper. Thus the |case| corresponds
  to the pattern matches on the left of the vertical bars on
  figure~\ref{fig:repmin-AG}.

  Position of the zipper is encoded using the following GADT which can be
  generated automatically using Template Haskell:
\begin{code}
  data Position :: Type -> Type where
    C_Leaf :: Position (Tree Int)
    C_Fork :: Position (Tree Int)
\end{code}
  Parametrization on the type of the hole allows the code like
  |let Leaf x = hole in x| to typecheck, even though the generic zipper itself knows close
  to nothing about the type of the hole. It might seem trivial at first, because
  the binary tree zipper is in fact homogeneous. The ``position trick'' however
  extends also to heterogeneous zippers which we will encounter in more advanced
  examples.

\begin{code}
  child :: Int -> Zipper cxt root -> Maybe (Zipper cxt root)
\end{code}
  |child n| moves the zipper to the |n|'th child, if there is one.

%if False
\begin{code}
  type Attributes = '[]
  type Cxt = WhereAmI Position &&& Show

  child = undefined

  unsafeChild :: Int -> Zipper cxt root -> Zipper cxt root
  unsafeChild = undefined


  deriving instance Eq (Position a)
  deriving instance Show (Position a)

  class WhereAmI (p :: Type -> Type) (a :: Type) where
    whereami :: a -> p a

  instance WhereAmI Position (Tree Int) where
    whereami (Fork _ _) = C_Fork
    whereami (Leaf _)   = C_Leaf
\end{code}
%endif
