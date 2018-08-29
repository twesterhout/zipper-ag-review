\documentclass[runningheads]{llncs}

\usepackage{amsmath}
\usepackage{color}
\usepackage{comment}
\usepackage{float}
\usepackage{graphicx}
\usepackage{url}
\usepackage{subcaption}
\usepackage{wrapfig}

\captionsetup{compatibility=false}

%include polycode.fmt

%%format .*.       = "\mathbin{.\!\!*\!\!.}"
%%format .=.       = "\mathbin{.\!\!=\!\!.}"
%%format .<.       = "\mathbin{.\!\!<\!\!.}"
%%format sp = " "
%%format qq = "''\!"
%%format .$$ = ".\$_m"
%%format parent' = "parent_m"
%%format  <*>       = "\mathbin{\text{\small\ttfamily{<*>}}}"
%%format  <$>       = "\mathbin{\text{\small\ttfamily{<\$>}}}"
%%format t1 = "t_1"
%%format t2 = "t_2"
%%format constructor' = "constructor_m"
%%%%format Leaf = "{\sf Leaf}"
%%%%format Fork = "{\sf Fork}"
%%format Fork3 = "{\sf Fork3}"
%%%%%%format Root = "{\sf Root}"
%%format CRoot = "C_{\mathit{Root}}"
%%%%%%"{\sf C_{Root}}"
%%format CLeaf = "C_{\mathit{Leaf}}"
%%%%%"{\sf C_{Leaf}}"
%%format CFork = "C_{\mathit{Fork}}"
%%%%% "{\sf C_{Fork}}"
%%format lexeme_Leaf = "lexeme_{\sf Leaf}"
%%format MemoRoot = "{\sf Memo_{Root}}"
%%format MemoFork = "\mathit{Fork}_m"
%%%%%%"{\sf Fork}_m"
%%%%%% "{\sf Memo_{Fork}}"
%%format MemoLeaf = "\mathit{Leaf}_m"
%%%%%%"{\sf Leaf}_m"
%%%%%% "{\sf Memo_{Leaf}}"
%%format treeLookup = "lookup_{MT}"
%%%format MemoTable = "{Cache}"
%%format buildMTree = "\mathit{build}_m"
%%%%%%"build_{MT}"
%%format constructor_m = "\mathit{constructor}_m"
%%format tree_m = "\mathit{tree}_m"
%%format left_m = "\mathit{left}_m"
%%format right_m = "\mathit{right}_m"
%%format constructorM_m = "\mathit{constructorM}_m"
%%format treeM_m = "\mathit{treeM}_m"
%%format leftM_m = "\mathit{leftM}_m"
%%format rightM_m = "\mathit{rightM}_m"
%%format up_m = "\mathit{up}_m"
%%format down_m = "\mathit{down}_m"
%%format modify_m = "\mathit{modify}_m"
%%format mkAG_m = "mkAG_m"
%%format Cxt_m = "\mathit{Cxt}_m"
%%format Root_m = "Root_m"
%%format Top_m = "Top_m"
%%format L_m = "L_m"
%%format R_m = "R_m"
%%format MemoTree = "\mathit{Tree}_m"
%%format ZipperMemoTree = "\mathit{Zipper}_m"
%%format MemoAGTree = "\mathit{AGTree}_m"
%%format C_Memo_RootLet    = "C_{\mathit{Memo\_RootLet   }}"
%%format C_Memo_Let        = "C_{\mathit{Memo\_Let       }}"
%%format C_Memo_In         = "C_{\mathit{Memo\_In        }}"
%%format C_Memo_ConsAssign = "C_{\mathit{Memo\_ConsAssign}}"
%%format C_Memo_ConsLet    = "C_{\mathit{Memo\_ConsLet   }}"
%%format C_Memo_EmptyList  = "C_{\mathit{Memo\_EmptyList }}"
%%format C_Memo_Plus       = "C_{\mathit{Memo\_Plus      }}"
%%format C_Memo_Variable   = "C_{\mathit{Memo\_Variable  }}"
%%format C_Memo_Constant   = "C_{\mathit{Memo\_Constant  }}"
%%format lexemeConsAssign  = "lexeme_{\mathit{ConsAssign}}"
%%format lexemmeConsLet    = "lexeme_{\mathit{ConsLet}}"
%%format calculateMemo     = "calculate_{\mathit{Memo}}"
%%format errsAlgolMemo     = "errs_{\mathit{AlgolMemo}}"
%%format errorsMemo             = "errors_{\mathit{Memo}}"
%%format algol68                = "algol_{\mathit{68}}"
%%format ata_algol68            = "ata\_algol_{\mathit{68}}"
%%format LetSem.constructorMemo = "LetSem.constructor_{\mathit{Memo}}"
%%format lexemeString           = "lexeme_{\mathit{String}}"
%%format upGetVarValueMemo      = "up_{\mathit{GetVarValueMemo}}"
%%format lexemeInt              = "lexeme_{\mathit{Int}}"
%%format Child1              = "Child_{\mathit{1}}"
%%format Child2              = "Child_{\mathit{2}}"
%%format Child3              = "Child_{\mathit{3}}"
%%format Childi              = "Child_{\mathit{i}}"
%%format constructorMemo     = "constructor_{\mathit{Memo}}"
%%format at = "@"
%%format Algol68m               = "Algol68_{\mathit{m}}"

% Formatting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%format TreePath = Path
%format TreeTop = Top
%format TreeLeft = Left
%format TreeRights = Right
%format TreeZipper = Zipper

%format ListZipper = Zipper
%format _listZipperHole = _hole
%format _listZipperContext = _cxt
%format ListContext = Context
%format lzLeft = left
%format lzRight = right

%format ! = "\,!"

% A command for declaring todos
\newcommand{\TODO}[1]{{\color[rgb]{1,0,0}\textbf{TODO:}\textit{#1}}}


\begin{document}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\title{\TODO{What's the title?}}

% TODO(twesterhout): This looks ugly... Someone, reformat it, please :)
\author{Jo{\~a}o Paulo Fernandes\inst{1}%
   \and Pedro Martins\inst{2}%
   \and Alberto Pardo\inst{3}%
   \and Jo{\~a}o Saraiva\inst{4}%
   \and Marcos Viera\inst{3}%
   \and Tom Westerhout\inst{5}%
}
\institute{%
  LISP/Release -- Universidade da Beira Interior, Portugal \email{jpf@@di.ubi.pt} \and
  University of California, Irvine, USA \email{pribeiro@@uci.edu} \and
  Universidad de la  Rep\'{u}blica, Uruguay \email{\{pardo,mviera\}@@fing.edu.uy} \and
  Universidade do Minho, Portugal \email{saraiva@@di.uminho.pt} \and
  Radboud University, The Netherlands \email{twesterhout@@student.ru.nl}%
}

\date{}

\maketitle

\begin{abstract}
  \TODO{What's the abstract?}

\keywords{%
       Embedded Domain Specific Languages
  \and Zipper data structure
  \and Memoization
  \and Attribute Grammars
  \and Higher-Order Attribute Grammars
  \and Functional Programming%
}
\end{abstract}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Introduction}\label{sec:introduction}

%if False
\begin{code}
  {-# LANGUAGE BangPatterns #-}
  {-# LANGUAGE TypeApplications #-}

  module Main where

  import           Control.Exception              ( evaluate )
  import           Control.Monad                  ( (>=>) )
  import           Test.Hspec                     ( SpecWith
                                                  , shouldBe
                                                  , shouldThrow
                                                  , describe
                                                  , it
                                                  , hspec
                                                  , anyException
                                                  )

  main :: IO ()
  main = do
    putStrLn "Hello world!"
    modifyExample
    hspec $ do
      modifyTest
      lzLeftTest
      lzRightTest
\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Functional Zippers}
  Zipper is a data structure commonly used in functional programming for
  traversal with fast local updates. The zipper data structure was originally
  conceived by Huet\cite{huet1997zipper} in the context of trees. We will,
  however, first consider a simpler problem: a bidirectional list traversal.

  Suppose that we would like to update a list at a specific position:
\begin{code}
  modify :: (a -> [a]) -> Int -> [a] -> [a]
  modify f i xs = helper [] xs 0
   where
    helper before (x : after) !j
      | j == i    = before ++ f x ++ after
      | otherwise = helper (before ++ [x]) after (j + 1)
    helper _ [] _ = error "Index out of bounds."
\end{code}
  Here @modify@ takes an update action @f@\footnote{@f@ returns a list rather
  than a single element to prevent curious readers from suggesting to use a
  boxed array instead of a list.}, an index @i@, and a list @xs@ and returns a
  new list with the @i@'th element replaced with the result of @f@.%
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
  This function "unpacks" a list, modifies one element, and "packs" the result
  into a list. If we do a lot of updates, we end up unpacking and packing the
  list over and over again -- very time-consuming for long lists. Explicitly
  working with the unpacked representation is bug-prone. A list zipper
  simplifies this.

  A zipper consists of a focus (alternatively called a hole) and surrounding
  context:
\begin{code}
  data ListZipper a = ListZipper
    { _listZipperHole :: a, _listZipperContext :: !(ListContext a) }
    deriving (Show, Eq)
  data ListContext a = ListContext [a] [a]
    deriving (Show, Eq)
\end{code}
  where the @ListContext@ keeps track of elements to the left and to the right
  of the focus. We can now define movements:
\begin{code}
  lzLeft :: ListZipper a -> Maybe (ListZipper a)
  lzLeft (ListZipper _ (ListContext [] _)) = Nothing
  lzLeft (ListZipper hole (ListContext (l : ls) rs)) = Just $
    ListZipper l (ListContext ls (hole : rs))

  lzRight :: ListZipper a -> Maybe (ListZipper a)
  lzRight (ListZipper _ (ListContext _ [])) = Nothing
  lzRight (ListZipper hole (ListContext ls (r : rs))) = Just $
    ListZipper r (ListContext (hole : ls) rs)
\end{code}
  and functions for entering and leaving the zipper:
\begin{code}
  lzEnter :: [a] -> Maybe (ListZipper a)
  lzEnter [] = Nothing
  lzEnter (x:xs) = Just $ ListZipper x (ListContext [] xs)

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

  Finally, we define a local version of our @modify@ function (\TODO{Boy, is
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
  using which we can perform multiple update efficiently and with minimal code
  bloat:
\begin{code}
  modifyExample :: IO ()
  modifyExample = print $
    lzEnter @Int >=> lzRight
                 >=> lzRight
                 >=> lzModify (const [])
                 >=> lzModify (return . (+1))
                 >=> lzLeft
                 >=> lzModify (return . negate)
                 >=> return . lzLeave $
      [1, 2, 3, 4, 5]
\end{code}

\begin{comment}
  to solve the problem of representing a tree together
  with a subtree that is the focus of attention, where that focus may move left,
  right, up or down the tree. Bla-bla-bla...
\end{comment}

  Application to binary trees...
\begin{code}
  data Tree a
    = Fork (Tree a) (Tree a)
    | Leaf !a

  data TreePath a
    = TreeTop
    | TreeLeft !(TreePath a) (Tree a)
    | TreeRight (Tree a) !(TreePath a)

  data TreeZipper a = TreeZipper !(TreePath a) (Tree a)
\end{code}

  Application to lists...

  Generic zipper...

  An application of generic zipper that we will consider is embedding of
  attribute grammars.

\section{Attribute Grammars}
  What attribute grammars are...

  Repmin as two traversals...

  Repmin as a circular program...

  Repmin as an AG...


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Related Work}\label{sec:related-work}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Conclusion}\label{sec:conclusion}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{Acknowledgements}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{References}
  % As per the LLNCS guidelines
  \bibliographystyle{splncs04}
  \bibliography{References}

\end{document}

