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

%format ListPath = Path
%format ListZipper = Zipper

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

\begin{code}
  main :: IO ()
  main = putStrLn "Hello world!"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Functional Zippers}
  The zipper data structure was originally conceived by
  Huet\cite{huet1997zipper} to solve the problem of representing a tree together
  with a subtree that is the focus of attention, where that focus may move left,
  right, up or down the tree. Bla-bla-bla...

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
\begin{code}
  data ListPath a = ListPath [a] [a]

  data ListZipper a = ListZipper !(TreePath a) [a]
\end{code}

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

