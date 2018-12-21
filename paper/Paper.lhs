\documentclass[runningheads]{llncs}

\usepackage{amsmath}
\usepackage{calc}
\usepackage{csquotes}
\usepackage{color}
\usepackage{comment}
\usepackage{float}
\usepackage{graphicx}
\usepackage{url}
\usepackage{subcaption}
\usepackage{wrapfig}
\usepackage{pbox}

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

%format TreeZipper         = Zipper
%format _treeZipperHole    = _hole
%format _treeZipperContext = _cxt
%format TreeContext = Context
%format TreeTop     = Top
%format TreeLeft    = Left
%format TreeRight   = Right
%format tzUp    = up
%format tzDown  = down
%format tzLeft  = left
%format tzRight = right
%format tzEnter = enter
%format tzLeave = leave

%format ListZipper = Zipper
%format _listZipperHole = _hole
%format _listZipperContext = _cxt
%format ListContext = Context
%format lzLeft = left
%format lzRight = right
%format lzEnter = enter
%format lzLeave = leave
%format lzModify = modify


\newcommand{\WithMath}[2]{{\parbox[][][b]{\widthof{#1}}{\centering$#2$}}}

%format . = "."
%format ∘ = "\WithMath{.}{\circ}"
%format == = "\WithMath{==}{\equiv}"
%format >=> = ">=>"
%format ++ = "\WithMath{++}{+\!\!+}"
%format -> = "\WithMath{->}{\rightarrow}"
%format error = "\textit{error}"
%format otherwise = "\textit{otherwise}"
%format ___UNPACK___ = "{-# UNPACK #-}"

% For the circular variant of repmin
%format mx = "\parbox[][3.5pt][t]{\widthof{mx}}{m$_{\text{x}}$}"
%format my = "\parbox[][3.5pt][t]{\widthof{my}}{m$_{\text{y}}$}"

% For the specification of AGs
%format SYN = "SYN"
%format SEM = "SEM"
%format INH = "INH"
%format DATA = "DATA"

% Tree positions
%format C_Fork = "\parbox{\widthof{CFork}}{\textit{C}$_\text{\textit{Fork}}$}"
%format C_Leaf = "\parbox{\widthof{CLeaf}}{\textit{C}$_\text{\textit{Leaf}}$}"


% A command for declaring todos
\newcommand{\TODO}[1]{{\color[rgb]{1,0,0}\textbf{TODO:}\textit{#1}}}

\newcommand{\ttsub}[2]{{\text{#1}_\text{#2}}}

\newcommand{\Let}{{\sf LET}}

\begin{document}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\title{Modern type-safe embedding of attribute grammars}
\subtitle{An exercise with functional zippers in Haskell}


% TODO(twesterhout): This looks ugly... Someone, reformat it, please :)
\author{Jo{\~a}o Paulo Fernandes\inst{1}%
   \and Pedro Martins\inst{2}%
   \and Alberto Pardo\inst{3}%
   \and Jo{\~a}o Saraiva\inst{4}%
   \and Marcos Viera\inst{3}%
   \and Tom Westerhout\inst{5}%
}
\institute{%
  CISUC -- Universidade de Coimbra, Portugal \email{jpf@@dei.uc.pt} \and
  University of California, Irvine, USA \email{pribeiro@@uci.edu} \and
  Universidad de la  Rep\'{u}blica, Uruguay, \email{\{pardo,mviera\}@@fing.edu.uy} \and
  Universidade do Minho, Portugal, \email{saraiva@@di.uminho.pt} \and
  Radboud University, The Netherlands, \email{t.westerhout@@student.ru.nl}%
}

\date{}

\maketitle

  % 1) Introduction. In one sentence, what’s the topic?
  % 2) State the problem you tackle
  % 3) Summarize (in one sentence) why nobody else has adequately answered the
  %    research question yet.
  % 4) Explain, in one sentence, how you tackled the research question.
  % 5) In one sentence, how did you go about doing the research that follows
  %    from your big idea.
  % 6) As a single sentence, what’s the key impact of your research?
\begin{abstract}
  Attribute grammars are a powerful, declarative formalism to implement and
  reason about programs which, by design, are conveniently modular. Although a
  full attribute grammar compiler can be tailored to specific needs, its
  implementation is highly non-trivial, and its long-term maintenance is a major
  endeavor. In fact, maintaining a traditional attribute grammar system is such
  a big effort that most systems that were proposed in the past are no longer
  active. Our approach to implementing attribute grammars is to write them as
  first-class citizens of a modern functional programming language. We improve a
  previous zipper-based attribute grammar embedding making it non-intrusive
  (i.e. no changes to the user-defined data types are required) and type-safe.
  On top of that, we achieve clearer syntax by using modern Haskell extensions.
  We believe that our embedding can be employed in practice to implement
  elegant, efficient, and modular solutions to real-life programming challenges.

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

  Attribute Grammars (AGs) are a declarative formalism which was proposed by
  Knuth~\cite{Knuth68} in the late 60s and allows one to implement and reason
  about programs in a modular and convenient way. A concrete AG relies on a
  context-free grammar to define the syntax of a language, and on attributes
  associated with the productions of the grammar to define the semantics of that
  language. AGs have been used in practice to specify real programming
  languages, like for example Haskell~\cite{DijkstraFS09}, as well as powerful
  pretty printing algorithms~\cite{SPS99}, deforestation
  techniques~\cite{joao07pepm}, and powerful type systems~\cite{MiddelkoopDS10}.

  When programming with AGs, modularity is achieved due to the possibility of
  defining and using different aspects of computations as separate attributes.
  Attributes are distinct computation units, typically quite simple and modular,
  that can be combined into elaborated solutions to complex programming
  problems. They can also be analyzed, debugged, and maintained independently
  which eases program development and evolution.

  AGs have proven to be particularly useful to specify computations over trees:
  given one tree, several AG systems such as~\cite{syngen,uuag,lrc,silver} take
  specifications of which values or attributes need to be computed and perform
  these computations. The design and coding efforts put into the creation,
  improvement, and maintenance of these AG systems, however, is tremendous which
  is often an obstacle to achieving the success they deserve.

  An increasingly popular alternative approach to the use of AGs relies on
  embedding them as first-class citizens of general purpose programming
  languages~\cite{Oege00,DBLP:conf/sblp/MartinsFS13,erlangAGs,kiama,doaitse09icfp,balestrieri}.
  This avoids the burden of implementing a totally new language and associated
  system by hosting it in state-of-the-art programming languages. Following this
  approach one then exploits the modern constructions and infrastructure that
  are already provided by those languages and focuses on the particularities of
  the domain specific language being developed.

  Functional zipper~\cite{huet1997zipper} is a powerful abstraction which
  greatly simplifies the implementation of traversal algorithms performing a lot
  of local updates. Functional zippers have successfully been applied to
  construct an AG embedding in
  Haskell~\cite{DBLP:conf/sblp/MartinsFS13,MARTINS20162}. Despite its elegance,
  this solution had a major drawback which prevented its use in real-world
  applications: attributes were not cached, but rather repeatedly recomputed
  which severely hurt performance. Recently, this flaw has been
  eliminated~\cite{FERNANDES2018} and replaced with a different one: the
  approach became intrusive, i.e. to benefit from the embedding user-defined
  data structures have to be adjusted.

  In this paper we present an alternative mechanism to cache attributes based on
  a self-organising infinite grid. This graph is laid on top of the user-defined
  algebraic data type (ADT) and mirrors its structure. The used-defined data type
  itself remains untouched. The embedding is then based on two (rather than one)
  coherent zippers traversing the data structures in parallel. On top of being
  non-intrusive our solution is completely type-safe. Modern Haskell extensions
  such as \texttt{ConstraintKinds} allow us to propagate constraints down in the
  ADT completely eliminating run-time type casts present in the previous
  versions. Another side benefit of using modern Haskell features is a cleaner
  syntax with less code being generated by means of Template Haskell.

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

  module Main where

  import           Test.Hspec                     ( hspec )

  import           Zippers
  import           AGs

  main :: IO ()
  main = do
    putStrLn "Hello world!"
    print $ repmin $ Fork @Int (Fork (Leaf 123) (Leaf 0)) (Leaf 5)
    modifyExample
    hspec $ do
      modifyTest
      lzLeftTest
      lzRightTest
      tzUpTest
      tzDownTest

\end{code}
%endif



\section{Running example}

%include Let.lhs



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%if not abstract_only
%include Zippers.lhs
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%if not abstract_only
%include AGs.lhs
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%if not abstract_only
\section{Related Work}\label{sec:related-work}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%if not abstract_only
\section{Conclusion}\label{sec:conclusion}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%if not abstract_only
\section*{Acknowledgements}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As per the LLNCS guidelines
\bibliographystyle{splncs04}
\bibliography{References}

\end{document}

