
In order to illustrate the convenience of our embedding, we will consider the problem of analyzing the semantic correctness of certain functional programming excerpts. In particular, we will focus on |let| expressions that are common in functional languages like Haskell~\cite{}, ML~\cite{}, or Scala~\cite{}. Throughout the paper, we shall refer to these expressions as programs in the \Let\ language.

A program in the \Let\ language consists of instruction blocks, where each instruction  either: i) declares a variable or ii) defines a nested block.  When declaring a variable, the programmer must also assigned a value to it; such value may be a constant, the value of a variable or an expression on these elements. Ultimately, each block defines a value as an expression on variables defined in it.

Examples of programs in \Let\ are given next.

%format program_1
%format program_2
%format program_3
%format program_e

\begin{spec}

program_1 =  let  x = 4
             in   x

program_2 =  let  x = 4
             in   x + 3

program_3 =  let  x = 4
                  y =  let  w = 2
                       in   x + w
             in   x + y          

\end{spec}

The values associated with |program_1| and |program_2| are, quite straightforwardly, |4| and |7|, respectively. As of |program_3|, its value of |10| is calculated adding the value of |x|, which is |4| and the value of |y|, i.e., |6|. The value of |y| is obtained by adding the value of |w|, which is |2|, and the value of |x|, which, again, is |4|.

Our goal is to implement a semantic analyzer that deals with the scope rules of the \Let\ language. These rules are quite natural and can easily be described as: 

\begin{enumerate}
\item if a variable is used in a block, it must be declared in that same block or in an outer one. The declaration of a variable, however, may occur after its use. 

\item a variable identifier may be declared at most once within the same block. In an inner block, declaring a variable that has already been declared in an outer one is allowed: the identifier in the local scope hides the definition of the same identifier in the global one.

\end{enumerate}

Let us now consider a more complex \Let\ program:

\begin{spec}

program_e =  let  x = y
                  a =  let  y = 4
                       in   y + w
                  x = 5
                  y = 6
             in   x + a
                   
\end{spec}

According to the scope rules that we have just defined, |program_e| contains two errors: 1) at the outer block, variable |x| has been declared twice; and 2) at the inner block, the use of variable |w| has no binding occurrence at all. Notice that |y| has been declared at both the inner and the outer levels, which in itself is not a problem (the inner declaration hides the outer one).

Programs such as the ones we have presented describe the basic block-structure found in many languages, with the peculiarity that variables can be used before they are defined.

We aim to implement a program that analyses \Let\ programs and computes a list containing the identifiers which do not obey the scope rules of the language. In order to facilitate the debugging phase, we require that the list of invalid identifiers follows the sequential structure of the program. That is to say, e.g., that the semantic meaning of processing |program_e| is |[w, x]|: the use of the undeclared variable |w| occurs in line 3 whereas the duplicate declaration of |x| occurs in line 4.

In order to implement such program, we first need a representation for programs in the \Let\ language. For this, we may use the following Haskell data-types:

\begin{code}

type Var    = String

data Let    = Let Decls Expr

data Decls  =  Empty
            |  Cons    Var Expr  Decls
            |  Nested  Var Let   Decls

data Expr   =  Const     Int
            |  Variable  Var
            |  Plus   Expr Expr 
            |  Times  Expr Expr

\end{code}

In this representation, |program_e| above is defined as:

\begin{code}

program_e = Let  (  Cons    "x"  (Variable "y") (
                    Nested  "a"  (Let (
                            Cons "y" (Const 4) Empty) (
                            Plus (Variable "y") (Variable "w"))) (
                    Cons "x" (Const 5) (
                    Cons "y" (Const 6) Empty)))) (
                    Plus (Variable "x") (Variable "a"))

\end{code}

















