
In order to illustrate the convenience of our embedding, we will consider the problem of analyzing the semantic correctness of certain functional programming excerpts. In particular, we will focus on |let| expressions that are common in functional languages like Haskell~\cite{}, ML~\cite{}, or Scala~\cite{}. Throughout the paper, we shall refer to these expressions as programs in the \Let\ language.

A program in the \Let\ language consists of instruction blocks, where each instruction  either: i) declares a variable or ii) defines a nested block.  When declaring a variable, the programmer must also assigned a value to it; such value may be a constant, the value of a variable or an expression on these elements. Ultimately, each block defines a value as an expression on variables defined in it.

Examples of programs in \Let\ are given next.

%format program_1
%format program_2
%format program_3

\begin{code}

program_1 =  let  x = 4
             in   x

program_2 =  let  x = 4
             in   x + 3

program_3 =  let  x = 4
                  y =  let  w = 2
                       in   x + w
             in   x + y          

\end{code}

The values associated with |program_1| and |program_2| are, quite straightforwardly, |4| and |7|, respectively. As of |program_3|, its value of |10| is calculated adding the value of |x|, which is |4| and the value of |y|, i.e., |6|. The value of |y| is obtained by adding the value of |w|, which is |2|, and the value of |x|, which, again, is |4|.

Our goal is to implement a semantic analyzer that deals with the scope rules of the \Let\ language. These rules are quite natural and can easily be described as: 

\begin{enumerate}
\item if a variable is used in a block, it must be declared in that same block or in an outer one. The declaration of a variable, however, may occur after its use. 

\item a variable identifier may be declared at most once within the same block. In an inner block, declaring a variable that has already been declared in an outer one is allowed: the identifier in the local scope hides the definition of the same identifier in the global one.

\end{enumerate}

