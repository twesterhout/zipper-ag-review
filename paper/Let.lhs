%if False
\begin{code}
module Let where
\end{code}
%endif

\newcommand{\withSub}[2]{{\parbox{\widthof{#1#2}}{#1$_\text{#2}$}}}
\newcommand{\itWithSub}[2]{{\parbox{\widthof{#1#2}}{\textit{#1}$_\text{#2}$}}}
%format program1       = "\withSub{program}{1}"
%format program2       = "\withSub{program}{2}"
%format program3       = "\withSub{program}{3}"
%format programe       = "\withSub{program}{e}"
%format Let2           = "\itWithSub{Let}{2}"
%format Decls2         = "\itWithSub{Decls}{2}"
%format Cons2          = "\itWithSub{Cons}{2}"
%format Empty2         = "\itWithSub{Empty}{2}"
%format Nested2        = "\itWithSub{Nested}{2}"
%format let2           = "\withSub{let}{2}"
%format decls2         = "\withSub{decls}{2}"
%format errors1        = "\withSub{errors}{1}"
%format errors2        = "\withSub{errors}{2}"
%format nested2        = "\withSub{nested}{2}"
%format expr1          = "\withSub{expr}{1}"
%format expr2          = "\withSub{expr}{2}"
%format duplicateLet   = "\withSub{duplicate}{\textit{Let}}"
%format duplicateDecls = "\withSub{duplicate}{\textit{Decls}}"
%format missingLet     = "\withSub{missing}{\textit{Let}}"
%format missingDecls   = "\withSub{missing}{\textit{Decls}}"
%format missingExpr    = "\withSub{missing}{\textit{Expr}}"


In order to illustrate the convenience of our embedding, we will consider the problem of analyzing the semantic correctness of certain functional programming excerpts. In particular, we will focus on |let| expressions that are common in functional languages like Haskell~\cite{}, ML~\cite{}, or Scala~\cite{}. Throughout the paper, we shall refer to these expressions as programs in the \Let\ language.

A program in the \Let\ language consists of instruction blocks, where each instruction  either: i) declares a variable or ii) defines a nested block.  When declaring a variable, the programmer must also assigned a value to it; such value may be a constant, the value of a variable or an expression on these elements. Ultimately, each block defines a value as an expression on variables defined in it.

Examples of programs in \Let\ are given next.


\begin{spec}
program1 = let x = 4 in x

program2 = let x = 4 in x + 3

program3 =
  let x = 4
      y = let w = 2 in x + w
  in  x + y
\end{spec}

The values associated with |program1| and |program2| are, quite straightforwardly, |4| and |7|, respectively. As of |program3|, its value of |10| is calculated adding the value of |x|, which is |4| and the value of |y|, i.e., |6|. The value of |y| is obtained by adding the value of |w|, which is |2|, and the value of |x|, which, again, is |4|.

Our goal is to implement a semantic analyzer that deals with the scope rules of the \Let\ language. These rules are quite natural and can be described as:

\begin{enumerate}
\item if a variable is used in a block, it must be declared in that same block or in an outer one. The declaration of a variable, however, may occur after its use;

\item a variable identifier may be declared at most once within the same block. In an inner block, declaring a variable that has already been declared in an outer one is allowed: the identifier in the local scope hides the definition of the same identifier in the global one.

\end{enumerate}

Let us now consider a more complex \Let\ program:

\begin{spec}
programe =
  let x = y
      a = let y = 4 in y + w
      x = 5
      y = 6
  in  x + a
\end{spec}

According to the scope rules that we have just defined, |programe| contains two errors: 1) at the outer block, variable |x| has been declared twice; and 2) at the inner block, the use of variable |w| has no binding occurrence at all. Notice that |y| has been declared at both the inner and the outer levels, which in itself is not a problem (the inner declaration hides the outer one).

Programs such as the ones we have presented describe the basic block-structure found in many languages, with the peculiarity that variables can be used before they are defined.

We aim to implement a program that analyses \Let\ programs and computes a list containing the identifiers which do not obey the scope rules of the language. In order to facilitate the debugging phase, we require that the list of invalid identifiers follows the sequential structure of the program. That is to say, e.g., that the semantic meaning of processing |programe| is |[w, x]|: the use of the undeclared variable |w| occurs in line 3 whereas the duplicate declaration of |x| occurs in line 4.

Since variables can be used before they are declared, a natural way to implement such an analysis is to traverse a \Let\ program twice: once to accumulate the declarations of identifiers (at each block), and again to check the uses of identifiers against such declarations. The uniqueness of declarations can already be detected in the first traversal: for each newly encountered declaration, it is possible to check whether its identifier has already been declared (in the same level).

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

In this representation, |programe| above is defined as:

\begin{code}
programe = Let
  (Cons
    "x"
    (Variable "y")
    (Nested
      "a"
      (Let (Cons "y" (Const 4) Empty) (Plus (Variable "y") (Variable "w")))
      (Cons "x" (Const 5) (Cons "y" (Const 6) Empty))
    )
  )
  (Plus (Variable "x") (Variable "a"))
\end{code}

Now, we implement name analysis on an abstract \Let\ tree using a set of composable functions.

The function that implements the first traversal described above needs to pass all the information that is required for the second traversal. Namely, in order to compute the final list of errors in the desired order, the second traversal needs to \textit{where} errors have occurred during the first. Also, the value of the nesting level must also be carried around because it is on the second traversal that the first traversal on nested expressions starts (the initial environment of an inner block is composed by the complete environment of its outer one).

In order to make available all the information that the second traversal needs, the first traversal will build a structure such as:

\begin{code}
data Let2   = Let2 Decls2 Expr
data Decls2 = Empty2
            | Cons2 Errors Expr Decls2
            | Nested2 Errors Lev Let Decls2
\end{code}


We are now ready to implement the two traversal strategy that we have described.

\begin{code}
type Errors  = [String]
type Lev     = Int

semantics :: Let -> Errors
semantics program = errors
 where
  (let2, env) = duplicateLet program [] 0
  errors       = missingLet let2 env

duplicateLet :: Let -> [(Var, Lev)] -> Lev -> (Let2, [(Var, Lev)])
duplicateLet (Let decls expr) dcli lev = (Let2 decls2 expr, dclo)
  where (decls2, dclo) = duplicateDecls decls dcli lev

duplicateDecls :: Decls -> [(Var, Lev)] -> Lev -> (Decls2, [(Var, Lev)])
duplicateDecls Empty dcli lev = (Empty2, dcli)
duplicateDecls (Cons var expr decls) dcli lev =
  (Cons2 error expr decls2, dclo)
 where
  error           = if (var, lev) `elem` dcli then [var] else []
  (decls2, dclo) = duplicateDecls decls ((var, lev) : dcli) lev
duplicateDecls (Nested var nested decls) dcli lev =
  (Nested2 error (lev + 1) nested decls2, dclo)
 where
  error           = if (var, lev) `elem` dcli then [var] else []
  (decls2, dclo) = duplicateDecls decls ((var, lev) : dcli) lev

missingLet :: Let2 -> [(Var, Lev)] -> Errors
missingLet (Let2 decls expr) env = errors1 ++ errors2
 where
  errors1 = missingDecls decls env
  errors2 = missingExpr expr env

missingDecls :: Decls2 -> [(Var, Lev)] -> Errors
missingDecls (Cons2 error expr decls) env = error ++ errors
  where errors = missingExpr expr env ++ missingDecls decls env
missingDecls (Nested2 error lev nested decls) env = error ++ errors
 where
  (nested2, dclo) = duplicateLet nested env lev
  errors           = missingLet nested2 dclo ++ missingDecls decls env
missingDecls Empty2 _ = []

missingExpr :: Expr -> [(Var, b)] -> Errors
missingExpr (Const _) _ = []
missingExpr (Plus expr1 expr2) env =
  missingExpr expr1 env ++ missingExpr expr2 env
missingExpr (Times expr1 expr2) env =
  missingExpr expr1 env ++ missingExpr expr2 env
missingExpr (Variable var) env = if var `elem` map fst env then [] else [var]
\end{code}

Notice that |duplicateLet| not only computes the total environment (using an initially empty accumulating parameter), but it also computes a |Let2| intermediate data structure that stores, e.g., the duplicated variables detected during the first traversal. The second traversal starts with a call to |missingLet| giving that computed data structure and the accumulated environment as arguments. It produces the list of errors that follows the sequential structure of the program.

In function |duplicateDecls|, for every block we compute: its environment, its level and its invalid identifiers. The environment defines the context where the block occurs. It consists of all the identifiers that are visible in the block (annotated with the level of the block). The level indicates the nesting depth of a block. Observe that we have to distinguish between the same identifier declared at different levels, which is valid.

Finally, please note that in the second traversal of a nested expression, in function |missingDecls| for the constructor |Nested2|, the program performs the two traversals to the body of that expression: calls |duplicateLet| and |missingLet|.

Although the semantic analysis we have implemented for the \Let\ language is relatively simple, still we had to face some challenges.

Indeed, scheduling computations was by no means trivial, with intermingled recursive functions. Also, we had to carefully design and implement intermediate data structures in order to convey data between traversals. These challenges are common to functional programming solutions to realistic programming problems.

In lazy (functional) programming languages, one can avoid both the need for scheduling and for additional data structures by constructing circular programs~\cite{Bird84}. This strategy, however, compromises the much desired modular nature of the implementations.

In our work, we seek an elegant and efficient alternative to the construction of functional programs.

