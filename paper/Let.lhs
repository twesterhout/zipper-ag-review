
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

Our goal is to implement a semantic analyzer that deals with the scope rules of the \Let\ language. These rules are quite natural and can be described as: 

\begin{enumerate}
\item if a variable is used in a block, it must be declared in that same block or in an outer one. The declaration of a variable, however, may occur after its use;

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

Now, we implement name analysis on an abstract \Let\ tree using a set of composable functions. 

The function that implements the first traversal described above needs to pass all the information that is required for the second traversal. Namely, in order to compute the final list of errors in the desired order, the second traversal needs to \textit{where} errors have occurred during the first. Also, the value of the nesting level must also be carried around because it is on the second traversal that the first traversal on nested expressions starts (the initial environment of an inner block is composed by the complete environment of its outer one).

%format Let_2
%format Decls_2
%format Cons_2
%format Empty_2
%format Nested_2
%format let_2
%format decls_2
%format errors_1
%format errors_2
%format nested_2
%format expr_1
%format expr_2
%format duplicate_Let duplicate_"Let"
%format duplicate_Decls duplicate_"Decls"
%format missing_Let missing_"Let"
%format missing_Decls missing_"Decls"
%format missing_Expr missing_"Expr"

In order to make available all the information that the second traversal needs, the first traversal will build a structure such as:

\begin{code}

data Let_2    = Let_2 Decls_2 Expr

data Decls_2  =  Empty_2
              |  Cons_2    Errors Expr  Decls_2
              |  Nested_2  Errors Lev Let   Decls_2

\end{code}


We are now ready to implement the two traversal strategy that we have described.

\begin{code}

type Errors  = [String]
type Lev     = Int

semantics :: Let -> Errors
semantics program = errors
    where  (let_2, env)  = duplicate_Let program [] 0
           errors        = missing_Let let_2 env

duplicate_Let :: Let -> [(Var, Lev)] -> Lev -> (Let_2, [(Var, Lev)])
duplicate_Let (Let decls expr) dcli lev = (Let_2 decls_2 expr, dclo)
    where  (decls_2, dclo) = duplicate_Decls decls dcli lev 



duplicate_Decls :: Decls -> [(Var, Lev)] -> Lev -> (Decls_2, [(Var, Lev)])
duplicate_Decls Empty dcli lev = (Empty_2, dcli)
    
duplicate_Decls (Cons var expr decls) dcli lev = (Cons_2 error expr decls_2, dclo)
    where  error  = if (var, lev) `elem` dcli then [var] else []
           (decls_2, dclo) = duplicate_Decls decls ((var, lev):dcli) lev
         
duplicate_Decls (Nested var nested decls) dcli lev = 
  (Nested_2 error (lev+1) nested decls_2, dclo)
    where  error = if (var, lev) `elem` dcli then [var] else []
           (decls_2, dclo) = duplicate_Decls decls ((var, lev):dcli) lev


missing_Let :: Let_2 -> [(Var, Lev)] -> Errors
missing_Let (Let_2 decls expr) env = errors_1 ++ errors_2
    where  errors_1 = missing_Decls decls env
           errors_2 = missing_Expr  expr env
           
           
missing_Decls :: Decls_2 -> [(Var, Lev)] -> Errors           
missing_Decls (Cons_2 error expr decls) env = error ++ errors
    where  errors = missing_Expr expr env ++ missing_Decls decls env

missing_Decls (Nested_2 error lev nested decls) env = error ++ errors
    where  (nested_2, dclo) = duplicate_Let nested env lev
           errors = missing_Let nested_2 dclo ++ missing_Decls decls env

missing_Decls Empty_2 _ = []


missing_Expr :: Expr -> [(Var, b)] -> Errors
missing_Expr (Const _) _ = []

missing_Expr (Plus expr_1 expr_2) env = 
  missing_Expr expr_1 env ++ missing_Expr expr_2 env

missing_Expr (Times expr_1 expr_2) env = 
  missing_Expr expr_1 env ++ missing_Expr expr_2 env

missing_Expr (Variable var) env = if var `elem` map fst env then [] else [var]
\end{code}

Notice that |duplicate_Let| not only computes the total environment (using an initially empty accumulating parameter), but it also computes a |Let_2| intermediate data structure that stores, e.g., the duplicated variables detected during the first traversal. The second traversal starts with a call to |missing_let| giving that computed data structure and the accumulated environment as arguments. It produces the list of errors that follows the sequential structure of the program.

In function |duplicate_Decls|, for every block we compute: its environment, its level and its invalid identifiers. The environment defines the context where the block occurs. It consists of all the identifiers that are visible in the block (annotated with the level of the block). The level indicates the nesting depth of a block. Observe that we have to distinguish between the same identifier declared at different levels, which is valid. 

Finally, please note that in the second traversal of a nested expression, in function |missing_Decls| for the constructor |Nested_2|, the program performs the two traversals to the body of that expression: calls |duplicate_Let| and |missing_Let|,













