%{
    let (symb, appl, add_args) = Terms.(symb, appl, add_args)
%}
%token <string> ID
%token EQ PLUS UMINUS TIMES FACT EOF
%token
%nonassoc EQ
%left PLUS
%right TIMES
%nonassoc UMINUS FACT
%start expr1
%type <Terms.t> expr1
%%

aexpr:
| ID { symb $1 }

sexpr:
| sexpr aexpr { appl $1 $2 }
| aexpr { $1 }

expr:
| sexpr { $1 }
| sexpr EQ sexpr { add_args (symb "=") [$1; $3] }
| sexpr PLUS sexpr { add_args (symb "+") [$1; $3] }
| sexpr TIMES sexpr { add_args (symb "*") [$1; $3] }
| UMINUS sexpr { appl (symb "-") $2 }
| sexpr FACT {appl (symb "!") $1}

expr1: expr EOF { $1 }
