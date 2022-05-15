{
  open UarithGram
}
rule token = parse
| [ ' ' '\t' '\n' ] { token lexbuf }
| '=' { EQ }
| '+' { PLUS }
| '-' { UMINUS }
| '*' { TIMES }
| '!' { FACT }
| ['a'-'z' 'A'-'Z' '0'-'9']+ as id { ID id }
| eof { EOF }
