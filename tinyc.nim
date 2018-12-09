import strutils, parseutils, tables;

discard """

  Copyright (C) 2001 by Marc Feeley, All Rights Reserved. */

 * This is a compiler for the Tiny-C language.  Tiny-C is a
 * considerably stripped down version of C and it is meant as a
 * pedagogical tool for learning about compilers.  The integer global
 * variables "a" to "z" are predefined and initialized to zero, and it
 * is not possible to declare new variables.  The compiler reads the
 * program from standard input and prints out the value of the
 * variables that are not zero.  The grammar of Tiny-C in EBNF is:
 *
 *  <program> ::= <statement>
 *  <statement> ::= "if" <paren_expr> <statement> |
 *                  "if" <paren_expr> <statement> "else" <statement> |
 *                  "while" <paren_expr> <statement> |
 *                  "do" <statement> "while" <paren_expr> ";" |
 *                  "{" { <statement> } "}" |
 *                  <expr> ";" |
 *                  ";"
 *  <paren_expr> ::= "(" <expr> ")"
 *  <expr> ::= <test> | <id> "=" <expr>
 *  <test> ::= <sum> | <sum> "<" <sum>
 *  <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
 *  <term> ::= <id> | <int> | <paren_expr>
 *  <id> ::= "a" | "b" | "c" | "d" | ... | "z"
 *  <int> ::= <an_unsigned_decimal_integer>
 *
 * Here are a few invocations of the compiler:
 *
 * % echo "a=b=c=2<3;" | ./a.out
 * a = 1
 * b = 1
 * c = 1
 * % echo "{ i=1; while (i<100) i=i+i; }" | ./a.out
 * i = 128
 * % echo "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }" | ./a.out
 * i = 25
 * j = 25
 * % echo "{ i=1; do i=i+10; while (i<50); }" | ./a.out
 * i = 51
 * % echo "{ i=1; while ((i=i+10)<50) ; }" | ./a.out
 * i = 51
 * % echo "{ i=7; if (i<5) x=1; if (i<10) y=2; }" | ./a.out
 * i = 7
 * y = 2
 *
 * The compiler does a minimal amount of error checking to help
 * highlight the structure of the compiler.
 """;

#---------------------------------------------------------------------------
# Lexer

type
  TType = enum 
    START, DO, ELSE, IF, WHILE, LBRA, RBRA, LPAR, RPAR, PLUS, MINUS, LESS, MORE, SEMI, EQUAL, INT, ID, END 
  SyntaxError = object of Exception
  Lexer = ref object 
    src: string
    pos: int
  Token = ref object
    sym: TType
    name: string
    ival: int
    pos: int

proc int_to_char( n : int ) : char = ( ord('a') + n ).char

proc char_to_int( c : char ) : int = ord(c) - ord('a')


let words = { "do": DO, "else": ELSE, "if": IF, "while": WHILE }.toTable;

proc len( lexer: Lexer ) : int = (lexer.src.len - lexer.pos)

proc advance( lexer: var Lexer, n = 1 ) =
  assert( n > 0 )
  lexer.pos = min( lexer.pos + n, lexer.src.len )

proc newLexer( src: string ) : Lexer = 
  new(result)
  result.src = src
  result.pos = 0

proc reset( lexer: Lexer, token: Token ) =
  lexer.pos = token.pos

proc more( t : Lexer ) : bool =  t.src.len > t.pos

proc show( lexer: Lexer ) : string =
  if lexer.more:
    result = lexer.src.substr(lexer.pos)

proc token( lexer:var Lexer, tt: TType, size = 0, ival = 0 ) : Token =
  assert(size>0 or tt==END)
  new(result)
  result.sym = tt
  if size > lexer.len:
    result.name = lexer.src.substr(lexer.pos)
  else:
    result.name = lexer.src.substr( lexer.pos, lexer.pos+size-1 )
  result.ival = ival
  result.pos = lexer.pos
  lexer.advance(size)



proc look( lexer: Lexer, n : int = 0 ) : char = 
  if n + lexer.pos >= lexer.src.len:
    return 0.char
  else:
    return lexer.src[lexer.pos+n]



proc skipwhite(lexer: var Lexer ) =
  var n = 0
  while lexer.look(n) in Whitespace:
    inc(n)
  if n>0: lexer.advance(n)


proc syntax_error( lexer: Lexer, msg: string ) =
  let ex = newException( SyntaxError, "syntax error ($1) at '$2'\n".format(msg, lexer.show) )
  raise ex

let symbols = {
          '{': LBRA, 
          '}': RBRA,
          '(': LPAR, 
          ')': RPAR,
          '+': PLUS,
          '-': MINUS,
          '<': LESS,
          '>': MORE,
          ';': SEMI,
          '=': EQUAL
    }.toTable;


proc scan( lexer:var Lexer ) : Token =
  lexer.skipwhite()
  if not lexer.more:
    new(result)
    result.sym = END
    return result
  var ch = lexer.look()
  if ch in symbols:
    result = lexer.token( symbols[ch], 1)
  elif ch in Digits:
    var int_val = 0
    var count = 0
    while ch in Digits:
      int_val = ( int_val*10 ) + (ord(ch) - ord('0'))
      inc( count )
      ch = lexer.look(count)
    result = lexer.token(INT, count, int_val) 
  elif ch in IdentStartChars:
    var name = ""
    var count = 0
    while lexer.look(count) in IdentChars:
      name &= lexer.look(count)
      inc(count)
    if name in words:
      result = lexer.token( words[name], count)
    elif name.len==1:
      result = lexer.token( ID, 1 );
    else:
      lexer.syntax_error("Invalid keyword or var:" & name )
  else:
    lexer.syntax_error("Unknown syntax");


#---------------------------------------------------------------------------
# Parser

type
  AST = enum
    VAR, CST, ADD, SUB, LT, GT, SET, IF1, IF2, WHILE_S, DO_S, EMPTY, SEQ, EXPR, PROG
  Node = ref object
    kind : AST
    o1, o2, o3 : Node
    val : int


proc margin( n : int ) : string =
  var i = n
  while i > 0:
    result &= " "
    dec(i)

proc to_str(n: Node, indent: int = 0) : string = 
  if n==nil:
    return ""
  else:
    result = margin(indent)
    var sval = (if n.kind!=VAR: $n.val  else: $n.val.int_to_char  )
    result &= $n.kind & "(" & sval & ")"
    if n.o1!=nil: 
      result &= ("\nC1:" & to_str(n.o1, indent+1))
      if n.o2!=nil: 
        result &= ("\nC2:" & to_str(n.o2, indent+1))
        if n.o3!=nil: 
          result &= ("\nC3:" & to_str(n.o3, indent+1))
    result &= ")"

proc dump( n : Node ) =
  echo( "AST:\n" & to_str(n, 1) )

proc newNode(k: AST) : Node =
  new(result)
  result.kind = k


proc peek( lexer: var Lexer ) : Token =
  let save = lexer.pos
  result = lexer.scan()
  lexer.pos = save

proc skip( lexer: var Lexer, t : TType ) : bool = 
  let save = lexer.pos
  let tok = lexer.scan()
  if tok.sym==t:
    result = true
  else:
    lexer.pos = save


proc expect( lexer: var Lexer, t : TType ) : Token=
  let tok = lexer.scan()
  if tok.sym==t:
    return tok
  else:
    lexer.syntax_error("Expected: $1 but got $2".format( t, $tok.sym))


# forward declaration
proc paren_expr(lexer:var Lexer) : Node ;
proc expr(lexer: var Lexer) : Node ;

proc term(lexer:var Lexer) : Node = 
  let tok = lexer.scan()
  if tok.sym==ID:
    result = newNode(VAR)
    result.val = tok.name[0].char_to_int
  elif tok.sym==INT:
    result = newNode(CST)
    result.val = tok.ival
  elif tok.sym==LPAR:
    lexer.reset(tok); 
    result = paren_expr(lexer)
  else:
    lexer.syntax_error("Expression expected")


proc sum(lexer:var Lexer) : Node =
  result = term(lexer)
  var tok = lexer.peek()
  while tok.sym in [PLUS, MINUS]:
    discard lexer.scan()
    let t = result
    result = newNode(if tok.sym==PLUS: ADD else: SUB)
    result.o1 = t
    result.o2 = term(lexer)
    tok = lexer.peek()


proc test(lexer:var Lexer) : Node =
  result = sum(lexer)
  if lexer.skip(LESS):
    let t = result
    result = newNode(LT)
    result.o1 = t
    result.o2 = sum(lexer)
  elif lexer.skip(MORE):
    let t = result
    result =newNode(GT)
    result.o1 = t
    result.o2 = sum(lexer)

proc expr(lexer: var Lexer) : Node =
  result = test(lexer)
  if lexer.skip(EQUAL):
    if result.kind!=VAR: 
      lexer.syntax_error("Cannot assign to non variable!")
    else:
      let t = result
      result = newNode(SET) 
      result.o1 = t
      result.o2 = expr(lexer)


proc paren_expr(lexer: var Lexer) : Node =
  discard lexer.expect(LPAR)
  result = expr(lexer)
  discard lexer.expect(RPAR)


proc statement(lexer: var Lexer) : Node =
  if lexer.skip( IF ):
    result = newNode(IF1)
    result.o1 = paren_expr(lexer)
    result.o2 = statement(lexer)
    if lexer.skip(ELSE):
      result.kind = IF2
      result.o3 = statement(lexer)
  elif lexer.skip(WHILE):
    result = newNode(WHILE_S)
    result.o1 = paren_expr(lexer)
    result.o2 = statement(lexer)    
  elif lexer.skip(DO):
    result = newNode(DO_S)
    result.o1 = statement(lexer)
    discard lexer.expect(WHILE)
    result.o2 = paren_expr(lexer)
    discard lexer.expect(SEMI)
  elif lexer.skip( LBRA ):
    result = newNode(EMPTY)
    while not lexer.skip(RBRA):
      let t = result
      result = newNode(SEQ)
      result.o1 = t
      result.o2 = statement(lexer)
  elif lexer.skip( SEMI ):
    result = newNode(EMPTY)
  else:
    result = newNode(EXPR)
    result.o1 = expr(lexer)
    discard lexer.expect(SEMI)


proc program(lexer:var Lexer) : Node =
  discard lexer.skip(START)
  result = newNode(PROG)
  result.o1 = statement(lexer)
  discard lexer.expect(END)

#---------------------------------------------------------------------------
# Code generator

type
  Instr = enum
    HALT=0, IFETCH, ISTORE, IPUSH, IPOP, IADD, ISUB, ILT, IGT, JZ, JNZ, JMP
  Code = tuple[ instr: Instr, val: int ]
  CodeBlock = ref object
    code: array[1000,Code]
    index: int


proc makeObj() : CodeBlock =
  new(result)
  result.index = 0
  for index in 0..high(result.code):
    result.code[index] = (HALT, 0)


proc dump( dest: CodeBlock) =
  echo("$1 bytecodes".format(dest.index))
  for index in 0..dest.index:
    let (op, val) = dest.code[index]
    if op in [ISTORE, IFETCH]:
      echo "$1:$2 ($3)".format(index, op , $val.int_to_char )
    elif op in [JMP, JNZ, JZ]:
      echo "$1:$2 to $3".format(index, op , val+index) 
    else:
      echo "$1:$2 $3".format(index, op , val) 

proc gen(dest: CodeBlock, i: Instr, param: int) =
  dest.code[dest.index] = (instr: i, val: param)
  inc(dest.index)

# generate a bytecode but return the index to patch up later
proc mark(dest: CodeBlock, i: Instr) : int =
  result = dest.index
  dest.gen(i, 0)
  assert dest.code[result].instr == i

# 'fix' or patch up an existing bytecode 'loc' to jump to 'dest'
proc fix(dest: CodeBlock, inst, target: int ) =
  let cur = dest.code[inst]
  assert( cur.instr in [JMP, JNZ, JZ ])
  let offset = target - inst
  dest.code[inst] = (instr: cur.instr, val: offset )

proc fix(dest: CodeBlock, inst: int ) =
  dest.fix( inst, dest.index )

  

proc cc(dest: CodeBlock, x: Node) =
  var p1, p2 : int
  case(x.kind)
  of VAR: 
    dest.gen(IFETCH, x.val);
  of CST: 
    dest.gen(IPUSH, x.val);
  of ADD: 
    dest.cc( x.o1 ) 
    dest.cc(x.o2)   
    dest.gen(IADD, 0); 
  of SUB: 
    dest.cc( x.o1 )
    dest.cc(x.o2)
    dest.gen(ISUB, 0);
  of LT: 
    dest.cc( x.o1 )
    dest.cc(x.o2)
    dest.gen(ILT,0);
  of GT: 
    dest.cc(x.o1)
    dest.cc(x.o2)
    dest.gen(IGT,0);
  of SET: 
    dest.cc( x.o2 )
    dest.gen(ISTORE, x.o1.val);
  of IF1: 
    dest.cc(x.o1); 
    p1 = dest.mark(JZ); 
    dest.cc(x.o2); 
    dest.fix(p1); 
  of IF2: 
    dest.cc(x.o1);
    p1=dest.mark(JZ); 
    dest.cc(x.o2); 
    p2=dest.mark(JMP);
    dest.fix(p1); 
    dest.cc(x.o3); 
    dest.fix(p2); 
  of WHILE_S: 
    p1 = dest.index
    dest.cc(x.o1)
    p2 = dest.mark(JZ)
    dest.cc(x.o2)
    dest.gen( JMP, p1 - dest.index )
    dest.fix(p2)
  of DO_S: 
    p1 = dest.index
    dest.cc(x.o1)
    dest.cc(x.o2)
    dest.gen(JNZ, p1)
  of EMPTY: 
    discard
  of SEQ: 
    dest.cc(x.o1); 
    dest.cc(x.o2);
  of EXPR: 
    dest.cc(x.o1)
    dest.gen(IPOP,0)
  of PROG: 
    dest.cc(x.o1)
    dest.gen(HALT,0)
    assert dest.code[dest.index-1].instr==HALT


#---------------------------------------------------------------------------
# Virtual machine

type
  VM = ref object
    globals: array[26, int]

proc newVM(): VM = 
  new(result)
  for n in 0..high(result.globals):
    result.globals[n] = 0

proc run(vm : var VM, obj: CodeBlock) =
  var 
    stack: array[1000,int]
    sp: int = 0
    pc: int = 0
  while pc < high(stack):
    let code = obj.code[pc]
    case code.instr
    of IFETCH:
      stack[sp]= vm.globals[code.val]
      inc(sp)
      inc(pc)
    of ISTORE:  
      vm.globals[code.val] = stack[sp-1]
      inc(pc)
    of IPUSH:
      stack[sp] = code.val
      inc(sp)
      inc(pc)
    of IPOP: 
      dec(sp)
      inc(pc)
    of IADD:
      dec(sp)
      stack[sp-1] = stack[sp-1] + stack[sp]
      inc(pc)
    of ISUB:
      stack[sp-2] = stack[sp-2] - stack[sp-1]
      dec(sp)
      inc(pc)
    of ILT:
      dec(sp)
      stack[sp-1] = (if stack[sp-1]<stack[sp]: 1 else: 0)
      inc(pc)
    of IGT:
      dec(sp)
      stack[sp-1] = (if stack[sp-1]>stack[sp]: 1 else: 0)      
      inc(pc)
    of JMP:
      inc(pc, code.val)
    of JZ:
      dec(sp)
      if stack[sp]==0:
        inc(pc, code.val)
      else:
        inc(pc)
    of JNZ:
      dec(sp)
      if stack[sp]!=0:
        inc( pc, code.val )
      else:
        inc( pc )
    of HALT:
      return
    else:
      echo("UNKNOWN bytecode:" & $code )
      quit(-1)

proc dump( vm : VM, v : char ) =
  let index = ord(v) - ord('a')
  echo "$1 = $2".format( v, vm.globals[index])

proc display( vm : VM ) =
  for index, val in vm.globals:
    if val!=0:
      let name = "" & chr( ord('a') + index )
      echo("$1 = $2".format( name , val ))

#---------------------------------------------------------------------------
# Tests

proc tests() =
  echo "\n**** TEST 0\n"
  var lex0 = newLexer("t 4 ( z )")
  discard lex0.expect(ID)
  discard lex0.expect(INT)
  discard lex0.expect(LPAR)
  discard lex0.expect(ID)
  discard lex0.expect(RPAR)  
  discard lex0.expect(END)
  discard lex0.expect(END)

  echo "\nn**** TEST 1\n"
  var lex1 = newLexer("100")
  lex1.expr().dump

  echo "\n**** TEST 2\n"
  var lex2 = newLexer("z")
  assert lex2.peek().sym==ID
  lex2.expr().dump

  echo "\n**** TEST 3\n"
  var lex3 = newLexer("z=200;")
  lex3.statement().dump

  echo "\n**** TEST 4\n"
  var lex4 = newLexer("z > 50")
  lex4.expr().dump
  assert lex4.peek().sym==END

  echo "\n**** TEST 5\n"
  var lex5 = newLexer("z+200> 50")
  lex5.expr().dump
  assert lex5.peek().sym==END, lex5.show


#---------------------------------------------------------------------------
# Main program

import parseopt;

proc main() =
  var 
    opt = initOptParser()
    src = ""
  for cmdType,key, val in opt.getopt():
    case cmdType
    of cmdArgument:
      src = readFile( key )
    of cmdLongOption, cmdShortOption:
      if key in [ "i", "interpret" ]:
        src = val
      elif key in [ "f", "file" ]:
        src = readFile(val)
      elif key in [ "t", "test" ]:
        tests()
      else:
        echo("Unknown option:" & key & " \n" )
    of cmdEnd:
      discard
  if src=="": 
    quit(0)
  else:
    echo( "\nSRC='$1'\n".format(src) )
  var lexer = newLexer(src)
  var ast =  program(lexer)
  ast.dump()
  var dest = makeObj()  
  dest.cc( ast )
  dest.dump()
  var vm = newVM()
  vm.run(dest)
  vm.display()


main()

