{- This module was generated from data in the Kate syntax highlighting file nasm.xml, version 1.30,
   by  Nicola Gigante (nicola.gigante@gmail.com) -}

module Text.Highlighting.Kate.Syntax.Nasm ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Intel x86 (NASM)"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.asm"

-- | Highlight source code using this syntax definition.
highlight :: String -> Either String [SourceLine]
highlight input =
  case runParser parseSource startingState "source" input of
    Left err     -> Left $ show err
    Right result -> Right result

-- | Parse an expression using appropriate local context.
parseExpression :: GenParser Char SyntaxState LabeledSource
parseExpression = do
  st <- getState
  let oldLang = synStLanguage st
  setState $ st { synStLanguage = "Intel x86 (NASM)" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Intel x86 (NASM)",["Normal"])], synStLanguage = "Intel x86 (NASM)", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Comment" -> (popContext >> return ())
    "Preprocessor" -> (popContext >> return ())
    "String" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  if null txt
     then fail "Parser matched no text"
     else return ()
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Registers","Keyword"),("Instructions","Keyword"),("NASM Keywords","Keyword"),("Comment","Comment"),("Label","Function"),("Data","DataType"),("BaseN","BaseN"),("Float","Float"),("Number","DecVal"),("Char","Char"),("String","String"),("Preprocessor","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Comment","Comment"),("Preprocessor","Preprocessor"),("String","String")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["eax","ax","ah","al","ebx","bx","bh","bl","ecx","cx","ch","cl","edx","dx","dh","dl","ebp","bp","esi","si","edi","di","esp","sp","cs","ds","es","fs","gs","ss","cr0","cr2","cr3","cr4","dr0","dr1","dr2","dr3","dr6","dr7","st","mm0","mm1","mm2","mm3","mm4","mm5","mm6","mm7","xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7"] >>= withAttribute "Registers"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["times","equ","db","dw","dd","dq","dt","resb","resw","resd","resq","rest","incbin","byte","word","dword","qword","short","ptr"] >>= withAttribute "Data"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["aaa","aad","aam","aas","adc","add","addpd","addps","addsd","addss","addsubpd","addsubps","and","andnpd","andnps","andpd","andps","arpl","bound","bsf","bsr","bswap","bt","btc","btr","bts","call","cbw","cwde","cwd","cdq","cdqe","cqo","clc","cld","clgi","cli","clts","clflush","cmc","cmova","cmovae","cmovb","cmovbe","cmovc","cmove","cmovg","cmovge","cmovl","cmovle","cmovna","cmovnae","cmovnb","cmovnbe","cmovnc","cmovne","cmovng","cmovnge","cmovnl","cmovnle","cmovno","cmovnp","cmovns","cmovnz","cmovo","cmovp","cmovpe","cmovpo","cmovs","cmovz","cmp","cmpeqpd","cmpeqps","cmpeqsd","cmpeqss","cmplepd","cmpleps","cmplesd","cmpless","cmpltpd","cmpltps","cmpltsd","cmpltss","cmpneqpd","cmpneqps","cmpneqsd","cmpneqss","cmpnlepd","cmpnleps","cmpnlesd","cmpnless","cmpnltpd","cmpnltps","cmpnltsd","cmpnltss","cmpordpd","cmpordps","cmpordsd","cmpordss","cmppd","cmpps","cmps","cmpsb","cmpsd","cmpss","cmpsw","cmpunordpd","cmpunordps","cmpunordsd","cmpunordss","cmpxchg","cmpxchg486","cmpxchg8b","cmpxchg16b","comisd","comiss","cpuid","cvtdq2pd","cvtdq2ps","cvtpd2dq","cvtpd2pi","cvtpd2ps","cvtpi2pd","cvtpi2ps","cvtps2dq","cvtps2pd","cvtps2pi","cvtsd2si","cvtsd2ss","cvtsi2sd","cvtsi2ss","cvtss2sd","cvtss2si","cvttpd2dq","cvttpd2pi","cvttps2dq","cvttps2pi","cvttsd2si","cvttss2si","daa","das","dec","div","divpd","divps","divsd","divss","emms","enter","f2xm1","fabs","fadd","faddp","fbld","fbstp","fchs","fclex","fnclex","fcmovb","fcmovbe","fcmove","fcmovnb","fcmovnbe","fcmovne","fcmovnu","fcmovu","fcom","fcomp","fcompp","fcomi","fcomip","fcos","fdecstp","fdisi","feni","fdiv","fdivr","fdivp","fdivrp","femms","ffree","ffreep","fiadd","ficom","ficomp","fidiv","fidivr","fild","fimul","fincstp","finit","fist","fistp","fisttp","fisub","fisubr","fld","fld1","fldl2e","fldl2t","fldlg2","fldln2","fldcw","fldenv","fldpi","fldz","fmul","fmulp","fndisi","fneni","fninit","fnop","fnsave","fnstcw","fnstenv","fnstsw","fnwait","fpatan","fptan","fprem","fprem1","frndint","frstor","fsave","fscale","fsetpm","fsin","fsincos","fsqrt","fst","fstp","fstcw","fstenv","fstsw","fsub","fsubr","fsubp","fsubrp","ftst","fucom","fucomp","fucompp","fucomi","fucomip","fwait","fxam","fxch","fxrstor","fxsave","fxtract","fyl2x","fyl2xp1","haddpd","haddps","hlt","hsubpd","hsubps","ibts","idiv","imul","in","inc","ins","insb","insd","insw","int","int1","int3","into","invd","invlpg","invlpga","iret","iretd","iretq","iretw","ja","jae","jb","jbe","jc","je","jg","jge","jl","jle","jna","jnae","jnb","jnbe","jnc","jne","jng","jnge","jnl","jnle","jno","jnp","jns","jnz","jo","jp","jpe","jpo","js","jz","jcxz","jecxz","jrcxz","jmp","lahf","lar","lddqu","ldmxcsr","lds","les","lea","leave","lfence","lfs","lgdt","lgs","lidt","lldt","lmsw","loadall","loadall286","lods","lodsb","lodsd","lodsq","lodsw","loop","loope","loopne","loopnz","loopz","lsl","lss","ltr","maskmovdqu","maskmovq","maxpd","maxps","maxsd","maxss","mfence","minpd","minps","minsd","minss","monitor","mov","movapd","movaps","movd","movddup","movdq2q","movdqa","movdqu","movhlps","movhpd","movhps","movlhps","movlpd","movlps","movmskpd","movmskps","movntdq","movnti","movntpd","movntps","movntq","movq","movq2dq","movs","movsb","movsd","movshdup","movsldup","movsq","movss","movsx","movsxd","movsw","movupd","movups","movzx","mul","mulpd","mulps","mulsd","mulss","mwait","neg","nop","not","or","orpd","orps","out","outs","outsb","outsw","outsd","packssdw","packsswb","packuswb","paddb","paddd","paddq","paddsb","paddsw","paddusb","paddusw","paddw","pand","pandn","pause","pavgb","pavgusb","pavgw","pcmpeqb","pcmpeqw","pcmpeqd","pcmpgtb","pcmpgtw","pcmpgtd","pdistib","pextrw","pf2id","pf2iw","pfacc","pfadd","pfcmpeq","pfcmpge","pfcmpgt","pfmax","pfmin","pfmul","pfnacc","pfpnacc","pfrcp","pfrcpit1","pfrcpit2","pfrsqit1","pfrsqrt","pfsub","pfsubr","pi2fd","pi2fw","pinsrw","pmachriw","pmaddwd","pmagw","pmaxsw","pmaxub","pminsw","pminub","pmovmskb","pmulhrw","pmulhuw","pmulhw","pmullw","pmuludq","pmvgezb","pmvlzb","pmvnzb","pmvzb","pop","popa","popaw","popad","popf","popfw","popfd","popfq","por","prefetch","prefetchnta","prefetcht0","prefetcht1","prefetcht2","prefetchw","psadbw","pshufd","pshufhw","pshuflw","pshufw","pslld","pslldq","psllq","psllw","psrad","psraw","psrld","psrldq","psrlq","psrlw","psubb","psubd","psubq","psubsb","psubsiw","psubsw","psubusb","psubusw","psubw","pswapd","punpckhbw","punpckhdq","punpckhqdq","punpckhwd","punpcklbw","punpckldq","punpcklqdq","punpcklwd","push","pusha","pushad","pushaw","pushf","pushfd","pushfq","pushfw","pxor","rcl","rcr","rcpps","rcpss","rdmsr","rdpmc","rdshr","rdtsc","rdtscp","ret","retf","retn","rol","ror","rsdc","rsldt","rsm","rsqrtps","rsqrtss","rsts","sahf","sal","sar","salc","sbb","scas","scasb","scasd","scasq","scasw","seta","setae","setb","setbe","setc","sete","setg","setge","setl","setle","setna","setnae","setnb","setnbe","setnc","setne","setng","setnge","setnl","setnle","setno","setnp","setns","setnz","seto","setp","setpe","setpo","sets","setz","sfence","sgdt","shl","shld","shr","shrd","shufpd","shufps","sidt","skinit","sldt","smi","smint","smintold","smsw","sqrtpd","sqrtps","sqrtsd","sqrtss","stc","std","stgi","sti","stmxcsr","stos","stosb","stosd","stosq","stosw","str","sub","subpd","subps","subsd","subss","svdc","svldt","svts","swapgs","syscall","sysenter","sysexit","sysret","test","ucomisd","ucomiss","ud0","ud1","ud2","umov","unpckhpd","unpckhps","unpcklpd","unpcklps","verr","verw","vmload","vmmcall","vmrun","vmsave","wait","wbinvd","wrmsr","wrshr","xadd","xbts","xchg","xlat","xlatb","xor","xorpd","xorps"] >>= withAttribute "Instructions"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["absolute","bits","common","extern","global","org","section","seg","segment","strict","use16","use32","wrt","struc","endstruc","istruc","at","iend","align","alignb","__SECT__","__NASM_MAJOR__","__NASM_MINOR__","__NASM_SUBMINOR__","___NASM_PATCHLEVEL__","__NASM_VERSION_ID__","__NASM_VER__","__FILE__","__LINE__"] >>= withAttribute "NASM Keywords"))
                        <|>
                        ((pDetectChar False ';' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pAnyChar "\"'" >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\s*[A-Za-z0-9_.$]+:") >>= withAttribute "Label"))
                        <|>
                        ((pRegExpr (compileRegex "(cmov|fcmov|j|loop|set)(a|ae|b|be|c|e|g|ge|l|le|na|nae|nb|nbe|nc|ne|ng|nge|nl|nle|no|np|ns|nz|o|p|pe|po|s|z)") >>= withAttribute "Instructions"))
                        <|>
                        ((pRegExpr (compileRegex "cpu (pentium|ppro|p2|p3|katmai|p4|willamette|prescott|ia64)*") >>= withAttribute "NASM Keywords"))
                        <|>
                        ((pRegExpr (compileRegex "(^|[ \\t,]+)((\\$|0x){1}[0-9]+[a-f0-9]*|[a-f0-9]+h)([ \\t,]+|$)") >>= withAttribute "BaseN"))
                        <|>
                        ((pRegExpr (compileRegex "(^|[ \\t,]+)([0-7]+(q|o)|[01]+b)([ \\t,]+|$)") >>= withAttribute "BaseN"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Number"))
                        <|>
                        ((pHlCOct >>= withAttribute "BaseN"))
                        <|>
                        ((pHlCHex >>= withAttribute "BaseN"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Number"))
                        <|>
                        ((pHlCChar >>= withAttribute "Char")))
     return (attr, result)

parseRules "Comment" = 
  pzero

parseRules "Preprocessor" = 
  pzero

parseRules "String" = 
  do (attr, result) <- ((pAnyChar "\"'" >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
