(define-module (thales core-modules)
    #:export (guile-provides?))

(define (guile-provides? modname)
    (and (member modname *guile-core-modules*)
	 'guile))

(define *guile-core-modules* '((value-history) ;; ??
			       (guile)
			       (ice-9 peg cache)
			       (ice-9 peg using-parsers)
			       (ice-9 peg string-peg)
			       (ice-9 peg codegen)
			       (ice-9 peg simplify-tree)
			       (ice-9 time)
			       (ice-9 list)
			       (ice-9 iconv)
			       (ice-9 slib)
			       (ice-9 poe)
			       (ice-9 compile-psyntax)
			       (ice-9 quasisyntax)
			       (ice-9 arrays)
			       (ice-9 pretty-print)
			       (ice-9 posix)
			       (ice-9 ls)
			       (ice-9 history)
			       (ice-9 hcons)
			       (ice-9 null)
			       (ice-9 receive)
			       (ice-9 r5rs)
			       (ice-9 top-repl)
			       (ice-9 occam-channel)
			       (ice-9 stack-catch)
			       (ice-9 safe)
			       (ice-9 syncase)
			       (ice-9 boot-9)
			       (ice-9 mapping)
			       (ice-9 threads)
			       (ice-9 test)
			       (ice-9 rw)
			       (ice-9 i18n)
			       (ice-9 getopt-long)
			       (ice-9 optargs)
			       (ice-9 curried-definitions)
			       (ice-9 control)
			       (ice-9 local-eval)
			       (ice-9 poll)
			       (ice-9 vlist)
			       (ice-9 lineio)
			       (ice-9 scm-style-repl)
			       (ice-9 rdelim)
			       (ice-9 weak-vector)
			       (ice-9 string-fun)
			       (ice-9 command-line)
			       (ice-9 binary-ports)
			       (ice-9 futures)
			       (ice-9 deprecated)
			       (ice-9 regex)
			       (ice-9 match)
			       (ice-9 psyntax)
			       (ice-9 format)
			       (ice-9 match.upstream)
			       (ice-9 debug)
			       (ice-9 and-let-star)
			       (ice-9 documentation)
			       (ice-9 popen)
			       (ice-9 psyntax-pp)
			       (ice-9 peg)
			       (ice-9 eval)
			       (ice-9 channel)
			       (ice-9 session)
			       (ice-9 calling)
			       (ice-9 runq)
			       (ice-9 networking)
			       (ice-9 r6rs-libraries)
			       (ice-9 eval-string)
			       (ice-9 safe-r5rs)
			       (ice-9 serialize)
			       (ice-9 streams)
			       (ice-9 buffered-input)
			       (ice-9 gap-buffer)
			       (ice-9 expect)
			       (ice-9 save-stack)
			       (ice-9 ftw)
			       (ice-9 common-list)
			       (ice-9 q)
			       (language value spec)
			       (language glil spec)
			       (language glil compile-assembly)
			       (language ecmascript array)
			       (language ecmascript tokenize)
			       (language ecmascript compile-tree-il)
			       (language ecmascript spec)
			       (language ecmascript function)
			       (language ecmascript impl)
			       (language ecmascript parse)
			       (language ecmascript base)
			       (language bytecode spec)
			       (language elisp runtime function-slot)
			       (language elisp runtime value-slot)
			       (language elisp falias)
			       (language elisp compile-tree-il)
			       (language elisp parser)
			       (language elisp spec)
			       (language elisp bindings)
			       (language elisp lexer)
			       (language elisp runtime)
			       (language cps compile-rtl)
			       (language cps contification)
			       (language cps spec)
			       (language cps reify-primitives)
			       (language cps primitives)
			       (language cps closure-conversion)
			       (language cps dfg)
			       (language cps slot-allocation)
			       (language cps arities)
			       (language cps verify)
			       (language scheme compile-tree-il)
			       (language scheme spec)
			       (language scheme decompile-tree-il)
			       (language tree-il inline)
			       (language tree-il peval)
			       (language tree-il fix-letrec)
			       (language tree-il canonicalize)
			       (language tree-il compile-cps)
			       (language tree-il cse)
			       (language tree-il spec)
			       (language tree-il primitives)
			       (language tree-il analyze)
			       (language tree-il debug)
			       (language tree-il optimize)
			       (language tree-il effects)
			       (language tree-il compile-glil)
			       (language objcode elf)
			       (language objcode spec)
			       (language rtl spec)
			       (language assembly spec)
			       (language assembly compile-bytecode)
			       (language assembly decompile-bytecode)
			       (language assembly disassemble)
			       (language brainfuck compile-tree-il)
			       (language brainfuck spec)
			       (language brainfuck compile-scheme)
			       (language brainfuck parse)
			       (language objcode)
			       (language rtl)
			       (language glil)
			       (language assembly)
			       (language tree-il)
			       (language cps)
			       (oop goops active-slot)
			       (oop goops compile)
			       (oop goops simple)
			       (oop goops stklos)
			       (oop goops describe)
			       (oop goops accessors)
			       (oop goops save)
			       (oop goops composite-slot)
			       (oop goops dispatch)
			       (oop goops util)
			       (oop goops internal)
			       (oop goops)
			       (texinfo plain-text)
			       (texinfo docbook)
			       (texinfo indexing)
			       (texinfo string-utils)
			       (texinfo html)
			       (texinfo reflection)
			       (texinfo serialize)
			       (web server http)
			       (web client)
			       (web server)
			       (web uri)
			       (web http)
			       (web request)
			       (web response)
			       (scripts help)
			       (scripts snarf-check-and-output-texi)
			       (scripts list)
			       (scripts lint)
			       (scripts read-text-outline)
			       (scripts compile)
			       (scripts read-scheme-source)
			       (scripts display-commentary)
			       (scripts generate-autoload)
			       (scripts use2dot)
			       (scripts frisk)
			       (scripts doc-snarf)
			       (scripts read-rfc822)
			       (scripts summarize-guile-TODO)
			       (scripts api-diff)
			       (scripts punify)
			       (scripts snarf-guile-m4-docs)
			       (scripts autofrisk)
			       (scripts scan-api)
			       (scripts disassemble)
			       (system base compile)
			       (system base ck)
			       (system base syntax)
			       (system base message)
			       (system base language)
			       (system base lalr.upstream)
			       (system base target)
			       (system base pmatch)
			       (system base lalr)
			       (system vm disassembler)
			       (system vm vm)
			       (system vm program)
			       (system vm coverage)
			       (system vm trap-state)
			       (system vm objcode)
			       (system vm frame)
			       (system vm elf)
			       (system vm linker)
			       (system vm traps)
			       (system vm inspect)
			       (system vm assembler)
			       (system vm debug)
			       (system vm instruction)
			       (system vm trace)
			       (system repl server)
			       (system repl common)
			       (system repl repl)
			       (system repl describe)
			       (system repl command)
			       (system repl debug)
			       (system repl error-handling)
			       (system xref)
			       (system foreign)
			       (sxml ssax input-parse)
			       (sxml upstream assert)
			       (sxml upstream SXPath-old)
			       (sxml upstream input-parse)
			       (sxml upstream SXML-tree-trans)
			       (sxml upstream SSAX)
			       (sxml simple)
			       (sxml xpath)
			       (sxml fold)
			       (sxml match)
			       (sxml ssax)
			       (sxml transform)
			       (sxml apply-templates)
			       (srfi srfi-67 compare)
			       (srfi srfi-42 ec)
			       (srfi srfi-9 gnu)
			       (srfi srfi-4 gnu)
			       (srfi srfi-16)
			       (srfi srfi-27)
			       (srfi srfi-4)
			       (srfi srfi-45)
			       (srfi srfi-38)
			       (srfi srfi-42)
			       (srfi srfi-26)
			       (srfi srfi-34)
			       (srfi srfi-69)
			       (srfi srfi-19)
			       (srfi srfi-18)
			       (srfi srfi-88)
			       (srfi srfi-13)
			       (srfi srfi-31)
			       (srfi srfi-60)
			       (srfi srfi-11)
			       (srfi srfi-2)
			       (srfi srfi-37)
			       (srfi srfi-39)
			       (srfi srfi-98)
			       (srfi srfi-9)
			       (srfi srfi-6)
			       (srfi srfi-67)
			       (srfi srfi-1)
			       (srfi srfi-14)
			       (srfi srfi-41)
			       (srfi srfi-17)
			       (srfi srfi-10)
			       (srfi srfi-35)
			       (srfi srfi-8)
			       (rnrs records syntactic)
			       (rnrs records inspection)
			       (rnrs records procedural)
			       (rnrs arithmetic flonums)
			       (rnrs arithmetic bitwise)
			       (rnrs arithmetic fixnums)
			       (rnrs io ports)
			       (rnrs io simple)
			       (rnrs syntax-case)
			       (rnrs bytevectors)
			       (rnrs r5rs)
			       (rnrs control)
			       (rnrs conditions)
			       (rnrs sorting)
			       (rnrs hashtables)
			       (rnrs mutable-pairs)
			       (rnrs exceptions)
			       (rnrs programs)
			       (rnrs files)
			       (rnrs unicode)
			       (rnrs lists)
			       (rnrs eval)
			       (rnrs base)
			       (rnrs mutable-strings)
			       (rnrs enums)
			       (statprof)
			       (texinfo)
			       (rnrs)))