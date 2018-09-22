#lang racket

(require rackunit)

(require "auxiliary_functions.rkt")
(provide int)

(define fill_in
  (lambda () '()))

#;(FlowChart is a simple imperative language
   <Program>    ::=   read <Var> ... <Var>: <BasicBlock>+
   <BasicBlock> ::=   <Label>: <Assignment>* <Jump>
   <Label>      ::=   any identifier or value
   <Assignment> ::=   := <Var> <Expression>
   <Jump>       ::=   goto <Label> | if <Expr> <Label> <Label> | return <Expression>
   <Expression> ::=   <Const> | <Var> | <Op> <Expr> ... <Expr>
   <Const>      ::=   any quoted data
   <Op>         ::=   any racket function
   Concrete syntax --- see example from examples.rkt

   int      --- FlowChart interpreter
   int-bb   --- basic block interpreter
   int-jump --- jump interpreter
   int-assn --- assignment interpreter
   int-TM   --- Turing Machine Interpreter on FlowChart
   int :: program -> data -> result
   (see example from examples.rkt)
   
   st is a program state (see dict from racket docs)
   all subsidiary functions are defined in auxiliary_functions.rkt

   function eval-exp (from auxiliary_functions) :: st -> expr -> result
     evaluates expression expr in environment st
   )

(define int
  (lambda (p d)
    (define st (int-read d (car p)))
    (define prog (initial-prog p))
    (define first-label (caadr p))
    (do-jump prog st first-label)))

(define int-read
  (lambda (data read)
    (define vars (cdr read))
    (check-equal? (car read) 'read "error: read expected.")
    (initial-st vars data)))

(define int-bb
  (lambda (prog st bb)
    (define jump (last bb))
    (define assignments (reverse (cdr (reverse bb))))
    (define changed-st (for/fold ([new-st st])
                                 ([assn assignments])
                         (check-equal? (car assn) ':= "error: assignment expected.")
                         (int-assn new-st assn)))
    (int-jump prog changed-st jump)))

(define int-jump
  (lambda (prog st jump)
    (define actual-int (match (car jump)
                         ['goto int-goto]
                         ['if int-if]
                         ['return int-return]
                         [_ (error "int-jump error: basic block is not ended with jump.")]))
    (actual-int prog st jump)))

(define int-goto
  (lambda (prog st goto)
    (do-jump prog st (cadr goto))))

(define int-if
  (lambda (prog st if-then-else)
    (define cond (eval-exp st (cadr if-then-else)))
    (define then-target (caddr if-then-else))
    (define else-target (cadddr if-then-else))
    (do-jump prog st (if cond then-target else-target))))

(define int-return
  (lambda (prog st return)
    (eval-exp st (cadr return))))

(define do-jump 
  (lambda (prog st target)
    (int-bb prog
            st
            (bb-lookup prog target))))

(define int-assn
  (lambda (st assn)
    (define var (cadr assn))
    (define val (eval-exp st (caddr assn)))
    (st-set st var val)))

(define int-TM
  '((read Q Right)
   (init (:= Qtail Q)
         (:= Left '())
         (goto loop))
   (loop (if (equal? Qtail '()) stop cont))
   (cont (:= Inst (cdar Qtail))
         (:= Ins (car Inst))
         (:= Qtail (cdr Qtail))
         (if (equal? Ins 'right) rght cnd1))
   (cnd1 (if (equal? Ins 'left) left cnd2))
   (cnd2 (if (equal? Ins 'write) wrgt cnd3))
   (cnd3 (if (equal? Ins 'goto) goto cnd4))
   (cnd4 (if (equal? Ins 'if) ifte errr))
   (rght (:= Left (cons (hd Right) Left))
         (:= Right (tl Right))
         (goto loop))
   (left (:= Right (cons (hd Left) Right))
         (:= Left (tl Left))
         (goto loop))
   (wrgt (:= Symbol (cadr Inst))
         (:= Right (cons Symbol (tl Right)))
         (goto loop))
   (goto (:= NextLabel (cadr Inst))
         (goto jump))
   (ifte (:= Symbol (cadr Inst))
         (:= NextLabel (cadddr Inst))
         (if (equal? Symbol (hd Right)) jump loop))
   (jump (:= Qtail (new-qtail Q NextLabel))
         (goto loop))
   (errr (return 'errorSyntax:Inst))
   (stop (return Right))))