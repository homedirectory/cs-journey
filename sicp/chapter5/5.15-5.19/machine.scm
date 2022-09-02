#lang sicp

(#%require "../../helpers.scm")
(#%provide (all-defined))

; ------------------------------------------------------------
; Registers
(define (do-make-register name trace?)
  ; a register contains 0 by default
  (let ((contents 0))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
               (when trace?
                 (print "REG: " name " NEW: " value " OLD: " contents))
               (set! contents value)))
            ((eq? message 'trace-on) (set! trace? #t))
            ((eq? message 'trace-off) (set! trace? #f))
            (else
              (error "Unknown request: REGISTER" message))))
    dispatch))

(define-syntax make-register
  (syntax-rules ()
    ((_ name trace?)
     (do-make-register name trace?))
    ((_ name)
     (do-make-register name #f))))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))
(define (trace-reg register) (register 'trace-on))
(define (untrace-reg register) (register 'trace-off))

; ------------------------------------------------------------
; The stack
(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
        (error "Empty stack: POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

; ------------------------------------------------------------
; The program counter
;(define (make-pc instructions)
;  (let ((counter 0)
;        (curr-insts instructions)) 
;    (define (reset)
;      (set! counter 0)
;      (set! curr-insts instructions))
;    (define (advance)
;      (if (null? curr-insts)
;        (error "No instructions: PC: advance" pc)
;        (begin
;          (set! counter (inc counter))
;          (set! curr-insts (cdr curr-insts)))))
;    (define (dispatch message)
;      (cond ((eq? message 'counter) counter)
;            ;((eq? message 'instructions) curr-insts)
;            ((eq? message 'reset) (reset))
;            ((eq? message 'advance) (advance))
;            ((eq? message 'fetch-instruction) (car curr-insts))
;            ((eq? message 'install-instructions)
;             (lambda (insts) (set! instructions insts) (reset)))
;            (else (error "Unknown request: PC" message))))
;    dispacth))

; ------------------------------------------------------------
; The machine
(define (do-make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define-syntax make-machine
  (syntax-rules ()
    ((make-machine reg-names ops text)
     (do-make-machine ops text))
    ((make-machine ops text)
     (do-make-machine ops text))
    ((make-machine text)
     (do-make-machine '() text))))

(define basic-ops 
  (list (list '> >) (list '= =) (list '< <)
        (list '>= >=) (list '<= <=)
        (list '* *) (list '+ +) (list '- -)
        (list '/ /)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ; breakpoints - list of instruction numbers
        ; none of that stuff with numbers relative to labels, 
        ; it doesn't make sense to me
        (breakpoints '())
        (paused? #f)
        (trace? #f))
    (let ((the-ops
            (append
              (list (list 'initialize-stack
                          (lambda () (stack 'initialize))))
              basic-ops))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
      (define (lookup-or-allocate-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (begin
              (allocate-register name)
              (lookup-register name)))))
      ; >>>>> debugging >>>>>
      (define (print-debug)
        (define (print-stack)
          (print "stack: " 
                 (map (lambda (x)
                        (if (label? x)
                          (list (instruction-text (car x)))
                          x)) 
                      (stack 'list))))
        (define (reg-entry-info reg-entry)
          (let ((reg-val 
                  (get-contents (cadr reg-entry))))
            (if (label? reg-val)
              (cons (car reg-entry) (list (instruction-text (car reg-val))))
              (cons (car reg-entry) reg-val))))
        (define (print-registers)
          (print "regs: " 
                 (map 
                   reg-entry-info
                   (filter (lambda (reg-entry)
                             (not (eq? 'pc (car reg-entry)))) 
                           register-table))))
        (print-trace) (print-stack) (print-registers) (newline))
      ; <<<<< debugging <<<<<
      (define (curr-inst)
        (let ((pc-contents (get-contents pc)))
          (if (null? pc-contents)
            #f
            (car pc-contents))))
      (define (print-trace)
        (let ((inst (curr-inst)))
          (print "PC: " (instruction-num inst) 
                 " " (instruction-text (curr-inst)))))
      (define (pause)
        (set! paused? #t)
        (when trace?
          (let ((inst (curr-inst)))
            (print "BREAKPOINT: " (instruction-num inst)
                   ": " (instruction-text inst))))
        'paused)
      (define (proceed)
        (when trace?
          (print "PROCEED"))
        (execute))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((not (instruction? (car insts))) ; debug
                 (error "Bad instruction: EXECUTE" (car insts)))
                (else
                  (when trace? (print-trace))
                  (if (and (not paused?) (list-find (instruction-num (car insts)) breakpoints))
                    (pause)
                    (begin (set! paused? #f)
                           ((instruction-execution-proc (car insts)))
                           (execute)))))))
      (define (advance-pc)
        (if (null? (get-contents pc))
          (error "No instructions: advance-pc" pc)
          (begin 
            (set-contents! pc (cdr (get-contents pc))))))
      (define (reset-pc)
        (set-contents! pc the-instruction-sequence))
      (define (trace-registers reg-names)
        (for-each
          (lambda (name)
            (trace-reg (lookup-register name)))
          reg-names))
      (define (untrace-registers reg-names)
        (for-each
          (lambda (name)
            (untrace-reg (lookup-register name)))
          reg-names))
      (define (set-breakpoint inst-num)
        (set! breakpoints (cons-if-absent inst-num breakpoints)))
      (define (rm-breakpoint inst-num)
        (set! breakpoints (list-remove inst-num breakpoints)))
      (define (rm-all-breakpoints)
        (set! breakpoints '()))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (reset-pc)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'try-get-register)
               lookup-or-allocate-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'advance-pc) (advance-pc))
              ((eq? message 'reset-pc) (reset-pc))
              ((eq? message 'trace-on) (set! trace? #t))
              ((eq? message 'trace-off) (set! trace? #f))
              ((eq? message 'trace-registers) trace-registers)
              ((eq? message 'untrace-registers) untrace-registers)
              ((eq? message 'trace-all-registers) 
               (trace-registers (map car register-table)))
              ((eq? message 'untrace-all-registers) 
               (untrace-registers (map car register-table)))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'rm-breakpoint) rm-breakpoint)
              ((eq? message 'rm-all-breakpoints) (rm-all-breakpoints))
              ((eq? message 'proceed) (proceed))
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))
(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (advance-pc machine)
  (machine 'advance-pc))
(define (trace-registers machine reg-names)
  ((machine 'trace-registers) reg-names))
(define (trace-register machine reg-name)
  (trace-registers machine (list reg-name)))
(define (trace-all-registers machine)
  (machine 'trace-all-registers))
(define (untrace-registers machine reg-names)
  ((machine 'untrace-registers) reg-names))
(define (untrace-register machine reg-name)
  (untrace-registers machine (list reg-name)))
(define (untrace-all-registers machine)
  (machine 'untrace-all-registers))
(define (set-breakpoint machine inst-num)
  ((machine 'set-breakpoint) inst-num))
(define (rm-breakpoint machine inst-num)
  ((machine 'rm-breakpoint) inst-num))
(define (rm-all-breakpoints machine)
  (machine 'rm-all-breakpoints))
(define (proceed machine) (machine 'proceed))

; for use by the assembler only
(define (try-get-register machine reg-name)
  ((machine 'try-get-register) reg-name))

; ------------------------------------------------------------
; Assembler
(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

; receive here is an example of using a continuation
; a cool trick to immitate the act of returning multiple values
(define (extract-labels text receive)
  (define (iter inst-num i-text i-receive)
    (if (null? i-text)
      (i-receive '() '())
      (let ((next-inst (car i-text)))
        (if (symbol? next-inst)
          (iter
            inst-num
            (cdr i-text)
            (lambda (insts labels)
              (i-receive 
                insts
                (if (assoc next-inst labels)
                  (error "Label defined more than once" next-inst)
                  (cons (make-label-entry next-inst insts)
                        labels)))))
          (iter
            (inc inst-num)
            (cdr i-text)
            (lambda (insts labels)
              (i-receive 
                (cons (make-instruction inst-num next-inst) insts)
                labels)))))))
  (iter 0 text receive))

(define (update-insts! insts labels machine)
  (let ((pc (try-get-register machine 'pc))
        (flag (try-get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine flag pc stack ops)))
      insts)))

; instruction data structure
; ('inst <instruction_number> <instruction_text> <execution_procedure>)
; the instruction text is not directly used by the machine simulator
; but is useful for debugging
(define (make-instruction n text) (list 'inst n text '()))
(define (instruction-num inst) (cadr inst))
(define (instruction-text inst) (caddr inst))
(define (instruction-execution-proc inst) (cadddr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdddr inst) proc))
(define (instruction? inst)
  (tagged-list? inst 'inst))

; label table entry
(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label: ASSEMBLE"
             label-name))))
(define (label? x)
  (and (list? x) 
       (not (null? x))
       (instruction? (car x))))

; ------------------------------------------------------------
; Instruction execution procedures
(define (make-execution-procedure
          inst labels machine flag pc stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'jz)
         (make-jz inst machine labels pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops))
        (else
          (error "Unknown instruction type: ASSEMBLE"
                 inst))))

; assign
; (assign <reg_name> <val_exp>)
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (make-assign inst machine labels operations)
  (let ((target
          (try-get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
              (make-operation-exp
                value-exp machine labels operations)
              (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc machine)))))

; test
; (test <cond>)
; <cond> : <op_exp>
; <op_exp> : <op> {<op_arg>}*
(define (make-test inst machine labels operations flag)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc
              (make-operation-exp
                condition machine labels operations)))
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc machine)))
      (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

; branch
; (branch <dest>)
; <dest> : <label_exp>
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts
              (lookup-label
                labels
                (label-exp-label dest))))
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc machine))))
      (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

; JZ - jump if zero
; (jz <reg_name> <label_name>)
(define (make-jz inst machine labels pc)
  (let ((reg (jz-reg inst))
        (dest (jz-dest inst)))
    (if (and (symbol? reg) (symbol? dest))
      (let ((insts
              (lookup-label
                labels
                dest)))
        (lambda ()
          (if (= 0 (get-contents (try-get-register machine reg)))
            (set-contents! pc insts)
            (advance-pc machine))))
      (error "Bad JZ instruction: ASSEMBLE" inst))))
(define (jz-reg inst)
  (cadr inst))
(define (jz-dest inst)
  (caddr inst))

; goto
; (goto <dest>)
; <dest> : <label_exp> | <reg_exp> 
(define (make-goto inst machine labels pc)
(let ((dest (goto-dest inst)))
  (cond ((label-exp? dest)
         (let ((insts (lookup-label
                        labels
                        (label-exp-label dest))))
           (lambda () (set-contents! pc insts))))
        ((register-exp? dest)
         (let ((reg (try-get-register
                      machine
                      (register-exp-reg dest))))
           (lambda ()
             (set-contents! pc (get-contents reg)))))
        (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

; save
; (save <reg_name>)
(define (make-save inst machine stack)
  (let ((reg (try-get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc machine))))

; restore
; (restore <reg_name>)
(define (make-restore inst machine stack)
  (let ((reg (try-get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc machine))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

; perform
; (perform <action>)
; <action> : <op_exp>
(define (make-perform inst machine labels operations)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc
              (make-operation-exp
                action machine labels operations)))
        (lambda () (action-proc) (advance-pc machine)))
      (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

; <prim_exp> : <reg_exp> | <label_exp> | <const_exp>
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                        labels
                        (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (try-get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

; <reg_exp> : (reg <reg_name>)
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))

; <const_exp> : (const <const_val>)
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

; <label_exp> : (label <label_name>)
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

; op
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
          (map (lambda (e)
                 (if (or (register-exp? e) (constant-exp? e))
                   (make-primitive-exp e machine labels)
                   (error "Unsupported operand" e)))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

; (<op> <args>)
; <op> : (op <op_name>)
; <args> : {<reg_exp> | <const_exp>}*
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation: ASSEMBLE"
             symbol))))
