#lang sicp


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
