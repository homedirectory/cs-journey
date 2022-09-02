#lang sicp

; the label that appears first in the text will overshadow the other ones

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (receive insts
                     (if (assoc next-inst labels)
                       (error "Label defined more than once" next-inst)
                       (cons (make-label-entry next-inst
                                               insts)
                             labels)))
            (receive (cons (make-instruction next-inst)
                           insts)
                     labels)))))))
