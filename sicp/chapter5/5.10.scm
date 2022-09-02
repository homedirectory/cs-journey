
; Exercise 5.10: Design a new syntax for register-machine
; instructions and modify the simulator to use your new syn-
; tax. Can you implement your new syntax without changing
; any part of the simulator except the syntax procedures in
; this section?

; JZ - jump if zero
; (jz <reg_name> <label_name>)

((eq? (car inst) 'jz)
 (make-jz inst machine labels pc))

(define (make-jz inst machine labels pc)
  (let ((reg (jz-reg inst))
        (dest (jz-dest inst)))
    (if (and (symbol? reg) (symbol? dest))
      (let ((insts
              (lookup-label
                labels
                dest)))
        (lambda ()
          (if (= 0 (get-contents (get-register machine reg)))
            (set-contents! pc insts)
            (advance-pc pc))))
      (error "Bad JZ instruction: ASSEMBLE" inst))))

(define (jz-reg inst)
  (cadr inst))
(define (jz-dest inst)
  (caddr inst))
