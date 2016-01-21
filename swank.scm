#| -*-Scheme-*-

Copyright (C) 2008 Helmut Eller

This file is licensed under the terms of the GNU General Public
License as distributed with Emacs (press C-h C-c for details).

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; SWANK module for MIT/GNU Scheme
;;; package: (runtime swank)

;;; Suggested for .emacs:
#|
(when (require 'slime nil t)

      (defun mit-scheme-start-swank (file encoding)
        (format "%S\n\n" `(start-swank ,file)))

      (defun mit-scheme-find-buffer-package ()
        (save-excursion
         (let ((case-fold-search t))
           (goto-char (point-min))
           (and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
                (match-string-no-properties 1)))))

      (defun mit-scheme-slime-mode-init ()
        (slime-mode t)
        (make-local-variable 'slime-find-buffer-package-function)
        (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

      (slime-setup)
      (if (not (memq 'mit-scheme slime-lisp-implementations))
          (setq slime-lisp-implementations
                (cons '(mit-scheme ("mit-scheme")
                                   :init mit-scheme-start-swank)
                      slime-lisp-implementations)))
      (setq slime-default-lisp 'mit-scheme)
      (add-hook 'scheme-mode-hook 'mit-scheme-slime-mode-init))
|#

(declare (usual-integrations))

(define (start-swank #!optional port-file)
  (let ((port-number 4005)
        (socket))
    (dynamic-wind
        (lambda ()
          (set! socket
                (open-tcp-server-socket port-number
                                        (host-address-loopback)))
          unspecific)
        (lambda ()
          (if (not (default-object? port-file))
              (call-with-output-file port-file
                (lambda (p)
                  (write port-number p))))
          (serve (tcp-server-connection-accept socket #t #f)))
        (lambda ()
          (close-tcp-server-socket socket)
          (set! socket)
          unspecific))))

(define (serve socket)
  (with-simple-restart 'DISCONNECT "Close connection."
                       (lambda ()
                         (with-keyboard-interrupt-handler
                          (lambda ()
                            (main-loop socket))))))

(define (with-keyboard-interrupt-handler thunk)
  (let ((index (char->integer #\G))
        (new-handler
         (lambda (char)
           char
           (with-simple-restart 'CONTINUE "Continue from interrupt."
                                (lambda ()
                                  (error "Keyboard Interrupt.")))))
        (old-handler))
    (dynamic-wind
        (lambda ()
          (let ((v keyboard-interrupt-vector))
            (set! old-handler (vector-ref v index))
            (vector-set! v index new-handler)))
        thunk
        (lambda ()
          (vector-set! keyboard-interrupt-vector index old-handler)
          (set! old-handler)
          unspecific))))

(define (disconnect)
  (invoke-restart (find-restart 'DISCONNECT)))

(define (main-loop socket)
  (do () (#f)
    (call-with-current-continuation
     (lambda (continuation)
       (with-restart 'ABORT "Return to SLIME top-level."
                     (lambda ()
                       (set! *aborted?* #t)
		       (write-message `(:return (:abort "NIL") ,*current-id*) socket)
                       (continuation unspecific))
                     values
                     (lambda ()
                       (let-fluid *top-level-restart* (find-restart 'ABORT)
                                  (lambda ()
                                    (call-with-current-continuation (lambda (k) (set! escape-to-loop (make-fluid k))))
                                    (process-one-message socket 0)))))))
    (if *aborted?*
        (begin
          (if (condition? *condition*)
              (write-message `(:return (:abort ,(condition/report-string *condition*)) ,*aborted-id*) socket)
              (write-message `(:return (:abort ,(format #f "unknown condition: ~s" *condition*)) ,(if *aborted-id* *aborted-id* 'nil)) socket))
          (set! *aborted-id* #f)
          (set! *aborted?* #f)
          (set! *condition* #f)))))

(define *aborted-id* #f)
(define *current-id* #f)
(define *aborted?* #f)
(define *condition* #f)
(define *top-level-restart* (make-fluid #f))

(define (get-current-environment)
  (nearest-repl/environment))

(define (set-current-environment! environment)
  (set-repl/environment! (nearest-repl) environment))

(define (top-level-abort)
  (invoke-restart (fluid *top-level-restart*)))

(define (bound-restarts-for-emacs)
  (let loop ((restarts (bound-restarts)))
    (if (pair? restarts)
        (cons (car restarts)
              (if (eq? (car restarts) (fluid *top-level-restart*))
                  '()
                  (loop (cdr restarts))))
        '())))

(define (process-one-message socket level)
  (dispatch (decode-message socket (read-packet socket)) socket level))

(define (read-packet in)
  (define (read-string! buffer in)
    (let loop ((i 0))
      (if (< i (string-length buffer))
	  (let ((n (read-substring! buffer i (string-length buffer) in)))
	    (if (not (exact-nonnegative-integer? n))
		(disconnect))
                                        ;(assert (<= n (- (string-length buffer) i)))
	    (loop (+ i n))))))
  (let ((buffer
	 (make-string
	  (let ((buffer (make-string 6)))
	    (read-string! buffer in)
	    (string->number buffer 16 #t)))))
    (read-string! buffer in)
    buffer))

(define (read-from-string str)
  (with-input-from-string str
    read))

(define (decode-message socket packet)
  (bind-condition-handler (list condition-type:serious-condition)
                          (lambda (condition)
                            (write-message `(:reader-error ,packet
                                                           ,(condition/report-string condition))
                                           socket)
                            (top-level-abort))
                          (lambda ()
                            (read-from-string packet))))

(define (write-message message out)
  ;; (with-output-to-file "/tmp/foo.log"
  ;;   (lambda ()
  ;;     (display message)
  ;;     (newline)
  ;;     (write message)
  ;;     (newline)))
  (write-packet (write-to-string message) out))

(define (write-packet packet out)
  (let ((s (number->string (string-length packet) 16)))
    (if (> (string-length s) 6)
        (begin
          (error "Expression length exceeds 24 bits:" s (substring packet 0 100))))
    (write-string (string-pad-left s 6 #\0) out))
  (write-string packet out)
  (flush-output out))

(define (dispatch message socket level)
  (let ((p
         (find (lambda (p)
                 (syntax-match? (car p) message))
               message-handlers)))
    (if (not p)
        (error "Unknown message:" message))
    (apply (cdr p) socket level (cdr message))))

(define (define-message-handler pattern handler)
  (set! message-handlers
        (cons (cons pattern handler)
              message-handlers))
  unspecific)

(define message-handlers '())

;;;; Message handlers

(define-message-handler '(':emacs-rex form datum datum index)
  (lambda (socket level sexp pstring thread id)
    thread
    (set! *current-id* id)
    (call-with-current-continuation
     (lambda (k)
       (bind-condition-handler (list condition-type:serious-condition)
                               (lambda (condition)
                                 (dynamic-wind
                                     (lambda ()
                                       #f)
                                     (lambda ()
                                       (invoke-sldb socket (+ level 1) condition))
                                     (lambda ()
                                       (set! *aborted-id* id)
                                       (set! *condition* condition)
                                       #f
                                       )))
                               (lambda ()
                                 (write-message `(:return (:ok ,(emacs-rex socket sexp pstring id))
                                                          ,id)
                                                socket)))))))

(define-message-handler '(':emacs-return-string form index string)
  (lambda (socket level unknown index string)
    (set! *input-buffer* (open-input-string string))
    (let ((cont (car *reading-continuations*)))
      (set! *reading-continuations* (cdr *reading-continuations*))
      (cont #f))))

(define *index* (make-fluid #f))

(define (emacs-rex socket sexp pstring id)
  (let-fluids *buffer-pstring* pstring
              *index* id
              (lambda ()
                (eval (cons* (car sexp) socket (map quote-special (cdr sexp)))
                      swank-env))))

(define *buffer-pstring* (make-fluid #f))

(define swank-env
  (the-environment))

(define (buffer-env)
  (pstring->env (fluid *buffer-pstring*)))

(define (pstring->env pstring)
  (cond ((or (not (string? pstring))
             (not (string? (fluid *buffer-pstring*)))
             (string-ci=? (fluid *buffer-pstring*) "COMMON-LISP-USER"))
         (get-current-environment))
        ((string-prefix? anonymous-package-prefix pstring)
         (let ((object
                (object-unhash
                 (string->number (string-tail pstring
                                              (string-length
                                               anonymous-package-prefix))
                                 10
                                 #t))))
           (if (not (environment? object))
               (error:wrong-type-datum object "environment"))
           object))
        (else
         (package/environment (find-package (read-from-string pstring) #t)))))

(define (env->pstring env)
  (if (eq? unknown-environment env)
      "unknown environment"
      (let ((package (environment->package env)))
	(if package
	    (write-to-string (package/name package))
	    (string anonymous-package-prefix (object-hash env))))))

(define anonymous-package-prefix
  "environment-")

;;;; Evaluation

(define (swank:interactive-eval socket string)
  (with-output-to-string
    (lambda ()
      (write (interactive-eval (read-from-string string) socket #f)))))

(define (swank:interactive-eval-region socket string)
  (for-each-sexp (lambda (sexp) (interactive-eval sexp socket #f))
                 string))

(define-structure swank-image type file data string)

(define (base64-encode string)
  (let ((out (open-output-string)))
    (run-shell-command "base64 -w 0"
		       'input (open-input-string string)
		       'output out)
    (string-trim (get-output-string out))))

(define (encode-swank-image-data str)
  (base64-encode str))

(define *image-converters* '())

(define (register-image-converter! predicate converter)
  (set! *image-converters* (cons (cons predicate converter) *image-converters*)))

(define (convert-to-swank-image thing)
  (if (swank-image? thing)
      thing
      (let loop ((conv *image-converters*))
	(if (null? conv)
	    #f
	    (if ((caar conv) thing)
		((cdar conv) thing)
		(loop (cdr conv)))))))

(define (present-thing socket result)
  (let ((hash-number (hash result)))
    (if (and *any-output?*
	     (not *last-character-newline?*))
	(write-message `(:write-string "\n" :repl-result) socket))
    (let ((swank-image (convert-to-swank-image result)))
      (if swank-image
	  (begin
	    (if (swank-image-file swank-image)
		(write-message `(:write-image ((:type ,(swank-image-type swank-image) ,@(list ':file (swank-image-file swank-image)))) ,(swank-image-string swank-image))
			       socket)
		(write-message `(:write-image ((:type ,(swank-image-type swank-image) ,@(list ':data (encode-swank-image-data (swank-image-data swank-image))))) ,(swank-image-string swank-image))
			       socket))
	    (write-message `(:write-string "\n" :repl-result) socket))))
    (write-message `(:presentation-start ,hash-number :repl-result)
		   socket)
    (write-message `(:write-string ,(format #f "~s" result)
				   :repl-result)
		   socket)
    (write-message `(:presentation-end ,hash-number :repl-result)
		   socket)
    (write-message `(:write-string "\n" :repl-result)
		   socket)
    (set! *last-character-newline?* #t)
    (set! *any-output?* #f)))

(define (swank-repl:listener-eval socket string)
  (let* ((result (interactive-eval (read-from-string string)
                                   socket
                                   #f)))
    (present-thing socket result))
  'NIL)

(define (interactive-eval sexp socket nl?)
  (let ((value (repl-eval sexp socket)))
    ;; (call-with-output-string
    ;;   (lambda (port)
    ;;  ;(port/write-result port sexp value (object-hash value) (buffer-env))
    ;;  (write value port)
    ;;  (if nl? (newline port))
    ;;  (hash value)))
    value
    ))

(define (for-each-sexp procedure string)
  (let ((input (open-input-string string)))
    (let loop ()
      (let ((sexp (read input)))
        (if (not (eof-object? sexp))
            (begin
              (procedure sexp)
              (loop)))))))

(define (repl-eval sexp socket)
  (with-i/o-to-repl socket
                    (lambda ()
                      (with-repl-eval-boundary 'SWANK
                                               (lambda ()
                                                 (eval sexp (buffer-env)))))))

(define (with-i/o-to-repl socket thunk)
  (let ((p (make-port repl-port-type socket)))
    (let ((trace-port (trace-output-port))
          (int-i/o-port (interaction-i/o-port)))
      (dynamic-wind
          (lambda () (set-trace-output-port! p) (set! *output-buffer* (open-output-string)) (set-interaction-i/o-port! p)
             )
          (lambda () (with-output-to-port p (lambda () (with-input-from-port p thunk))))
          (lambda () (set-trace-output-port! trace-port) (set-interaction-i/o-port! int-i/o-port) (flush-output p)
             )))))

(define repl-port-type)
(define *last-character-newline?* #t)
(define *any-output?* #f)
(define *output-buffer* (open-output-string))
(define *input-buffer* (open-input-string ""))
(define *reading-continuations* '())
(define escape-to-loop (make-fluid #f))
(define *read-index* 1)

(define (ensure-input port)
  (let ((char (peek-char *input-buffer*)))
    (if (not (eof-object? char))
        #f
        (call-with-current-continuation
         (lambda (k)
           (set! *reading-continuations* (cons k *reading-continuations*))
           (write-message `(:read-string 1 ,*read-index*)
                          (port/state port))
           (set! *read-index* (+ *read-index* 1))
           ((fluid escape-to-loop) #f))))))

(define (initialize-package!)
  (set! *top-level-restart* (make-fluid unspecific))
  (set! *sldb-state* (make-fluid #f))
  (set! *index* (make-fluid unspecific))
  (set! *buffer-pstring* (make-fluid unspecific))
  (set! repl-port-type
        (make-port-type
         `((WRITE-CHAR
            ,(lambda (port char)
               ;; (write-message `(:write-string ,(string char))
               ;;                     (port/state port))
               (display char *output-buffer*)
               (set! *any-output?* #t)
               (set! *last-character-newline?* (char=? char #\newline))
               1))
           (READ-CHAR
            ,(lambda (port)
               (ensure-input port)
               (read-char *input-buffer*)))
           (PEEK-CHAR
            ,(lambda (port)
               (ensure-input port)
               (peek-char *input-buffer*)))
           (UNREAD-CHAR
            ,(lambda (port c)
               (ensure-input port)
               (unread-char c *input-buffer*)))
           (WRITE-SUBSTRING
            ,(lambda (port string start end)
               (if (< start end)
                   (let ((str (substring string start end)))
                     ;; (write-message `(:write-string ,str)
                     ;;                     (port/state port))
                     (display str *output-buffer*)
                     (set! *any-output?* #t)
                     (set! *last-character-newline?* (char=? (string-ref str (- (string-length str) 1)) #\newline))))
               (- end start)))
           (FLUSH-OUTPUT
            ,(lambda (port)
               (let ((str (get-output-string *output-buffer*)))
		 (if (not (string-null? str))
		     (write-message `(:write-string ,str)
				    (port/state port))))
               (set! *output-buffer* (open-output-string)))))
         #f))
  unspecific)

(define (swank:pprint-eval socket string)
  socket
  (pprint-to-string (eval (read-from-string string)
                          (buffer-env))))

;;;; Compilation

(define (swank:compile-string-for-emacs socket string . x)
  socket x
  (let ((sexps (snarf-string string)))
    (call-compiler
     (lambda ()
       (let ((env (buffer-env)))
         (scode-eval ((environment-lookup #f 'compile-scode)
                      (syntax `(begin ,@sexps) env)
                      #t)
                     env)))
     #f)))

(define (snarf-string string)
  (let ((port (open-input-string string)))
    (let loop ()
      (let ((e (read port)))
        (if (eof-object? e)
            '()
            (cons e (loop)))))))

(define (call-compiler fun pathname)
  (let ((time #f))
    (with-timings fun
                  (lambda (run-time gc-time real-time)
                    run-time gc-time
                    (set! time real-time)
                    unspecific))
    `(:compilation-result NIL T ,(internal-time/ticks->seconds time) NIL ,(if pathname (->namestring (pathname-new-type pathname "com")) 'nil)))) ;; ec: not sure about these parameters :-/

(define (swank:compile-file-for-emacs socket file load?)
  (let ((result (call-compiler
		 (lambda ()
		   (with-i/o-to-repl socket
				     (lambda ()
				       ((environment-lookup #f 'compile-file) file))))
		 file)))
    (if (elisp-true? load?)
	(swank:load-file socket
			 (pathname-new-type file "com")))
    result))

(define (swank:load-file socket file)
  (with-i/o-to-repl socket
                    (lambda ()
                      (load file (buffer-env))
                      'loaded)))

(define (swank:disassemble-symbol socket string)
  socket
  (with-output-to-string
    (lambda ()
      ((environment-lookup #f 'compiler:disassemble)
       (eval (read-from-string string)
             (buffer-env))))))

;;;; Directory Functions
(define (swank:default-directory socket)
  (->namestring (working-directory-pathname)))

(define (swank:set-default-directory socket directory)
  (->namestring (set-working-directory-pathname! directory)))

;;;; Describe
(define (swank:describe-symbol socket symbol)
  (let* ((env (buffer-env))
         (package (env->pstring env))
         (symbol (string->symbol symbol))
         (type (environment-reference-type env symbol))
         (binding (if (eq? type 'normal) (environment-lookup env symbol) #f))
         (binding-type (if binding (get-object-type-name binding) #f))
         (doc (if (and (eq? type 'normal)
                       binding)
                  (and-let* ((doc (documentation-string binding))
			     ((not (eq? doc unspecific))))
			    doc)
                  #f))
         (params (if (and binding (procedure? binding)) (procedure-parameters symbol env) #f)))
    (string-append
     (format #f "~a in package ~a~a of type ~a.~%~%" (string-upcase (symbol->string symbol))
             package
             (if (and binding
                      (procedure? binding))
		 (format #f " [originally defined in package ~a]" (env->pstring (procedure-environment binding)))
                 "")
             (if binding-type binding-type type))
     (if doc
         (format #f "~a~%~%" doc)
         "")
     (if binding
         (format #f "Bound to ~a.~%" binding)
         "")
     (if params
         (format #f "~%Signature: ~a.~%~%" params)
         "")
     (if binding
         (format #f "It is:~%~%~a~%" (with-output-to-string (lambda () (pp binding))))
         ""))))

(define (swank:describe-function socket function)
  (swank:describe-symbol socket function))

(define (swank:describe-definition-for-emacs socket name type)
  type
  (swank:describe-symbol socket name))

(define (get-object-type-name obj)
  (cond ((boolean? obj) "boolean")
        ((string? obj) "string")
        ((char? obj) "char")
        ((fixnum? obj) "fixnum")
        ((integer? obj) "integer")
        ((rational? obj) "rational")
        ((real? obj) "real")
        ((complex? obj) "complex")
        ((vector? obj) "vector")
        ((pair? obj) "pair")
        ((null? obj) "empty list")
        ((bit-string? obj) "bit-string")
        ((cell? obj) "cell")
        ((condition? obj) "condition")
        ((environment? obj) "environment")
        ((port? obj) "port")
        ((procedure? obj) "procedure")
        ((promise? obj) "promise")
        ((symbol? obj) "symbol")
        ((weak-pair? obj) "weak-pair")
        ((record-type? obj) "record-type")
	((dispatch-tag? obj) "dispatch-tag")
        ((wide-string? obj) "wide-string")
	((record? obj) "record")
	((eq? unspecific obj) "unspecific")
	((eq? #!aux obj) "#!aux")
	((eq? #!rest obj) "#!rest")
	((eq? #!optional obj) "#!optional")
	((eq? #!key obj) "#!key")
	((eq? (user-object-type obj) 'false) "false")
        (else (error "get-object-type-name: unknown type" obj (user-object-type obj)))))

;;;; Miscellaneous

(define (swank:set-package socket pstring)
  socket
  (let ((env (pstring->env pstring)))
    (set-current-environment! env)
    (let ((pstring (env->pstring env)))
      (list pstring pstring))))

(define (swank-repl:create-repl socket . args)
  socket args
  (let ((pstring (env->pstring (->environment '(user)))))
    (list pstring pstring)))

(define (swank:swank-macroexpand-all socket string)
  socket
  (with-output-to-string
    (lambda ()
      (pp (syntax (read-from-string string)
                  (buffer-env))))))

(define swank:swank-macroexpand-1 swank:swank-macroexpand-all)
(define swank:swank-macroexpand swank:swank-macroexpand-all)

(define (swank:operator-arglist socket name pstring)
  socket
  (let ((v (ignore-errors
            (lambda ()
              (with-output-to-string
                (lambda ()
                  (carefully-pa
                   (eval (read-from-string name) (pstring->env pstring)))))))))
    (if (condition? v) 'NIL v)))

(define (carefully-pa o)
  (cond ((arity-dispatched-procedure? o)
         ;; MIT Scheme crashes for (pa /)
         (display "arity-dispatched-procedure"))
        ((procedure? o) (pa o))
        (else (error "Not a procedure"))))

(define (swank:connection-info socket)
  socket
  (let ((pstring (env->pstring (buffer-env))))
    `(:pid ,(unix/current-pid)
           :style :spawn
           :encoding (:coding-systems ("utf-8-unix"))
           :lisp-implementation (:type "MIT/GNU Scheme" :name "mit-scheme" :version ,(get-subsystem-version-string "release") :program "/usr/bin/mit-scheme")
           :machine (:instance ,(get-host-name) :type "X86-64")
           :features (:swank)
           :modules ("SWANK-ARGLISTS" "SWANK-REPL" "SWANK-PRESENTATIONS")
           :package (:name ,pstring :prompt ,pstring)
           :version "2015-12-13"
           )))

(define (swank:swank-require socket packages)
  socket
  packages
  '())

(define swank-extra-documentation
  '((let bindings . body)
    (let* bindings . body)
    (letrec bindings . body)
    (receive bindings expression . body)
    (define name . body)
    (quote expression)
    (quasiquote expression)
    (unquote expression)
    (unquote-splicing expression)
    (if test then else)
    (set! name value)))

(define (procedure-parameters symbol env)
  (let ((type (environment-reference-type env symbol)))
    (let ((ans (if (eq? type 'normal)
                   (let ((binding (environment-lookup env symbol)))
                     (if (and binding
                              (procedure? binding))
                         (cons symbol (read-from-string (string-trim (with-output-to-string
                                                                       (lambda () (pa binding))))))
                         #f))
                   (let ((extra (assq symbol swank-extra-documentation)))
                     (if extra
                         extra
                         #f)))))
      ans)))

(define (find-expression-containing-swank-cursor-marker expr)
  (if (list? expr)
      (if (member 'swank::%cursor-marker% expr)
          expr
          (any (lambda (ex)
                 (find-expression-containing-swank-cursor-marker ex))
               expr))
      #f))

(define (find-string-before-swank-cursor-marker expr)
  (let ((ex (find-expression-containing-swank-cursor-marker expr)))
    (if ex
        (if (string? (car ex))
            (car ex)
            #f))))

(define (improper-length lst)
  (if (pair? lst)
      (+ 1 (improper-length (cdr lst)))
      (if (null? lst)
          0
          1)))

(define (improper->proper-list lst)
  (if (pair? lst)
      (cons (car lst)
            (improper->proper-list (cdr lst)))
      (if (null? lst)
          '()
          (list '#!rest lst))))

(define *stderr* (current-output-port))

(define (wrap-item/index lst index before-marker after-marker)
  (let loop ((i 0)
             (lst lst))
    (cond ((null? (cdr lst))
           (list before-marker (car lst) after-marker))
          ((member (car lst) '(#!rest #!optional))
           (cons (car lst) (loop i (cdr lst))))
          ((= i index)
           (append (list before-marker (car lst) after-marker) (cdr lst)))
          (else
           (cons (car lst)
                 (loop (+ i 1) (cdr lst))))))
  )

(define (highlight-at-cursor signature expr)
  (let* ((form (find-expression-containing-swank-cursor-marker expr))
         (index (list-index (lambda (el) (eq? el '|swank::%cursor-marker%|)) form)))
    (wrap-item/index (improper->proper-list signature) (- index 1) '===> '<===)))

(define (swank:autodoc socket expr . params)
  socket params
  (let* ((op-string (find-string-before-swank-cursor-marker expr)))
    (if op-string
        (let* ((op (string->symbol op-string))
               (type (environment-reference-type (buffer-env) op)))
          (let ((ans (procedure-parameters op (buffer-env)))
                (doc (if (and (eq? type 'normal)
                              (environment-bound? (buffer-env) op))
                         (documentation-string (environment-lookup (buffer-env) op))
                         #f)))
            (let ((answer (if ans (with-output-to-string (lambda ()
                                                           (let* ((ans (highlight-at-cursor ans expr)))
                                                             (write ans))
                                                           (if doc
                                                               (begin
                                                                 (display "  - ")
                                                                 (display doc))))) ':not-available)))
              (list answer 't))))
        (list ':not-available 't))))

(define (swank:quit-lisp socket)
  socket
  (%exit))

;;;; Some unimplemented stuff.

(define (swank:buffer-first-change socket filename)
  socket filename
  'NIL)

(define *mit-scheme-sources-location* #f)
;; (->pathname "/home/nex/mit-scheme/src/")

(define (find-file+position-of-definition object name)
  (let ((file
         (if (and (entity? object)
                  (procedure? object))
             (receive (a b) (compiled-entry/filename-and-index (entity-procedure object)) a)
             (if (compiled-procedure? object)
                 (receive (a b) (compiled-entry/filename-and-index object) a)
                 #f))))
    (let ((scm-file (if (and file
                             (string-suffix? ".inf" file))
                        (string-append (substring file 0 (- (string-length file) 3)) "scm")
                        file)))
      (if (and scm-file
               (file-exists? scm-file))
          (find-definition-position-in-file scm-file object name)
          (let ((scm-file (if scm-file (find-mit-scheme-sources-file scm-file) #f)))
            (if (and scm-file
                     (file-exists? scm-file))
                (find-definition-position-in-file scm-file object name)
                'nil))))))

(define (read-file-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line))
                 (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (loop (read-line)
                  (cons line lines)))))))

(define (compiled-entity-name object)
  (if (and (entity? object)
           (procedure? object))
      (compiled-procedure/name (entity-procedure object))
      (compiled-procedure/name object)))

;; try to find the definition of object in file..
;; for now, try to find a line that has ".*(define.* (?<name>"
(define (find-definition-position-in-file file object name)
  (define (find-pos file object)
    (let ((file-lines (read-file-lines file)))
      (let loop ((lines file-lines)
                 (count 0))
        (if (null? lines)
            (values 0 "")
            (let ((match (re-string-search-forward (string-append ".*(define.* (?" name) (car lines))))
              (if match
                  (values count (car lines))
                  (loop (cdr lines)
                        (+ count (string-length (car lines)) 1)))))))) ;; 1 for new-line
  (receive (pos snippet)
      (find-pos file object)
    `((,(string-append "(define " name ")")
       (:location
        (:file ,file)
        (:position ,(+ 1 pos)) ;; extra new-line
        (:snippet ,snippet)
        )))))

(define (find-mit-scheme-sources-file filename)
  (if *mit-scheme-sources-location*
      (let* ((pathname (->pathname filename))
             (directory (pathname-directory pathname)))
        (let loop ((pathname (pathname-new-type (pathname-new-name *mit-scheme-sources-location* (pathname-name filename)) (pathname-type filename)))
                   (directories (cdr directory)))
	  (format #t "pathname: ~a~%directories: ~a~%" pathname directories)
          (if (null? directories)
              #f
              (let ((new-pathname (pathname-new-directory pathname (append (pathname-directory pathname)
                                                                           directories))))
		(format #t "new-pathname: ~a~%" new-pathname)
                (if (file-exists? new-pathname)
                    (->namestring new-pathname)
                    (loop pathname
                          (cdr directories)))))))
      #f))



;; M-. is implemented heuristically. For compiled entries, the location of the debugging info file is taken and a similarly-named file is sought.
(define (swank:find-definitions-for-emacs socket name)
  socket name
  (find-file+position-of-definition (environment-lookup (buffer-env) (string->symbol name)) name))

#|
;;; List of names obtained by grepping through "slime.el" and
;;; "slime-repl.el".

swank:commit-edited-value
swank:compile-file-if-needed
swank:compile-multiple-strings-for-emacs
swank:create-server
swank:debug-nth-thread
swank:default-directory
swank:describe-definition-for-emacs
swank:describe-function
swank:describe-inspectee
swank:describe-symbol
swank:documentation-symbol
swank:eval-and-grab-output
swank:eval-string-in-frame
swank:find-source-location-for-emacs
swank:frame-source-location
swank:inspect-current-condition
swank:inspect-in-frame
swank:inspector-reinspect
swank:inspector-toggle-verbose
swank:io-speed-test
swank:kill-nth-thread
swank:list-threads
swank:pprint-eval-string-in-frame
swank:profile-package
swank:profile-report
swank:profile-reset
swank:profiled-functions
swank:quit-thread-browser
swank:re-evaluate-defvar
swank:redirect-trace-output
swank:restart-frame
swank:set-default-directory
swank:sldb-break
swank:sldb-break-on-return
swank:sldb-break-with-default-debugger
swank:sldb-disassemble
swank:sldb-next
swank:sldb-out
swank:sldb-return-from-frame
swank:sldb-step
swank:start-server
swank:start-swank-server-in-thread
swank:swank-compiler-macroexpand
swank:swank-compiler-macroexpand-1
swank:swank-format-string-expand
swank:swank-require
swank:swank-toggle-trace
swank:toggle-profile-fdefinition
swank:undefine-function
swank:unprofile-all
swank:untrace-all
swank:update-indentation-information
swank:value-for-editing
swank:xref
|#

;;;; Debugger

(define-structure (sldb-state (conc-name sldb-state.))
  condition
  restarts)

(define *sldb-state* (make-fluid #f))


(define (invoke-sldb socket level condition)
  (let-fluids *sldb-state* (make-sldb-state condition (bound-restarts-for-emacs))
              *open-restart-abort-indexes* '()
              (lambda ()
                (dynamic-wind
                    (lambda () #f)
                    (lambda ()
                      (write-message `(:debug 0 ,level ,@(sldb-info (fluid *sldb-state*) 0 20))
                                     socket)
                      (write-message `(:debug-activate 0 ,level) socket)
                      (let-fluid escape-to-loop (call-with-current-continuation (lambda (k) k))
                                 (lambda ()
                                   (sldb-loop level socket))))
                    (lambda ()
                      (for-each (lambda (index)
                                  (write-message `(:return (:abort "NIL") ,index) socket))
                                (fluid *open-restart-abort-indexes*))
                      (write-message `(:debug-return 0 ,(- level 1) 'NIL) socket))))))

(define (sldb-loop level socket)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart 'ABORT (string "Return to SLDB level " level ".")
		   (lambda ()
		     (set! *aborted?* #t)
		     (continuation unspecific))
		   values
		   (lambda ()
		     (process-one-message socket level)))))
  (if *aborted?*
      (begin
	(write-message `(:return (:abort ,(condition/report-string *condition*)) ,*aborted-id*) socket)
	(set! *aborted-id* #f)
	(set! *aborted?* #f)
	(set! *condition* #f)))
  (sldb-loop level socket))

(define (sldb-info state start end)
  (let ((c (sldb-state.condition state))
        (rs (sldb-state.restarts state)))
    (list (list (condition/report-string c)
                (string "  [" (condition-type/name (condition/type c)) "]")
                'NIL)
          (sldb-restarts rs)
          (sldb-backtrace c start end)
          (list (fluid *index*)))))

(define (sldb-restarts restarts)
  (map (lambda (r)
         (list (symbol->string (restart/name r))
               (with-string-output-port
                (lambda (p) (write-restart-report r p)))))
       restarts))

(define (swank:throw-to-toplevel socket . args)
  socket args
  (top-level-abort))

(define (swank:sldb-abort socket . args)
  socket args
  (abort (sldb-state.restarts (fluid *sldb-state*))))

(define (swank:sldb-continue socket . args)
  socket args
  (continue (sldb-state.restarts (fluid *sldb-state*))))

(define (restart-has-interactor? restart)
  (restart/interactor restart))

(define *open-restart-abort-indexes* (make-fluid '()))
(define (swank:invoke-nth-restart-for-emacs socket sldb-level n)
  socket sldb-level
  (let ((restart (list-ref (sldb-state.restarts (fluid *sldb-state*)) n)))
    (set! *open-restart-abort-indexes* (make-fluid (cons (fluid *index*) (fluid *open-restart-abort-indexes*))))
    (invoke-restart-interactively restart)))

(define (swank:debugger-info-for-emacs socket from to)
  socket
  (sldb-info (fluid *sldb-state*) from to))

(define (swank:backtrace socket from to)
  socket
  (sldb-backtrace (sldb-state.condition (fluid *sldb-state*)) from to))

(define (sldb-backtrace condition from to)
  (sldb-backtrace-aux (condition/continuation condition) from to))

(define (sldb-backtrace-aux k from to)
  (let ((l (map frame->string (substream (continuation->frames k) from to))))
    (let loop ((i from) (l l))
      (if (null? l)
          '()
          (cons (list i (car l)) (loop (+ i 1) (cdr l)))))))

;; Stack parser fails for this:
;; (map (lambda (x) x) "/tmp/x.x")

(define (continuation->frames k)
  (let loop ((frame (continuation->stack-frame k)))
    (if (or (not frame)
            (stack-frame/repl-eval-boundary? frame))
        (stream)
        (cons-stream frame
                     (let ((next
                            (ignore-errors
                             (lambda ()
                               (stack-frame/next-subproblem frame)))))
                       (if (condition? next)
                           (stream next)
                           (loop next)))))))

(define (frame->string frame)
  (if (condition? frame)
      (string "Bogus frame: " frame
              " " (condition/report-string frame))
      (call-with-output-string (lambda (p) (print-frame frame p)))))

(define (print-frame frame port)
  (receive (expression environment subexpression)
      (stack-frame/debugging-info frame)
    environment
    (cond ((debugging-info/compiled-code? expression)
           (write-string ";unknown compiled code" port))
          ((not (debugging-info/undefined-expression? expression))
           (let-fluid *unparse-primitives-by-name?* #t
                      (lambda ()
                        (write
                         (unsyntax
                          (if (or (debugging-info/undefined-expression? subexpression)
                                  (debugging-info/unknown-expression? subexpression))
                              expression
                              subexpression))
                         port))))
          ((debugging-info/noise? expression)
           (write-string ";" port)
           (write-string ((debugging-info/noise expression) #f)
                         port))
          (else
           (write-string ";undefined expression" port)))))

(define (substream s from to)
  (let loop ((i 0) (l '()) (s s))
    (cond ((or (if (null? to) #f (= i to)) (stream-null? s)) (reverse l))
          ((< i from) (loop (+ i 1) l (stream-cdr s)))
          (else (loop (+ i 1) (cons (stream-car s) l) (stream-cdr s))))))

(define (swank:sdlb-print-condition socket)
  socket
  (condition/report-string (sldb-state.condition (fluid *sldb-state*)))
  )

(define (swank:inspect-current-condition socket)
  socket
  (inspect-object (sldb-state.condition (fluid *sldb-state*))))

(define (swank:frame-locals-and-catch-tags socket frame)
  socket
  (list (map frame-var>elisp (frame-vars (sldb-get-frame frame)))
        '()))

(define (frame-vars frame)
  (receive (expression environment subexpression)
      (stack-frame/debugging-info frame)
    expression subexpression
    (if (environment? environment)
        (environment>frame-vars environment)
        '())))

(define (environment>frame-vars environment)
  (let loop ((e environment))
    (if (top-level-environment? e)
        '()
        (append (environment-bindings e)
                (if (environment-has-parent? e)
                    (loop (environment-parent e))
                    '())))))

(define (frame-var>elisp b)
  (list ':name (write-to-string (car b))
        ':value (cond ((null? (cdr b)) "{unavailable}")
                      (else (->line (cadr b))))
        ':id 0))

(define (sldb-get-frame index)
  (stream-ref (continuation->frames
               (condition/continuation
                (sldb-state.condition (fluid *sldb-state*))))
              index))

(define (frame-var-value frame var)
  (let ((binding (list-ref (frame-vars frame) var)))
    (cond ((cdr binding) (cadr binding))
          (else unspecific))))

(define (swank:inspect-frame-var socket frame var)
  socket
  (reset-inspector)
  (inspect-object (frame-var-value (sldb-get-frame frame) var)))

;;;; Completion

(define (swank:simple-completions socket string pstring)
  socket
  (let ((strings (all-completions string (pstring->env pstring))))
    (list (sort strings string<?)
          (longest-common-prefix strings))))

(define (all-completions prefix environment)
  (let ((prefix
         (if (fluid (environment-lookup environment '*PARSER-CANONICALIZE-SYMBOLS?*))
             (string-downcase prefix)
             prefix))
        (completions '()))
    (for-each-interned-symbol
     (lambda (symbol)
       (if (and (string-prefix? prefix (symbol-name symbol))
                (environment-bound? environment symbol))
           (set! completions (cons (symbol-name symbol) completions)))
       unspecific))
    completions))

(define (longest-common-prefix strings)
  (reduce (lambda (s1 s2)
            (substring s1 0 (string-match-forward s1 s2)))
          ""
          strings))

;;;; Fuzzy Completions
(define (swank:fuzzy-completions socket string pstring . parameters)
  socket
  (let ((completions (all-fuzzy-completions string (pstring->env pstring))))
    (list completions 'nil)))

;; bfgctmsp
;; b=global
;; f=function
;; g=generic
;; c=class
;; t=type
;; m=macro
;; s=special
;; p=package
(define (binding-type-string type binding-type)
  (string-pad-right (case type
		      ((normal)
		       binding-type)
		      ((macro)
		       "macro")
		      ((unassigned)
		       "unassigned")
		      (else
		       "unknown"))
		    15
		    #\space))

(define (match-score prefix name)
  (let* ((name-len (string-length name))
	 (prefix-len (string-length prefix))
	 (difference (- name-len prefix-len)))
    (- 100 (* (/ difference 50) 100))))
(define (all-fuzzy-completions prefix environment)
  (let ((prefix
         (if (environment-lookup environment '*PARSER-CANONICALIZE-SYMBOLS?*)
             (string-downcase prefix)
             prefix))
        (completions '()))
    (for-each-interned-symbol
     (lambda (symbol)
       (if (and (substring? prefix (symbol-name symbol))
                (environment-bound? environment symbol))
	   (let* ((type (environment-reference-type environment symbol))
		  (unbound (list 'unbound))
		  (binding (if (eq? type 'normal) (environment-lookup environment symbol) unbound))
		  (binding-type (if (not (eq? binding unbound)) (get-object-type-name binding) #f))
		  (match-score (match-score prefix (symbol-name symbol))))
	     (set! completions (cons (list (symbol-name symbol) (number->string match-score) '() (binding-type-string type binding-type)) completions))))
       unspecific))
    (sort completions (lambda (a b) (> (string->number (second a)) (string->number (second b)))))))

(define (swank:fuzzy-completion-selected socket string form)
  'nil)

;;;; Apropos

(define unknown-environment (cons 'unknown 'environment))
(define (safe-procedure-environment p)
  (let ((p (ignore-errors (lambda () (procedure-environment p)))))
    (if (condition? p)
	unknown-environment
	p)))
(define (swank:apropos-list-for-emacs socket text external-only? case-sensitive?
                                      pstring)
  socket case-sensitive?
  (if (elisp-true? external-only?)
      (let ((env
	     (if (elisp-true? external-only?)
		 system-global-environment
		 (pstring->env pstring))))
	(map (lambda (symbol)
	       `(:designator ,(string symbol) ;;  " " pstring
			     ,@(case (environment-reference-type env symbol)
				 ((UNBOUND) '())
				 ((UNASSIGNED) `(:variable :not-documented))
				 ((MACRO) `(:macro :not-documented))
				 (else
				  (let ((v (environment-lookup env symbol)))
				    `(,(cond ((generic-procedure? v) ':generic-function)
					     ((procedure? v) ':function)
					     (else ':variable))
				      ,(string-append (write-to-string v) (if (procedure? v) (format #f " in package ~a" (env->pstring (safe-procedure-environment v))) ""))))))))
	     (reverse (apropos-list text env #t))))
      (let ((seen (make-hash-table)))
	(sort (append-map (lambda (p)
			    (let ((env (package/environment p)))
			      (filter identity-procedure (map (lambda (symbol)
								(let* ((type (environment-reference-type env symbol))
								       (v (if (memq type '(unbound unassigned macro)) #f (environment-lookup env symbol))))
								  (if (or (not v)
									  (and v
									       (not (hash-table/get seen v #f))))
								      (begin
									(hash-table/put! seen v #t)
									`(:designator ,(string-append (string symbol) " in " (if (and (procedure? v) (not (primitive-procedure? v))) (env->pstring (safe-procedure-environment v)) (env->pstring env)))
										      ,@(case (environment-reference-type env symbol)
											  ((UNBOUND) '())
											  ((UNASSIGNED) `(:variable :not-documented))
											  ((MACRO) `(:macro :not-documented))
											  (else
											   (let ((v (environment-lookup env symbol)))
											     `(,(cond ((generic-procedure? v) ':generic-function)
												      ((procedure? v) ':function)
												      (else ':variable))
											       ,(string-append (write-to-string v))))))))
								      #f)))
							      (filter (lambda (name)
									(substring? text (symbol->string name)))
								      (environment-bound-names env))))))
			  (all-packages))
	      (lambda (a b)
		(or (< (string-length (cadr a))
		       (string-length (cadr b)))
		    (and (= (string-length (cadr a))
			    (string-length (cadr b)))
			 (string<? (cadr a) (cadr b)))))))))

(define (swank:list-all-package-names socket . args)
  socket args
  (map (lambda (package) (env->pstring (package/environment package)))
       (all-packages)))

(define (all-packages)
  (let loop ((package (name->package '()))) ;;  system-global-package
    (cons package
          (append-map loop (package/children package)))))

;;;; Inspector

(define-record-type <istate>
  (make-istate object parts next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define istate #f)

(define (reset-inspector)
  (set! istate #f)
  unspecific)

(define (swank:init-presentations socket)
  ;; TODO
  '())

(define (swank:init-inspector socket string)
  socket
  (reset-inspector)
  (inspect-object (eval (read-from-string string)
                        (buffer-env))))

(define (inspect-object o)
  (let ((previous istate)
        (content (inspect o))
        (parts (make-strong-eqv-hash-table)))
    (set! istate (make-istate o parts #f previous content))
    (if previous (set-istate-next! previous istate))
    (istate->elisp istate)))

(define (istate->elisp istate)
  `(:title ,(->line (istate-object istate))
           :id ,(assign-index (istate-object istate) (istate-parts istate))
           :content ,(prepare-range (istate-parts istate)
                                    (istate-content istate)
                                    0 500)))

(define (assign-index o parts)
  (let ((i (hash-table/count parts)))
    (hash-table/put! parts i o)
    i))

(define (prepare-range parts content from to)
  (let* ((cs (substream content from to))
         (ps (prepare-parts cs parts)))
    (list ps
          (if (< (length cs) (- to from))
              (+ from (length cs))
              (+ to 1000))
          from to)))

(define (prepare-parts ps parts)
  (define (line label value)
    `(,(string label ": ")
      (:value ,(->line value) ,(assign-index value parts))
      "\n"))
  (append-map (lambda (p)
                (cond ((string? p) (list p))
                      ((symbol? p) (list (symbol->string p)))
                      (else
                       (case (car p)
                         ((line) (apply line (cdr p)))
                         (else (error "Invalid part:" p))))))
              ps))

(define (swank:inspect-nth-part socket index)
  socket
  (inspect-object (hash-table/get (istate-parts istate) index 'no-such-part)))

(define listener-saved-value)
(define (swank-repl:listener-save-value socket function . params)
  (let ((function (eval function (->environment '(runtime swank)))))
    (set! listener-saved-value (apply function socket params))
    't))
(define (swank-repl:listener-get-value socket)
  socket
  (present-thing socket listener-saved-value)
  'nil)

(define (swank:pprint-inspector-part socket index)
  socket
  (with-output-to-string
    (lambda ()
      (pp (hash-table/get (istate-parts istate) index 'no-such-part)))))

(define (swank:inspector-nth-part socket index)
  socket
  (hash-table/get (istate-parts istate) index 'no-such-part))

(define (swank:quit-inspector socket)
  socket
  (reset-inspector))

(define (swank:inspector-pop socket)
  socket
  (cond ((istate-previous istate)
         (set! istate (istate-previous istate))
         (istate->elisp istate))
        (else 'NIL)))

(define (swank:inspector-next socket)
  socket
  (cond ((istate-next istate)
         (set! istate (istate-next istate))
         (istate->elisp istate))
        (else 'NIL)))

(define (swank:inspector-range socket from to)
  socket
  (prepare-range (istate-parts istate)
                 (istate-content istate)
                 from to))

(define (iline label value)
  `(LINE ,label ,value))

(define (inspect o)
  (cond ((environment? o) (inspect-environment o))
        ((vector? o) (inspect-vector o))
        ((procedure? o) (inspect-procedure o))
        ((compiled-code-block? o) (inspect-code-block o))
        ((pair? o) (inspect-pair o))
        ;;((system-pair? o) (inspect-system-pair o))
        ((probably-scode? o) (inspect-scode o))
	((record? o) (inspect-record o))
	((integer? o) (inspect-integer o))
	((char? o) (inspect-char o))
	((symbol? o) (inspect-symbol o))
        (else (inspect-fallback o))))

(define (inspect-symbol s)
  (stream (iline "Symbol" s)))
(define (inspect-char c)
  (stream (iline "Char code" (char->integer c))
	  (iline "Lower cased" (char-downcase c))
	  (iline "Upper cased" (char-upcase c))))
(define (inspect-integer n)
  (stream
   (format #f "Value: ~a = #x~a = #o~a = #b~a~%" n (number->string n 16) (number->string n 8) (number->string n 2)) ;; = 4.e+0
   (iline "Code-char" (integer->char n))
   ;; (:value "#\\Eot" 1)
   ;; "\n" "Integer-length" ": "
   ;; (:value "3" 2)
   ;; (if (< epoch n)
   ;;     (iline "Decoded-time" (universal-time->global-decoded-time n))
   ;;     (format #f "Too small for universal time (min. ~a)" epoch))
   ;;    (:value "\"1900-01-01T01:00:04+01:00\"" 3)
   ))

(define (inspect-record o)
  (let ((type (record-type-descriptor o)))
    (cons-stream (iline "Record of type" type)
		 (stream-map (lambda (name)
			       (iline name ((record-accessor type name) o)))
			     (apply stream (record-type-field-names type) )))))
(define (inspect-fallback o)
  (cons-stream (iline "Object" o)
               (stream)))

(define (inspect-pair pair)
  (if (or (pair? (cdr pair))
          (null? (cdr pair)))
      (let loop ((l1 pair) (l2 pair) (i 0))
        (cond ((pair? l1)
               (cons-stream (iline i (car l1))
                            (let ((l1 (cdr l1)))
                              (if (eq? l1 l2)
                                  (stream "{circular list detected}")
                                  (loop l1
                                        (if (odd? i) (cdr l2) l2)
                                        (+ i 1))))))
              ((null? l1) (stream))
              (else (stream (iline "tail" (cdr l1))))))
      (stream (iline "car" (car pair))
              (iline "cdr" (cdr pair)))))

(define (inspect-environment env)
  (let ((package (environment->package env))
        (tail
         (let loop ((bindings (environment-bindings env)))
           (if (pair? bindings)
               (cons-stream (let ((binding (car bindings)))
                              (iline (car binding)
                                     (if (pair? (cdr binding))
                                         (cadr binding)
                                         (string "{"
                                                 (environment-reference-type
                                                  env
                                                  (car binding))
                                                 "}"))))
                            (loop (cdr bindings)))
               (if (environment-has-parent? env)
                   (stream (iline "(<parent>)" (environment-parent env)))
                   (stream))))))
    (if package
        (cons-stream (iline "(package)" package) tail)
        tail)))

(define (inspect-vector o)
  (let ((len (vector-length o)))
    (let loop ((i 0))
      (if (< i len)
          (cons-stream (iline i (vector-ref o i))
                       (loop (+ i 1)))
          (stream)))))

(define (inspect-procedure o)
  (cond ((primitive-procedure? o)
         (stream (iline "name" (primitive-procedure-name o))
                 (iline "arity" (primitive-procedure-arity o))
                 (iline "doc" (primitive-procedure-documentation o))))
        ((compound-procedure? o)
         (stream (iline "arity" (procedure-arity o))
                 (iline "lambda" (procedure-lambda o))
                 (iline "env" (ignore-errors
                               (lambda () (procedure-environment o))))))
        (else
         (stream (iline "block" (compiled-entry/block o))
                 (with-output-to-string
                   (lambda ()
                     ((environment-lookup #f compiler:disassemble) o)))))))

(define (inspect-code-block block)
  (let loop ((i (compiled-code-block/constants-start block)))
    (if (< i (compiled-code-block/constants-end block))
        (cons-stream (iline i (system-vector-ref block i))
                     (loop (+ i compiled-code-block/bytes-per-object)))
        (stream (iline "debuginfo" (compiled-code-block/debugging-info block))
                (iline "env" (compiled-code-block/environment block))
                (with-output-to-string
                  (lambda ()
                    (compiler:disassemble block)))))))

(define (inspect-scode o)
  (stream (pprint-to-string o)))

(define (probably-scode? o)
  (any (lambda (predicate) (predicate o))
       scode-predicates))

(define scode-predicates
  (list access? assignment? combination? comment?
        conditional? definition? delay? disjunction? lambda?
        quotation? sequence? the-environment? variable?))

(define (inspect-system-pair o)
  (stream (iline "car" (system-pair-car o))
          (iline "cdr" (system-pair-cdr o))))

;;;; Auxilary functions

(define (elisp-false? o) (or (null? o) (eq? o 'NIL)))
(define (elisp-true? o) (not (elisp-false? o)))

(define (->line o)
  (let ((r (write-to-string o 100)))
    (if (car r)
        (string-append (cdr r) " ..")
        (cdr r))))

(define (read-from-string s)
  (read (open-input-string s)))

(define (pprint-to-string o)
  (call-with-output-string
   (lambda (p)
     (let-fluids *unparser-list-breadth-limit* 10
                 *unparser-list-depth-limit* 4
                 *unparser-string-length-limit* 100
                 (lambda ()
                   (pp o p))))))

;; quote keywords, t and nil
(define (quote-special x)
  (cond ((and (symbol? x)
              (or
               (and (> (string-length (symbol->string x)) 0)
                    (char=? #\: (string-ref (symbol->string x) 0)))
               (eq? x 't)))
         `(quote ,x))
        ((and (symbol? x)
              (eq? x 'nil))
         '())
        (else
         x)))

(define swank:completions swank:simple-completions)

;; only return the object, not a list
(define (swank:lookup-presented-object number)
                                        ;(unhash number)
  't
  )

(define (swank:lookup-presented-object-or-lose n)
  (unhash (inexact->exact n)))

;; depends on the implementation of swank:lookup-presented-object
(define (cl:nth-value socket n lst)
  lst)

;; the following two are a clutch, contrib/swank-presentation.el uses #. for some forms :-/
(eval
 '(define (handler:dot port db ctx char1 char2)
    (let ((form (read port)))
      (if (and (list? form)
               (eq? (car form)
                    'swank:lookup-presented-object-or-lose))
          (make-quotation (unhash (inexact->exact (cadr form))))
          form)))
 (->environment '(runtime parser)))

;; TODO: use the port's parser table instead of the global one
(eval
 '(vector-set! (parser-table/special (fluid runtime-parser-table)) (char->integer #\.) handler:dot)
 (->environment '(runtime parser)))

;;;; Tracing
(define-structure itrace id parent-id function params returns)

(define (swank::from-string string)
  (environment-lookup (buffer-env) (string->symbol string)))
(define *traced-functions* '())
(define (swank-trace-dialog:dialog-toggle-trace socket object)
  (if (memq object *traced-functions*)
      (begin
	(set! *traced-functions* (delete object *traced-functions*))
	(internal-untrace! object)
	(format #f "~s is now untraced for trace dialog" (procedure-name object)))
      (begin
	(set! *traced-functions* (cons object *traced-functions*))
	(internal-trace! object)
	(format #f "~s is now traced for trace dialog" (procedure-name object)))))
(define (swank-trace-dialog:clear-trace-tree socket)
  (set! *traces* '())
  (set! *trace-ids* '(nil))
  (set! *trace-counter* 0)
  'nil)
(define (internal-untrace! procedure)
  (unadvise procedure))
(define *trace-counter* 0)
(define (next-trace-count!)
  (set! *trace-counter* (1+ *trace-counter*))
  *trace-counter*)
(define *trace-ids* '(nil))
(define *traces* '())
(define (internal-trace! procedure)
  (advise-entry procedure (lambda (proc params env)
			    (set! *trace-ids* (cons (next-trace-count!) *trace-ids*))))
  (advise-exit procedure (lambda (proc params exit env)
			   (let ((trace-id (car *trace-ids*)))
			     (set! *trace-ids* (cdr *trace-ids*))
			     (set! *traces* (cons (make-itrace trace-id (car *trace-ids*) proc params (list exit)) *traces*))))))

(define (swank-trace-dialog:dialog-untrace symbol)
  (let ((function (swank::from-string (symbol->string symbol))))
    (internal-untrace! function)
    (set! *traced-functions* (delete function *traced-functions*))))

(define (find-itrace number)
  (find (lambda (el) (= number (itrace-id el))) *traces*))

(define (swank-trace-dialog:report-trace-detail socket number)
  (let ((part (find-itrace number)))
    (append (describe-trace-for-emacs part)
	    '(nil)
	    (list (procedure-name (itrace-function part))))))
(define (swank-trace-dialog:dialog-untrace-all)
  (for-each internal-untrace! *traced-functions*)
  (set! *traced-functions* '()))

(define-syntax cl:progn
  (syntax-rules ()
    ((cl:progn a b ...)
     (begin a b ...))))

;; ,in (runtime advice)
(eval
 '(define trace-indentation -2)
 (->environment '(runtime advice)))

(define (swank-trace-dialog:report-specs #!optional socket) ;; may be called inside cl:progn when hitting the untrace button in the trace dialog
  socket
  (if (null? *traced-functions*)
      'nil
      (map (lambda (el)
	     (procedure-name el))
	   *traced-functions*)))
(define (swank-trace-dialog:report-total socket)
  (length *traces*))

(define (describe-trace-for-emacs itrace)
  `(,(itrace-id itrace) ,(itrace-parent-id itrace) ,(procedure-name (itrace-function itrace))
    ,(map (lambda (index param)
	    `(,index ,(write-to-string param)))
	  (iota (length (itrace-params itrace)))
	  (itrace-params itrace))
    ,(map (lambda (index res)
	    `(,index ,(write-to-string res)))
	  (iota (length (itrace-returns itrace)))
	  (itrace-returns itrace))))
(define (swank-trace-dialog:inspect-trace-part socket trace part type)
  (let* ((itrace (find-itrace trace))
	 (values ((case type
		    ((:arg)
		     itrace-params)
		    ((:retval)
		     itrace-returns))
		  itrace))
	 (value (list-ref values part)))
    (inspect-object value)))

(define (swank-trace-dialog:report-partial-tree socket key)
  (let ((res `(,(map (lambda (el)
		       (describe-trace-for-emacs el))
		     *traces*)
	       0 ,key)))
    ;; (set! *traces* '())
    ;; (set! *trace-ids* '(nil))
    ;; (set! *trace-counter* 0)
    res))

(eval '(define (trace-indent level)
         (display (make-string level #\space)))
      (->environment '(runtime advice)))

(eval '(define (trace-display port procedure arguments #!optional result)
         (fresh-line port)
         (let ((width (- (max 40 (output-port/x-size port)) 1))
               (write-truncated
                (lambda (object width)
                  (let ((output (write-to-string object width)))
                    (if (car output)
                        (substring-fill! (cdr output) (- width 3) width #\.))
                    (write-string (cdr output) port)))))
           (if (default-object? result)
               (begin
                 (set! trace-indentation (+ 2 trace-indentation))
                 (trace-indent trace-indentation)
                 (write-string
                  (format #f "(Enter (~a ~a)~%" (procedure-name procedure) (decorated-string-append "" " " "" (map (lambda (arg) (format #f "~a" arg)) arguments)))
                  port))
               (begin
                 (trace-indent trace-indentation)
                 (set! trace-indentation (- trace-indentation 2))
                 (write-string
                  (format #f " Leave ~a " (procedure-name procedure))
                  port)
                 (write-truncated result (- width 2))
                 (write-string ")" port)
                 (newline port)))))
      (->environment '(runtime advice)))

(eval '(define (procedure-name proc)
         (cond ((arity-dispatched-procedure? proc)
                (let ((p (entity-procedure proc)))
                  (procedure-name p)))
               ((compound-procedure? proc)
                (car (lambda-components* (procedure-lambda proc) list)))
               ((compiled-procedure? proc)
                (string->symbol (compiled-procedure/name proc)))
               ((primitive-procedure? proc)
                (primitive-procedure-name proc))
               (else
                (error proc))))
      (->environment '(runtime advice)))

(define procedure-name (access procedure-name (->environment '(runtime advice))))

(define (documentation-string proc)
  "Extract the documentation string from a procedure.

The documentation string is the first expression inside a procedure's body, if it is a string."
  (if (procedure? proc)
      (let ((code (unsyntax (procedure-lambda proc))))
        (if (and (list? code)
                 (> (length code) 3)
                 (string? (list-ref code 2)))
            (list-ref code 2)
            #f))))

(initialize-package!)

#|
to find file of compiled-entry:
(receive (a b) (compiled-entry/filename-and-index list) a)
(receive (a b) (compiled-entry/filename-and-index (entity-procedure +)) a)
|#
