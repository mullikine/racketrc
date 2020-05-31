;; If I set this, I can C-c C-c the file in emacs BUT the racket repl (racket -iI racket) will not load the file.
;; #!/usr/bin/env racket
;; #lang racket

#| $HOME/notes2018/ws/racket/scratch/scratch.rkt |#

;; ~syntax-parse~ is the primitive of the most advanced syntax transformer in
(require syntax/parse syntax/parse/define)

(require racket/trace) ;; Nice tracing
(define sum (λ (x y) (+ x y)))
(trace sum) ;; Adds tracing to the sum function
;; (sum 4 4)

;; pattern-matching
(require racket/match)

(require racket/format) ;; Needed for ~a and str

(require racket/string) ;; Needed for string-join

(require shell/pipeline-macro)

(require file/glob)
#| (glob "/*") |#
#| (glob "{foo,qux}-{bar,baz}.rkt") |#

#|
(run-pipeline =unix-pipe= ls -l =unix-pipe= tv)
(run-pipeline =object-pipe= list 1 2 3 =object-pipe= second)
(run-pipeline =unix-pipe= ls -l =object-pipe= string-upcase)
(run-pipeline =unix-pipe= ls -l =object-pipe= string-upcase =unix-pipe= tv)
|#


#|
;; Either of these works to get readline. After installing Racket-v7 it suspect I no longer need this. This isn't confirmed.
;; The following line loads `xrepl' support
(require xrepl)
;; load readline support (added by `install-readline!')
(require readline/rep)
|#


;; How to conditionally evaluate parts of racketrc depending on the language set by the interpreter
#| racket -il hackett |#


(module racketrc racket/base
  (require racket/pretty syntax/srcloc)
  (print-graph #f)
  (pretty-print-columns 200)
  (current-print pretty-print-handler)

  (let ((old-error-display-handler (error-display-handler)))
    (error-display-handler
      (λ (str exn)
        (when (exn:srclocs? exn)
          (for ((srcloc ((exn:srclocs-accessor exn) exn)))
            (displayln (source-location->string srcloc))))
        (old-error-display-handler str exn)))))


;; (require syntax/parse/define (for-syntax racket))
;; (define-syntax-parser Cstat
;;   ([_ ((~datum +) x ...)]
;;    #:with x* (string-join (map (lambda (x) (writeln x)
;;                                  (if (symbol? (syntax-e x))
;;                                      (symbol->string (syntax-e x))
;;                                      (number->string (syntax-e x))))
;;                                (attribute x)) " + ")
;;    #'(string-append 'x*)))
;; (define-syntax-parser Cfun
;;   ([_ name:id ((type:expr param:id) ...) (return:expr ...)
;;       body ...+]
;;    #'(string-append
;;        (symbol->string 'return) ...
;;        " "
;;        (symbol->string 'name)
;;        "("
;;        (string-append (symbol->string 'type) (symbol->string 'param) ",") ...
;;        ") {\n"
;;        (Cstat body) ...
;;        "}"
;;        )))
;;
;; ; (Cfun whatever [(int value) (char b)] (void)
;; ;       (+ 1 2 value))


(require spipe)


(require math/matrix)
(matrix ([1 1] [1 0]))


;; cats a file
;; (file->string "/home/shane/notes2018/ws/emacs-lisp-elisp/remember.org")

(require python/config)
(enable-cpyimport!) ;; this is noisy!


;; $HOME/notes2018/ws/racket/for.rkt

(require racket/port)

(define (readEntry)
 (newline)
 (regexp-match* #rx"(([a-zA-Z0-9]+)|(\".*\")|(\\(.*\\)))"
                (read-line (current-input-port))))


(require snappy)

;; how do i get a string compatible with compress?
#| (compress "oetnsuhnseto")  |#

; this may automatically install
;; (require (planet williams/describe/describe)) ;; the old way to import. auto download
;; use this for describing things. it's like type-of
(require describe) ;; the new way

;; EXAMPLES
;; racket also has class-of
;; (variant (λ (x) x))
;; (describe #\a)

;; megapasack
(require megaparsack megaparsack/text) ; import the basic parser functions, as well as some built-in parsers for parsing textual data.

(parse-string integer/p "42") ; parse an integer



;; a cosmetic macro -- adds then, else
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))
(my-if #t then (+ 3 4) else 72)
(my-if #f then (+ 3 4) else 72)


; (my-if foo then bar else baz) -> (if foo bar baz)

;; a macro to replace an expression with another one
;; Not sure what this does
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))
;; (define-syntax comment-out
;;   (syntax-rules ()
;;     [(comment-out ignore instead) (instead)])) ; the extra parens here around instead would ; add calling as a function, which is incorrect
;;(comment-out (car null) (+ 3 4))
;; (+ 3 4)
;; (comment-out (car null) (+ 3 4))


; replace define with a memoized version
(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       ; store the cache as a hash of args => result
       (let ([results (make-hash)])
         ; need to do this to capture both the names and the values
         (lambda (args ...)
           ((lambda vals
              ; if we haven't calculated it before, do so now
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              ; return the cached result
              (hash-ref results vals))
            args ...))))]))


(require memoize)
(define/memo (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

; Tupper's "self-referential" formula
; Encodes a bitmap as an integer
;; (define (tupper k)
;;   (flomap->bitmap
;;    (build-flomap*
;;     1 106 17
;;     (λ (x y)
;;       (set! y (+ y k))
;;       (set! x (- 105 x))
;;       (cond
;;         [(< 1/2 (floor (mod (* (floor (/ y 17)) (expt 2 (- (* -17 (floor x)) (mod (floor y) 17)))) 2)))
;;          (vector 0)]
;;         [else
;;          (vector 1)])))))


;; ;; This may work in some random scheme but not in racket
;; (define-syntax incf
;;   (syntax-rules ()
;;     ((_ x)   (begin (set! x (+ x 1)) x))
;;     ((_ x n) (begin (set! x (+ x n)) x))))
;;(define-syntax decf
;;  (syntax-rules ()
;;    ((_ x)   (incf x -1))
;;    ((_ x n) (incf x (- n)))))



;; This is one way to alias a function
(define-syntax 1+
  (syntax-rules ()
    ((_ x) (add1 x))))


#| Make macros instead of snippets |#
#| $HOME/notes2018/ws/racket/examples/racket-macros.rkt |#

;; I want to make macros that I can simply expand in my editor
;; One day (hopefully soon) I will figure out how to expand macros as snippets.

;; (unless (and (stx-list? x)
;;              (> (length l) 3))
;;   (raise-syntax-error
;;    #f
;;    "bad form"
;;    x))

; sets as unordered lists:

; a set for now is defined as as being able
; to undergo the following operations

; 1)  element-of-set? checks if x is in set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 2) adjoin set
; cons (set element) if element not in set
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
  (cons x set)))

; 3) intersection-set T(n) = revisit
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; intersection-set takes 2 sets
; returns a set that contains only the common elements

; sets as ordered lists - this speeds up traversal

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((= x (car set)) true)
;;         ((< x (car set)) false)
;;         (else (element-of-set? x (cdr set)))))

; This is on average T(n) = revisit
;; (define (intersection-set set1 set2)
;;   (if (or (null? set1) (null? set2))
;;       '()
;;       (let ((x1 (car set1)) (x2 (car set2)))
;;         (cond ((= x1 x2)
;;                (cons x1
;;                      (intersection-set (cdr set1)
;;                                        (cdr set2))))
;;               ((< x1 x2)
;;                (intersection-set (cdr set1) set2))
;;               ((< x2 x1)
;;                (intersection-set set1 (cdr set2)))))))

; it can get even better than ordered lists
; binary trees
; each node of the tree holds one value/entry
; and links to 2 other nodes
; which may be empty
; the left is smaller, the right is greater

; define a tree based on procedure:
; each node will be a list of 3 items
; 1 - the entry at the node
; 2 - the left subtree
; 3 - the right subtree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; this needs a new element-of-set?
; T(n) = revisit

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((= x (entry set)) true)
;;         ((< x (entry set))
;;          (element-of-set? x (left-branch set)))
;;         ((> x (entry set))
;;          (element-of-set? x (left-branch set)))))

; now adjoin-set
;; (define (adjoin-set x set)
;;   (cond ((null? set) (make-tree x '() '()))
;;         ((= x (entry set)) set)
;;         ((< x (entry set))
;;          (make-tree (entry set)
;;                     (adjoin-set x (left-branch set))
;;                     (right-branch set)))
;;         ((> x (entry set))
;;          (make-tree (entry set)
;;                     (left-branch set)
;;                     (adjoin-set x (right-branch set))))))

(define (str s)
  (~a s))

(require data/applicative
         data/monad
         megaparsack/base
         megaparsack/combinator
         racket/contract
         racket/list
         racket/function)

;; (define (slist->string slst)
;;   (cond ((empty? slst) "")
;;         ((empty? (rest slst)) (symbol->string (first slst)))
;;         (else (string-append (symbol->string (first slst))
;;                              " "
;;                              (slist->string (rest slst))))))

;; Using higher-level functions
;; symbol list to string
(define (slist->string slst)
  (string-join (map symbol->string slst) " "))

(slist->string '(red yellow blue green))

(define strlist (list "red" "yellow" "blue" "green"))
(string-join strlist " ")

(define-syntax-rule (join-lines l)
  (string-join (map str l) "\n"))

(string-join (map str (glob "/*")) "\n")
(join-lines (map str (glob "/*")))

(require json)
(require racket/file)                   ; file->string
(string->jsexpr "{\"pancake\" : 5, \"waffle\" : 7}")
(string->jsexpr (file->string "/home/shane/var/smulliga/source/git/mullikine/marketing/pipeline/json/pl-flow-gopasspw-gopass-before-bundles.json"))

;; ewwlinks +/"4.5.1 Function Shorthand" "https://docs.racket-lang.org/guide/define.html"
;; This is not valid racket
;; (define
;;   (file->jsexpr arg ...)
;;   (string->jsexpr (file->string body
;;                                 ...+)))

(define
  (file->json fp)
  (string->jsexpr (file->string fp)))
(file->json "/home/shane/var/smulliga/source/git/mullikine/marketing/pipeline/json/pl-flow-gopasspw-gopass-before-bundles.json")