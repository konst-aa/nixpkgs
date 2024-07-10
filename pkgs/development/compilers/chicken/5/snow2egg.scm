(import (srfi 1)
        (chicken string)
        (chicken pretty-print)
        (chicken process))

(define-syntax anif
  (syntax-rules (:=)
    ((_ (bool := sym) x y)
     (let ((sym bool))
       (if sym x y)))
    ((_ b x)
     (anif b x #f))))

(define pkg-alist (cdr (read)))

(define ref assoc)

(define (sref alist key)
  ;; reaches
  ;; ((authors "john doe" ...) ...)
  ;;            ^^^^
  (anif ((ref key alist) := t)
        (cadr t)))

(define (pref key default)
  ;; lookup key in package
  (anif ((sref pkg-alist key) := t)
        t
        default))

(define (lref key)
  (let ((t (cdr (ref 'library pkg-alist))))
    ;; gets tail of alist reference in library
    (anif ((ref key t) := t)
          (cdr t))))

;; flattens module names and intersperses with delim
(define (flatten-delim delim)
  (letrec
    ((self
       (lambda (dep)
         (if (list? dep)
           (string-intersperse (map self dep) delim)
           (->string dep)))))
    self))

(define cond-expanded-deps
  (anif ((lref 'cond-expand) := t)
        (eval
          `(cond-expand
            ,@(map (lambda (clause)
                     `(,(car clause)
                       ,`',(map (flatten-delim "-") (cdadr clause))))
                   t)))
        (list)))


;; Rename test script for chicken if it exists
(anif ((pref 'test #f) := t)
      (process-run "mv" (list t "tests/run.scm")))

(pretty-print
  `((author ,(pref 'authors "UNKNOWN AUTHOR"))
    (synopsis ,(pref 'description "NO SYNOPSIS"))
    ;; Packages like chibi-optional violate semantic versioning
    (version ,(string-intersperse
                (take (string-split (pref 'version "NO VERSION") ".")
                      3)
                "."))
    (license ,(symbol->string (pref 'license 'NO-LICENSE)))
    (category uncategorized) ; placeholder
    (dependencies r7rs ;,@cond-expanded-deps ,@(lref 'depends)
                  )
    (test-dependencies r7rs ;,@cond-expanded-deps ;,@(lref 'depends)
                       )
    (component-options
      ;; used https://gitlab.com/rgherdt/scheme-json-rpc/-/blob/master/json-rpc.egg as reference
      (csc-options "-d0" "-O2" "-X" "r7rs" "-R" "r7rs"))
    (components
      (extension ,(string->symbol
                    (let ((name (car (lref 'name))))
                      (if (and (pair? name)
                               (eq? (car name) 'srfi))
                        ((flatten-delim "-") name) ; unsure why this works this way
                        ((flatten-delim ".") name))))
                 (source ,@(lref 'path)))))
)

