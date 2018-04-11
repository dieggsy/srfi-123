;;; generic-ref-set --- Generic accessor and modifier operators.

;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Copyright © 2017 Diego <dieggsy@pm.me>

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(module srfi-123 (ref
                  ref*
                  ~
                  register-getter-with-setter!)
  (import scheme chicken extras)
  (use srfi-1 srfi-69 srfi-99 box srfi-4 r6rs.bytevectors)

;;; Helpers
  (define-syntax push!
    (syntax-rules ()
      ((_ <list-var> <x>)
       (set! <list-var> (cons <x> <list-var>)))))

  (define (pair-ref pair key)
    (cond ((eqv? 'car key)
           (car pair))
          ((eqv? 'cdr key)
           (cdr pair))
          (else
           (list-ref pair key))))

  (define (list-set! ls idx val)
    (let ((ls (drop ls idx)))
      (if (null? ls)
          (error 'list-set! "index out-of-bounds" idx ls)
          (set-car! ls val) ) ) )

  (define (pair-set! pair key value)
    (cond
     ((eqv? 'car key)
      (set-car! pair value))
     ((eqv? 'cdr key)
      (set-cdr! pair value))
     (else
      (list-set! pair key value))))

;;; Record inspection support
  (define (record-ref record field)
    (let* ((rtd (record-rtd record))
           (accessor (rtd-accessor rtd field)))
      (accessor record)))
  (define (record-set! record field value)
    (let* ((rtd (record-rtd record))
           (mutator (rtd-mutator rtd field)))
      (mutator record value)))
  (define record-getter
    (list (cons record? record-ref)))
  (define record-setter
    (list (cons record? record-set!)))
  (define record-type
    (list record?))

;;; SRFI-4 support

;;; In some implementations, SRFI-4 vectors are also bytevectors.  We accomodate
;;; for those implementations by using generic bytevector-ref/set! procedures
;;; which possibly dispatch to an SRFI-4 type's getter/setter, but also
;;; inserting the SRFI-4 getters/setters into the top-level dispatch tables.
  (define srfi-4-getters
    (list (cons s8vector? s8vector-ref)
          (cons u8vector? u8vector-ref)
          (cons s16vector? s16vector-ref)
          (cons u16vector? u16vector-ref)
          (cons s32vector? s32vector-ref)
          (cons u32vector? u32vector-ref)
          (cons f64vector? f64vector-ref)))
  (define srfi-4-setters
    (list (cons s8vector? s8vector-set!)
          (cons u8vector? u8vector-set!)
          (cons s16vector? s16vector-set!)
          (cons u16vector? u16vector-set!)
          (cons s32vector? s32vector-set!)
          (cons u32vector? u32vector-set!)
          (cons f64vector? f64vector-set!)))
  (define srfi-4-types
    (list s8vector? u8vector? s16vector? u16vector? s32vector? u32vector?
          f64vector? f64vector?))
  (define srfi-4-getters-table (alist->hash-table srfi-4-getters))
  (define srfi-4-setters-table (alist->hash-table srfi-4-setters))
  (define (bytevector-ref bytevector index)
    (let* ((type (find (lambda (pred) (pred bytevector)) srfi-4-types))
           (getter (if type
                       (ref srfi-4-getters-table type)
                       bytevector-u8-ref)))
      (getter bytevector index)))
  (define (bytevector-set! bytevector index value)
    (let* ((type (find (lambda (pred) (pred bytevector)) srfi-4-types))
           (setter (if type
                       (ref srfi-4-setters-table type)
                       bytevector-u8-set!)))
      (setter bytevector index value)))

;;; SRFI-111 boxes support
  (define (srfi-123-box-ref box _field)
    (unbox box))
  (define (srfi-123-box-set! box _field value)
    (set-box! box value))
  (define box-getter (list (cons box? srfi-123-box-ref)))
  (define box-setter (list (cons box? srfi-123-box-set!)))
  (define box-type (list box?))

;;; Main

  (define %ref
    (case-lambda
      ((object field)
       (let ((getter (lookup-getter object))
             (sparse? (sparse-type? object)))
         (if sparse?
             (let* ((not-found (cons #f #f))
                    (result (getter object field not-found)))
               (if (eqv? result not-found)
                   (error "Object has no entry for field." object field)
                   result))
             (getter object field))))
      ((object field default)
       (let ((getter (lookup-getter object)))
         (getter object field default)))))

  (define (%ref* object field . fields)
    (if (null? fields)
        (%ref object field)
        (apply %ref* (%ref object field) fields)))

  (define (%set! object field value)
    (let ((setter (lookup-setter object)))
      (setter object field value)))

  (define ref
    (getter-with-setter
     %ref
     (lambda (object field value)
       (%set! object field value))))

  (define ref*
    (getter-with-setter
     %ref*
     (rec (set!* object field rest0 . rest)
          (if (null? rest)
              (%set! object field rest0)
              (apply set!* (ref object field) rest0 rest)))))

  (define ~ ref*)

  (define $bracket-apply$ ref*)

  (define (lookup-getter object)
    (or (hash-table-ref/default getter-table (type-of object) #f)
        (error "No generic getter for object's type." object)))

  (define (lookup-setter object)
    (or (hash-table-ref/default setter-table (type-of object) #f)
        (error "No generic setter for object's type." object)))

  (define (sparse-type? object)
    (memv (type-of object) sparse-types))

  (define (type-of object)
    (find (lambda (pred) (pred object)) type-list))

  (define getter-table
    (alist->hash-table
     (append
      (list (cons bytevector? bytevector-ref)
            (cons hash-table? hash-table-ref)
            (cons pair? pair-ref)
            (cons string? string-ref)
            (cons vector? vector-ref))
      record-getter
      srfi-4-getters
      box-getter)))

  (define setter-table
    (alist->hash-table
     (append
      (list (cons bytevector? bytevector-set!)
            (cons hash-table? hash-table-set!)
            (cons pair? pair-set!)
            (cons string? string-set!)
            (cons vector? vector-set!))
      record-setter
      srfi-4-setters
      box-setter)))

  (define sparse-types
    (list hash-table?))

  (define type-list
    ;; Although the whole SRFI intrinsically neglects performance, we still use
    ;; the micro-optimization of ordering this list roughly according to most
    ;; likely match.
    (append
     (list hash-table? vector? pair? bytevector? string?)
     srfi-4-types
     box-type
     ;; The record type must be placed last so specific record types (e.g. box)
     ;; take precedence.
     record-type
     ;; Place those types we don't support really last.
     (list boolean? char? eof-object? null? number? port? procedure? symbol?)))

  (define (register-getter-with-setter! type getter sparse?)
    (push! type-list type)
    (set! (~ getter-table type) getter)
    (set! (~ setter-table type) (setter getter))
    (when sparse?
      (push! sparse-types type))))
