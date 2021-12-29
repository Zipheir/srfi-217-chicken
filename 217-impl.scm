;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-record-type <iset>
  (raw-iset trie)
  iset?
  (trie iset-trie : trie))

(define-type iset (struct <iset>))

;;;; Constructors

(: iset (#!rest fixnum --> iset))
(define (iset . args)
  (list->iset args))

(: pair-or-null? (* --> boolean))
(define (pair-or-null? x)
  (or (pair? x) (null? x)))

(: list->iset ((list-of fixnum) --> iset))
(define (list->iset ns)
  (assume (pair-or-null? ns))
  (raw-iset
   (fold (lambda (n t)
           (assume (valid-integer? n))
           (trie-insert t n))
         #f
         ns)))

(: list->iset! (iset (list-of fixnum) --> iset))
(define (list->iset! set ns)
  (assume (iset? set))
  (assume (pair-or-null? ns))
  (raw-iset (fold (lambda (n t)
                    (assume (valid-integer? n))
                    (trie-insert t n))
                  (iset-trie set)
                  ns)))

(: iset-unfold (procedure procedure procedure * -> iset))
(define (iset-unfold stop? mapper successor seed)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (let lp ((trie #f) (seed seed))
    (if (stop? seed)
        (raw-iset trie)
        (let ((n (mapper seed)))
          (assume (valid-integer? n))
          (lp (trie-insert trie n) (successor seed))))))

;; The easy and common step = 1 case is optimized, but all other cases
;; are implemented as unfolds.  This could benefit from more general
;; tuning.
(: make-range-iset (fixnum fixnum #!optional fixnum --> iset))
(define make-range-iset
  (case-lambda
    ((start end) (make-range-iset start end 1))
    ((start end step)
     (assume (valid-integer? start))
     (assume (valid-integer? end))
     (assume (valid-integer? step))
     (assume (if (< end start)
                 (negative? step)
                 (not (zero? step)))
             "Invalid step value.")
     (if (= step 1)
         (raw-iset (range-trie-step-1 start end))  ; Tuned case.
         (let ((stop? (if (positive? step)
                          (lambda (i) (>= i end))
                          (lambda (i) (<= i end)))))
           (iset-unfold stop?
                        values
                        (lambda (i) (+ i step))
                        start))))))

;;;; Predicates

(: iset-contains? (iset fixnum --> boolean))
(define (iset-contains? set n)
  (assume (iset? set))
  (assume (valid-integer? n))
  (trie-contains? (iset-trie set) n))

(: iset-empty? (iset --> boolean))
(define (iset-empty? set)
  (assume (iset? set))
  (not (iset-trie set)))

(: iset-disjoint? (iset iset --> boolean))
(define (iset-disjoint? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (trie-disjoint? (iset-trie set1) (iset-trie set2)))

;;;; Accessors

(: iset-member (forall (a) (iset fixnum a --> (or fixnum a))))
(define (iset-member set elt default)
  (if (iset-contains? set elt)
      elt
      default))

(: iset-min (iset --> (or false fixnum)))
(define (iset-min set)
  (assume (iset? set))
  (trie-min (iset-trie set)))

(: iset-max (iset --> (or false fixnum)))
(define (iset-max set)
  (assume (iset? set))
  (trie-max (iset-trie set)))

;;;; Updaters

(: iset-adjoin (iset #!rest fixnum --> iset))
(define iset-adjoin
  (case-lambda
    ((set n)
     (assume (iset? set))
     (assume (valid-integer? n))
     (raw-iset (trie-insert (iset-trie set) n)))
    ((set . ns)
     (raw-iset
      (fold (lambda (n t)
              (assume (valid-integer? n))
              (trie-insert t n))
            (iset-trie set)
            ns)))))

(: iset-adjoin! (iset #!rest fixnum --> iset))
(define (iset-adjoin! set . ns)
  (apply iset-adjoin set ns))

(: iset-delete (iset #!rest fixnum --> iset))
(define iset-delete
  (case-lambda
    ((set n)
     (assume (iset? set))
     (assume (valid-integer? n))
     (raw-iset (trie-delete (iset-trie set) n)))
    ((set . ns) (iset-delete-all set ns))))

(: iset-delete! (iset #!rest fixnum --> iset))
(define (iset-delete! set n) (iset-delete set n))

(: iset-delete-all (iset (list-of fixnum) --> iset))
(define (iset-delete-all set ns)
  (assume (iset? set))
  (assume (or (pair? ns) (null? ns)))
  (iset-difference set (list->iset ns)))

(: iset-delete-all! (iset (list-of fixnum) --> iset))
(define (iset-delete-all! set ns)
  (iset-delete-all set ns))

;; Thanks to the authors of SRFI 146 for providing examples
;; of how to implement this shoggoth.
(: iset-search (iset fixnum procedure procedure -> iset))
(define (iset-search set elt failure success)
  (assume (iset? set))
  (assume (valid-integer? elt))
  (assume (procedure? failure))
  (assume (procedure? success))
  (call-with-current-continuation
   (lambda (return)
     (let-values
      (((trie obj)
        (trie-search (iset-trie set)
                     elt
                     (lambda (insert ignore)
                       (failure insert
                                (lambda (obj)
                                  (return set obj))))
                     (lambda (key update remove)
                       (success
                        key
                        (lambda (new obj)
                          (assume (valid-integer? new))
                          (if (fx=? key new)
                              (update new obj)
                              (return (iset-adjoin (iset-delete set key)
                                                   new)
                                      obj)))
                        remove)))))
       (values (raw-iset trie) obj)))))

(: iset-search! (iset fixnum procedure procedure -> noreturn))
(define (iset-search! set elt failure success)
  (iset-search set elt failure success))

(: iset-delete-min (iset --> fixnum iset))
(define (iset-delete-min set)
  (assume (iset? set))
  (let*-values (((trie) (iset-trie set))
                ((n trie*) (trie-delete-min trie)))
    (values n (raw-iset trie*))))

(: iset-delete-max (iset --> fixnum iset))
(define (iset-delete-max set)
  (assume (iset? set))
  (let*-values (((trie) (iset-trie set))
                ((n trie*) (trie-delete-max trie)))
    (values n (raw-iset trie*))))

(: iset-delete-min! (iset --> fixnum iset))
(define (iset-delete-min! set) (iset-delete-min set))
(: iset-delete-max! (iset --> fixnum iset))
(define (iset-delete-max! set) (iset-delete-max set))

;;;; The whole iset

(: iset-size (iset --> integer))
(define (iset-size set)
  (assume (iset? set))
  (trie-size (iset-trie set)))

(: iset-find ((fixnum -> *) iset procedure -> *))
(define (iset-find pred set failure)
  (assume (procedure? failure))
  (call-with-current-continuation
   (lambda (return)
     (or (iset-fold (lambda (n _)
                      (and (pred n) (return n)))
                    #f
                    set)
         (failure)))))

(: iset-count ((fixnum -> *) iset -> integer))
(define (iset-count pred set)
  (assume (procedure? pred))
  (iset-fold (lambda (n acc)
               (if (pred n) (+ 1 acc) acc))
             0
             set))

(: iset-any? ((fixnum -> *) iset -> boolean))
(define (iset-any? pred set)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (iset-fold (lambda (n _)
                  (and (pred n) (return #t)))
                #f
                set))))

(: iset-every? ((fixnum -> *) iset -> boolean))
(define (iset-every? pred set)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (iset-fold (lambda (n _)
                  (or (pred n) (return #f)))
                #t
                set))))

;;;; Mapping and folding

(: iset-map ((fixnum -> fixnum) iset -> iset))
(define (iset-map proc set)
  (assume (procedure? proc))
  (raw-iset
   (iset-fold (lambda (n t)
                (let ((n* (proc n)))
                  (assume (valid-integer? n*))
                  (trie-insert t (proc n))))
              #f
              set)))

(define (unspecified)
  (if #f #f))

(: iset-for-each (procedure iset -> undefined))
(define (iset-for-each proc set)
  (assume (procedure? proc))
  (iset-fold (lambda (n _)
               (proc n)
               (unspecified))
             (unspecified)
             set))

(: iset-fold (procedure * iset -> *))
(define (iset-fold proc nil set)
  (assume (procedure? proc))
  (assume (iset? set))
  (trie-fold proc nil (iset-trie set)))

(: iset-fold-right (procedure * iset -> *))
(define (iset-fold-right proc nil set)
  (assume (procedure? proc))
  (assume (iset? set))
  (trie-fold-right proc nil (iset-trie set)))

(: iset-filter ((fixnum -> *) iset -> iset))
(define (iset-filter pred set)
  (assume (procedure? pred))
  (assume (iset? set))
  (raw-iset (trie-filter pred (iset-trie set))))

(: iset-remove ((fixnum -> *) iset -> iset))
(define (iset-remove pred set)
  (assume (procedure? pred))
  (assume (iset? set))
  (raw-iset (trie-filter (lambda (n) (not (pred n))) (iset-trie set))))

(: iset-partition ((fixnum -> *) iset -> iset iset))
(define (iset-partition pred set)
  (assume (procedure? pred))
  (assume (iset? set))
  (let-values (((tin tout) (trie-partition pred (iset-trie set))))
    (values (raw-iset tin) (raw-iset tout))))

(: iset-partition! ((fixnum -> *) iset -> iset iset))
(define (iset-partition! pred set)
  (iset-partition pred set))

;;;; Copying and conversion

(: iset-copy (iset --> iset))
(define (iset-copy set)
  (assume (iset? set))
  (raw-iset (copy-trie (iset-trie set))))

(: iset->list (iset --> (list-of fixnum)))
(define (iset->list set)
  (iset-fold-right cons '() set))

;;;; Comparison

(: iset=? (iset iset #!rest iset --> boolean))
(define (iset=? set1 set2 . sets)
  (assume (iset? set1))
  (let ((iset-eq1 (lambda (set)
                    (assume (iset? set))
                    (or (eqv? set1 set)
                        (trie=? (iset-trie set1) (iset-trie set))))))
    (and (iset-eq1 set2)
         (or (null? sets)
             (every iset-eq1 sets)))))

(: iset<? (iset iset #!rest iset --> boolean))
(define (iset<? set1 set2 . sets)
  (assume (iset? set1))
  (assume (iset? set2))
  (let lp ((t1 (iset-trie set1)) (t2 (iset-trie set2)) (sets sets))
    (and (trie-proper-subset? t1 t2)
         (or (null? sets)
             (lp t2 (iset-trie (car sets)) (cdr sets))))))

(: iset>? (iset iset #!rest iset --> boolean))
(define (iset>? set1 set2 . sets)
  (assume (iset? set1))
  (assume (iset? set2))
  (let lp ((t1 (iset-trie set1)) (t2 (iset-trie set2)) (sets sets))
    (and (trie-proper-subset? t2 t1)
         (or (null? sets)
             (lp t2 (iset-trie (car sets)) (cdr sets))))))

(: iset<=? (iset iset #!rest iset --> boolean))
(define (iset<=? set1 set2 . sets)
  (assume (iset? set1))
  (assume (iset? set2))
  (let lp ((t1 (iset-trie set1)) (t2 (iset-trie set2)) (sets sets))
    (and (memv (trie-subset-compare t1 t2) '(less equal))
         (or (null? sets)
             (lp t2 (iset-trie (car sets)) (cdr sets))))))

(: iset>=? (iset iset #!rest iset --> boolean))
(define (iset>=? set1 set2 . sets)
     (assume (iset? set1))
     (assume (iset? set2))
     (let lp ((t1 (iset-trie set1)) (t2 (iset-trie set2)) (sets sets))
       (and (memv (trie-subset-compare t1 t2) '(greater equal))
            (or (null? sets)
                (lp t2 (iset-trie (car sets)) (cdr sets))))))

;;;; Set theory operations

(: iset-union (iset iset #!rest iset --> iset))
(define iset-union
  (case-lambda
    ((set1 set2)
     (assume (iset? set1))
     (assume (iset? set2))
     (raw-iset (trie-union (iset-trie set1) (iset-trie set2))))
    ((set . rest)
     (raw-iset (fold (lambda (s t)
                       (assume (iset? s))
                       (trie-union (iset-trie s) t))
                     (iset-trie set)
                     rest)))))

(: iset-union! (iset iset #!rest iset --> iset))
(define (iset-union! set . rest)
  (apply iset-union set rest))

(: iset-intersection (iset iset #!rest iset --> iset))
(define iset-intersection
  (case-lambda
    ((set1 set2)
     (assume (iset? set1))
     (assume (iset? set2))
     (raw-iset (trie-intersection (iset-trie set1) (iset-trie set2))))
    ((set . rest)
     (assume (iset? set))
     (raw-iset (fold (lambda (s t)
                       (assume (iset? s))
                       (trie-intersection (iset-trie s) t))
               (iset-trie set)
               rest)))))

(: iset-intersection! (iset iset #!rest iset --> iset))
(define (iset-intersection! set . rest)
  (apply iset-intersection set rest))

(: iset-difference (iset iset #!rest iset --> iset))
(define iset-difference
  (case-lambda
    ((set1 set2)              ; fast path
     (assume (iset? set1))
     (assume (iset? set2))
     (raw-iset (trie-difference (iset-trie set1) (iset-trie set2))))
    ((set . rest)
     (assume (iset? set))
     (raw-iset
      (trie-difference (iset-trie set)
                       (iset-trie (apply iset-union rest)))))))

(: iset-difference! (iset iset #!rest iset --> iset))
(define (iset-difference! set . rest)
  (apply iset-difference set rest))

(: iset-xor (iset iset --> iset))
(define (iset-xor set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (if (eqv? set1 set2)  ; quick check
      (iset)
      (raw-iset
       (trie-xor (iset-trie set1) (iset-trie set2)))))

(: iset-xor! (iset iset --> iset))
(define (iset-xor! set1 set2) (iset-xor set1 set2))

;;;; Subsets

(: isubset= (iset fixnum --> iset))
(define (isubset= set k)
  (if (iset-contains? set k) (iset k) (iset)))

(: iset-open-interval (iset fixnum fixnum --> iset))
(define (iset-open-interval set low high)
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-iset (subtrie-interval (iset-trie set) low high #f #f)))

(: iset-closed-interval (iset fixnum fixnum --> iset))
(define (iset-closed-interval set low high)
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-iset (subtrie-interval (iset-trie set) low high #t #t)))

(: iset-open-closed-interval (iset fixnum fixnum --> iset))
(define (iset-open-closed-interval set low high)
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-iset (subtrie-interval (iset-trie set) low high #f #t)))

(: iset-closed-open-interval (iset fixnum fixnum --> iset))
(define (iset-closed-open-interval set low high)
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-iset (subtrie-interval (iset-trie set) low high #t #f)))

(: isubset< (iset fixnum --> iset))
(define (isubset< set k)
  (assume (iset? set))
  (assume (valid-integer? k))
  (raw-iset (subtrie< (iset-trie set) k #f)))

(: isubset<= (iset fixnum --> iset))
(define (isubset<= set k)
  (assume (iset? set))
  (assume (valid-integer? k))
  (raw-iset (subtrie< (iset-trie set) k #t)))

(: isubset> (iset fixnum --> iset))
(define (isubset> set k)
  (assume (iset? set))
  (assume (valid-integer? k))
  (raw-iset (subtrie> (iset-trie set) k #f)))

(: isubset>= (iset fixnum --> iset))
(define (isubset>= set k)
  (assume (iset? set))
  (assume (valid-integer? k))
  (raw-iset (subtrie> (iset-trie set) k #t)))
