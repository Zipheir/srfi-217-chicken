;;; This file implements integers sets as compressed binary radix
;;; trees (AKA Patricia tries), as described by Chris Okasaki and
;;; Andrew Gill in "Fast Mergeable Integer Maps" (1998).  Integers
;;; in big-endian binary encoding are stored in a trie structure
;;; which allows fast lookup, insertion, and set-theoretical
;;; operations (union, intersection, etc.)
;;;
;;; To make efficient use of space, "buddy compression" is used
;;; to store runs of adjacent integers as bitmaps in single leaves.
;;; Integers are broken up into a prefix and a string of low-order
;;; bits; the latter are represented by setting the appropriate bit
;;; in a leaf's bitmap.  Since a bitmap is represented by a single
;;; fixnum, we can represent a 5-bit suffix on most 64-bit Schemes,
;;; and a 4-bit suffix on most 32-bit implementations.  (We pay a
;;; tax for dynamic typing.)
;;;
;;; A trie is represented by #f (the empty trie), a leaf, or a branch.
;;;
;;; Throughout this code, the empty trie (#f) is always returned
;;; as an explicit value, not, e.g. as the default value of an
;;; (and ...) expression, to clarify its use as a trie value.
;;;
;;; The interface procedures of this library are the `trie-' forms
;;; not prefixed with '%'.  This flimsy naming scheme is no substitute
;;; for a real submodule, but not every Scheme supports those.

;;;; Types and constructors

;;; Leaves and branches are constructed so as to maintain the following
;;; invariant: Every leaf and every subtrie contains at least one value.
;;; This means that the empty trie (#f) never appears as a subtrie.

(module (srfi 217 internal)
  (trie-contains? trie-min trie-max trie-insert trie-fold trie-fold-right
   trie-partition trie-filter trie-delete trie-delete-min trie-delete-max
   trie-search trie-union trie-xor trie-intersection trie-difference
   copy-trie trie-size trie=? trie-subset-compare trie-proper-subset?
   trie-disjoint? subtrie< subtrie> subtrie-interval
   )

(import scheme
        (chicken base)
        (chicken condition)
        (chicken type)
        (srfi 143)
        typed-records
        )

(define-record-type <leaf>
  (raw-leaf prefix bitmap)
  leaf?
  (prefix leaf-prefix : fixnum)
  (bitmap leaf-bitmap : fixnum))

;; The primary leaf constructor creates a leaf only if `bitmap'
;; contains at least one value.
(: leaf (fixnum fixnum -> (or false (struct <leaf>))))
(define (leaf prefix bitmap)
  (if (fxpositive? bitmap)
      (raw-leaf prefix bitmap)
      #f))

;; Shorthand for extracting leaf elements.
(define-syntax let*-leaf
  (syntax-rules ()
    ((_ () e1 e2 ...) (begin e1 e2 ...))
    ((_ (((p b) expr) . binds) . body)
     (let ((lf expr))
       (let ((p (leaf-prefix lf)) (b (leaf-bitmap lf)))
         (let*-leaf binds . body))))))

(define-type trie (or false (struct <leaf>) (struct <branch>)))
(define-type non-empty-trie (or (struct <leaf>) (struct <branch>)))

(define-record-type <branch>
  (raw-branch prefix branching-bit left right)
  branch?
  (prefix branch-prefix : fixnum)
  (branching-bit branch-branching-bit : fixnum)
  (left branch-left : trie)
  (right branch-right : trie))
  ;(left branch-left : (or false (struct <leaf>) (struct <branch>)))
  ;(right branch-right : (or false (struct <leaf>) (struct <branch>))))

;; The primary branch constructor creates a branch only if the subtrees
;; are non-empty.
(: branch (fixnum fixnum trie trie -> trie))
(define (branch prefix mask trie1 trie2)
  (cond ((not trie1) trie2)
        ((not trie2) trie1)
        (else (raw-branch prefix mask trie1 trie2))))

;; Shorthand for extracting branch elements.
(define-syntax let*-branch
  (syntax-rules ()
    ((_ () e1 e2 ...) (begin e1 e2 ...))
    ((_ (((p m l r) expr) . binds) . body)
     (let ((b expr))
       (let ((p (branch-prefix b))
             (m (branch-branching-bit b))
             (l (branch-left b))
             (r (branch-right b)))
         (let*-branch binds . body))))))

;;;; Bitwise constants and procedures

;; Constant.  Gives the maximum number of integers storable
;; in a single leaf.
(: leaf-bitmap-size fixnum)
(define leaf-bitmap-size
  (expt 2 (inexact->exact (floor (log fx-width 2)))))

(: suffix-mask fixnum)
(define suffix-mask (- leaf-bitmap-size 1))
(: prefix-mask fixnum)
(define prefix-mask (fxnot suffix-mask))

(define (valid-integer? x) (fixnum? x))

;; Zero the bits of k at and below (BE) the set bit of m.
(: mask (fixnum fixnum -> fixnum))
(define (mask k m)
  (if (fx=? m fx-least)
      0
      (fxand k (fxxor (fxnot (fx- m 1)) m))))

;; Does the m-masked prefix of k match p?
(: match-prefix? (fixnum fixnum fixnum -> boolean))
(define-inline (match-prefix? k p m)
  (fx=? (mask k m) p))

(: branching-bit (fixnum fixnum fixnum fixnum -> fixnum))
(define (branching-bit p1 m1 p2 m2)
  (if (fxnegative? (fxxor p1 p2))
      fx-least        ; different signs
      (highest-bit-mask (fxxor p1 p2) (fxmax 1 (fx* 2 (fxmax m1 m2))))))

;; Two's-complement trick.
(: lowest-bit-mask (fixnum -> fixnum))
(define-inline (lowest-bit-mask b)
  (fxand b (fxneg b)))

(: highest-bit-mask (fixnum fixnum -> fixnum))
(define (highest-bit-mask k guess-m)
  (let lp ((x (fxand k (fxnot (fx- guess-m 1)))))
    (let ((m (lowest-bit-mask x)))
      (if (fx=? x m)
          m
          (lp (fx- x m))))))

;; FIXME: To improve.
(: highest-set-bit (fixnum -> fixnum))
(define (highest-set-bit k)
  (fxfirst-set-bit (highest-bit-mask k 1)))

(: zero-bit? (fixnum fixnum -> boolean))
(define-inline (zero-bit? k m)
  (fxzero? (fxand k m)))

(: isuffix (fixnum -> fixnum))
(define-inline (isuffix k)
  (fxand k suffix-mask))

(: iprefix (fixnum -> fixnum))
(define-inline (iprefix k)
  (fxand k prefix-mask))

(: ibitmap (fixnum -> fixnum))
(define-inline (ibitmap k)
  (fxarithmetic-shift 1 (isuffix k)))

(: bitmap-delete (fixnum fixnum -> fixnum))
(define (bitmap-delete bitmap key)
  (fxand bitmap (fxnot (ibitmap key))))

(: bitmap-delete-min (fixnum -> fixnum))
(define (bitmap-delete-min b)
  (fxand b (fxnot (lowest-bit-mask b))))

(: bitmap-delete-max (fixnum -> fixnum))
(define (bitmap-delete-max b)
  (fxand b (fxnot (highest-bit-mask b (lowest-bit-mask b)))))

;;;; Predicates and accessors

(: trie-contains? (trie fixnum -> boolean))
(define (trie-contains? trie key)
  (and trie
       (if (leaf? trie)
           (and (fx=? (iprefix key) (leaf-prefix trie))
                (not (fxzero? (fxand (ibitmap key) (leaf-bitmap trie)))))
           (let*-branch (((p m l r) trie))
             (and (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (trie-contains? l key)
                      (trie-contains? r key)))))))

;; Assumes that trie is non-empty.
(: trie-min (trie -> fixnum))
(define (trie-min trie)
  (letrec
   ((search
     (lambda (t)
       (and t
            (if (leaf? t)
                (fx+ (leaf-prefix t) (fxfirst-set-bit (leaf-bitmap t)))
                (search (branch-left t)))))))
    (if (branch? trie)
        (if (fxnegative? (branch-branching-bit trie))
            (search (branch-right trie))
            (search (branch-left trie)))
        (search trie))))

;; Assumes that trie is non-empty.
(: trie-max (trie -> fixnum))
(define (trie-max trie)
  (letrec
   ((search
     (lambda (t)
       (and t
            (if (leaf? t)
                (fx+ (leaf-prefix t) (highest-set-bit (leaf-bitmap t)))
                (search (branch-right t)))))))
    (if (branch? trie)
        (if (fxnegative? (branch-branching-bit trie))
            (search (branch-left trie))
            (search (branch-right trie)))
        (search trie))))

;;;; Insert

(: %trie-insert-parts (trie fixnum fixnum -> non-empty-trie))
(define (%trie-insert-parts trie prefix bitmap)
  (letrec
   ((ins
     (lambda (t)
       (cond ((not t) (raw-leaf prefix bitmap))
             ((leaf? t)
              (let*-leaf (((p b) t))
                (if (fx=? prefix p)
                    (raw-leaf prefix (fxior b bitmap))
                    (%trie-join prefix 0 (raw-leaf prefix bitmap) p 0 t))))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? prefix p m)
                    (if (zero-bit? prefix m)
                        (branch p m (ins l) r)
                        (branch p m l (ins r)))
                    (%trie-join prefix 0 (raw-leaf prefix bitmap) p m t))))))))
    (ins trie)))

(: trie-insert (trie fixnum -> non-empty-trie))
(define (trie-insert trie key)
  (%trie-insert-parts trie (iprefix key) (ibitmap key)))

;;;; Iterators and filters

;; Fold trie in increasing numerical order.
(: trie-fold (procedure * trie -> *))
(define (trie-fold proc nil trie)
  (letrec
   ((cata
     (lambda (b t)
       (cond ((not t) b)
             ((leaf? t)
              (fold-left-bits (leaf-prefix t) proc b (leaf-bitmap t)))
             (else
              (cata (cata b (branch-left t)) (branch-right t)))))))
    (if (branch? trie)
        (let*-branch (((p m l r) trie))
          (if (fxnegative? m)
              (cata (cata nil r) l)
              (cata (cata nil l) r)))
        (cata nil trie))))

(: fold-left-bits (fixnum procedure * fixnum -> *))
(define (fold-left-bits prefix proc nil bitmap)
  (let loop ((bm bitmap) (acc nil))
    (if (fxzero? bm)
        acc
        (let* ((mask (lowest-bit-mask bm))
               (bi (fxfirst-set-bit mask)))
          (loop (fxxor bm mask) (proc (fx+ prefix bi) acc))))))

;; Fold trie in decreasing numerical order.
(: trie-fold-right (procedure * trie -> *))
(define (trie-fold-right proc nil trie)
  (letrec
   ((cata
     (lambda (b t)
       (cond ((not t) b)
             ((leaf? t)
              (fold-right-bits (leaf-prefix t) proc b (leaf-bitmap t)))
             (else
              (cata (cata b (branch-right t)) (branch-left t)))))))
    (if (branch? trie)
        (let*-branch (((p m l r) trie))
          (if (fxnegative? m)
              (cata (cata nil l) r)
              (cata (cata nil r) l)))
        (cata nil trie))))

;; This might benefit from tuning.
(: fold-right-bits (fixnum procedure * fixnum -> *))
(define (fold-right-bits prefix proc nil bitmap)
  (let loop ((bm bitmap) (acc nil))
    (if (fxzero? bm)
        acc
        (let* ((mask (highest-bit-mask bm (lowest-bit-mask bm)))
               (bi (fxfirst-set-bit mask)))
          (loop (fxxor bm mask) (proc (fx+ prefix bi) acc))))))

(: bitmap-partition ((fixnum -> *) fixnum fixnum -> fixnum fixnum))
(define (bitmap-partition pred prefix bitmap)
  (let loop ((i 0) (in 0) (out 0))
    (cond ((fx=? i leaf-bitmap-size) (values in out))
          ((fxbit-set? i bitmap)
           (let ((bit (fxarithmetic-shift 1 i)))
             (if (pred (fx+ prefix i))
                 (loop (fx+ i 1) (fxior in bit) out)
                 (loop (fx+ i 1) in (fxior out bit)))))
          (else (loop (fx+ i 1) in out)))))

(: trie-partition ((fixnum -> *) trie -> trie trie))
(define (trie-partition pred trie)
  (letrec
   ((part
     (lambda (t)
       (cond ((not t) (values #f #f))
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (let-values (((in out) (bitmap-partition pred p bm)))
                  (values (leaf p in) (leaf p out)))))
             (else
              (let-values (((p) (branch-prefix t))
                           ((m) (branch-branching-bit t))
                           ((il ol) (part (branch-left t)))
                           ((ir or) (part (branch-right t))))
                (values (branch p m il ir) (branch p m ol or))))))))
    (part trie)))

(: bitmap-filter ((fixnum -> *) fixnum fixnum -> fixnum))
(define (bitmap-filter pred prefix bitmap)
  (let loop ((i 0) (res 0))
    (cond ((fx=? i leaf-bitmap-size) res)
          ((and (fxbit-set? i bitmap) (pred (fx+ prefix i)))
           (loop (fx+ i 1) (fxior res (fxarithmetic-shift 1 i))))
          (else (loop (fx+ i 1) res)))))

(: trie-filter ((fixnum -> *) trie -> trie))
(define (trie-filter pred trie)
  (cond ((not trie) #f)
        ((leaf? trie)
         (let*-leaf (((p bm) trie))
           (leaf p (bitmap-filter pred p bm))))
        (else
         (branch (branch-prefix trie)
                 (branch-branching-bit trie)
                 (trie-filter pred (branch-left trie))
                 (trie-filter pred (branch-right trie))))))

;;;; Update operations

(: trie-delete (trie fixnum -> trie))
(define (trie-delete trie key)
  (letrec*
   ((prefix (iprefix key))
    (update
     (lambda (t)
       (cond ((not t) #f)
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (if (fx=? p prefix)
                    (leaf p (bitmap-delete bm key))
                    t)))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? prefix p m)
                    (if (zero-bit? prefix m)
                        (branch p m (update l) r)
                        (branch p m l (update r)))
                    t)))))))
    (update trie)))

(: trie-delete-min (trie -> fixnum trie))
(define (trie-delete-min trie)
  (letrec
   ((update/min
     (lambda (t)
       (cond ((not t) (error "Empty set"))
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (values (+ p (fxfirst-set-bit bm))
                        (leaf p (bitmap-delete-min bm)))))
             (else
              (let*-branch (((p m l r) t))
                (let-values (((n l*) (update/min l)))
                  (values n (branch p m l* r)))))))))
    (if (branch? trie)
        (let*-branch (((p m l r) trie))
          (if (fxnegative? m)
              (let-values (((n r*) (update/min r)))
                (values n (branch p m l r*)))
              (let-values (((n l*) (update/min l)))
                (values n (branch p m l* r)))))
        (update/min trie))))

(: trie-delete-max (trie -> fixnum trie))
(define (trie-delete-max trie)
  (letrec
   ((update/max
     (lambda (t)
       (cond ((not t) (error "Empty set"))
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (values (+ p (highest-set-bit bm))
                        (leaf p (bitmap-delete-max bm)))))
             (else
              (let*-branch (((p m l r) t))
                (let-values (((n r*) (update/max r)))
                  (values n (branch p m l r*)))))))))
    (if (branch? trie)
        (let*-branch (((p m l r) trie))
          (if (fxnegative? m)
              (let-values (((n l*) (update/max l)))
                (values n (branch p m l* r)))
              (let-values (((n r*) (update/max r)))
                (values n (branch p m l r*)))))
        (update/max trie))))

(: trie-search (trie fixnum procedure procedure -> trie *))
(define (trie-search trie key failure success)
  (let* ((kp (iprefix key))
         (key-leaf (raw-leaf kp (ibitmap key))))
    (letrec
     ((search
       (lambda (t build)
         (cond ((not t)
                (failure (lambda (obj) (build key-leaf obj))
                         (lambda (obj) (build #f obj))))
               ((leaf? t)
                (leaf-search t key failure success build))
               (else
                (let*-branch (((p m l r) t))
                  (if (match-prefix? key p m)
                      (if (zero-bit? key m)
                          (search l (lambda (l* obj)
                                      (build (branch p m l* r) obj)))
                          (search r (lambda (r* obj)
                                      (build (branch p m l r*) obj))))
                      (failure (lambda (obj)
                                 (build (%trie-join kp 0 key-leaf p m t)
                                        obj))
                               (lambda (obj) (build t obj))))))))))
      (if (branch? trie)
          (let*-branch (((p m l r) trie))
            (if (fxnegative? m)
                (if (fxnegative? key)
                    (let-values (((r* obj) (search r values)))
                      (values (branch p m l r*) obj))
                    (let-values (((l* obj) (search l values)))
                      (values (branch p m l* r) obj)))
                (search trie values)))
          (search trie values)))))

(: leaf-search
   ((struct <leaf>) fixnum procedure procedure procedure -> noreturn))
(define (leaf-search lf key failure success build)
  (let ((kp (iprefix key)) (kb (ibitmap key)))
    (let*-leaf (((p bm) lf))
      (if (fx=? kp p)
          (if (fxzero? (fxand kb bm))
              (failure (lambda (obj)
                         (build (raw-leaf p (fxior kb bm)) obj))
                       (lambda (obj) (build lf obj)))
              (success key
                       (lambda (elt obj)
                         (unless (eqv? key elt)  ; FIXME?
                           (error 'iset-search "invalid new element" elt))
                         (build lf obj))
                       (lambda (obj)
                         (build (leaf p (bitmap-delete bm key)) obj))))
          (failure (lambda (obj)
                     (build (%trie-join kp 0 (raw-leaf kp kb) p 0 lf)
                            obj))
                   (lambda (obj) (build lf obj)))))))

;;;; Set-theoretical operations

(: %trie-join
   (fixnum fixnum trie fixnum fixnum trie -> (struct <branch>)))
(define (%trie-join prefix1 mask1 trie1 prefix2 mask2 trie2)
  (let ((m (branching-bit prefix1 mask1 prefix2 mask2)))
    (if (zero-bit? prefix1 m)
        (raw-branch (mask prefix1 m) m trie1 trie2)
        (raw-branch (mask prefix1 m) m trie2 trie1))))

(: branching-bit-higher? (fixnum fixnum -> boolean))
(define (branching-bit-higher? mask1 mask2)
  (if (negative? (fxxor mask1 mask2))  ; signs differ
      (negative? mask1)
      (fx>? mask1 mask2)))

;; Merge two tries.  The exact contents of the result depend on the
;; `insert-leaf' function, which is used to merge leaves into branches.
;; Taking the running time of `insert-leaf' to be constant, runs in
;; O(n+m) time.
(: %trie-merge ((trie (struct <leaf>) -> trie) trie trie -> trie))
(define (%trie-merge insert-leaf trie1 trie2)
  (letrec
    ((merge
      (lambda (s t)
        (cond ((not s) t)
              ((not t) s)
              ((leaf? s) (insert-leaf t s))
              ((leaf? t) (insert-leaf s t))
              (else (merge-branches s t)))))
     (merge-branches
      (lambda (s t)
        (let*-branch (((p m s1 s2) s)
                      ((q n t1 t2) t))
          (cond ((and (fx=? m n) (fx=? p q))
                 ;; the prefixes match, so merge the subtries
                 (branch p m (merge s1 t1) (merge s2 t2)))
                ((and (branching-bit-higher? m n) (match-prefix? q p m))
                 ;; p is a prefix of q, so merge t with a subtrie of s.
                 (if (zero-bit? q m)
                     (branch p m (merge s1 t) s2)
                     (branch p m s1 (merge s2 t))))
                ((and (branching-bit-higher? n m) (match-prefix? p q n))
                 ;; q is a prefix of p, so merge s with a subtrie of t.
                 (if (zero-bit? p n)
                     (branch q n (merge s t1) t2)
                     (branch q n t1 (merge s t2))))
                (else    ; the prefixes disagree
                 (%trie-join p m s q n t)))))))
    (merge trie1 trie2)))

(: trie-union (trie trie -> trie))
(define (trie-union trie1 trie2)
  (%trie-merge (lambda (s t) (insert-leaf/proc fxior s t))
               trie1
               trie2))

(: trie-xor (trie trie -> trie))
(define (trie-xor trie1 trie2)
  (%trie-merge (lambda (s t) (insert-leaf/proc fxxor s t))
               trie1
               trie2))

;; Insert the elements of `lf' into `trie', combining bitmaps with
;; the binary bitwise operation `fxcombine'.
(: insert-leaf/proc
   ((fixnum fixnum -> fixnum) trie (struct <leaf>) -> trie))
(define (insert-leaf/proc fxcombine trie lf)
  (let*-leaf (((p bm) lf))
    (letrec
     ((ins
       (lambda (t)
         (cond ((not t) lf)  ; a whole new leaf
               ((leaf? t)
                (let*-leaf (((q bm*) t))
                  (if (fx=? p q)
                      (leaf p (fxcombine bm bm*))
                      (%trie-join p 0 lf q 0 t))))
               (else         ; branch
                (let*-branch (((q m l r) t))
                  (if (match-prefix? p q m)
                      (if (zero-bit? p m)
                          (raw-branch q m (ins l) r)
                          (raw-branch q m l (ins r)))
                      (%trie-join p 0 lf q 0 t))))))))
      (ins trie))))

;; Construct a trie which forms the intersection of the two tries.
;; Runs in O(n+m) time.
(: trie-intersection (trie trie -> trie))
(define (trie-intersection trie1 trie2)
  (letrec
   ((intersect
     (lambda (s t)
       (cond ((or (not s) (not t)) #f)
             ((leaf? s) (intersect/leaf s t))
             ((leaf? t) (intersect/leaf t s))
             (else (intersect-branches s t)))))
    (intersect/leaf
     (lambda (l t)
       (let*-leaf (((p bm) l))
         (let lp ((t t))
           (cond ((not t) #f)
                 ((leaf? t)
                  (if (fx=? p (leaf-prefix t))
                      (leaf p (fxand bm (leaf-bitmap t)))
                      #f))          ; disjoint
                 (else              ; branch
                  (let*-branch (((q m l r) t))
                    (if (match-prefix? p q m)
                        (if (zero-bit? p m) (lp l) (lp r))
                        #f))))))))  ; disjoint
    (intersect-branches
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((branching-bit-higher? m n)
                (and (match-prefix? q p m)
                     (if (zero-bit? q m)
                         (intersect sl t)
                         (intersect sr t))))
               ((branching-bit-higher? n m)
                (and (match-prefix? p q n)
                     (if (zero-bit? p n)
                         (intersect s tl)
                         (intersect s tr))))
               ((fx=? p q)
                (branch p m (intersect sl tl) (intersect sr tr)))
               (else #f))))))
    (intersect trie1 trie2)))

;; Construct a trie containing the elements of trie1 not found in trie2.
;; Runs in O(n+m) time.
(: trie-difference (trie trie -> trie))
(define (trie-difference trie1 trie2)
  (letrec
   ((difference
     (lambda (s t)
       (cond ((not s) #f)
             ((not t) s)
             ((leaf? s) (diff/leaf s t))
             ((leaf? t)
              (%trie-delete-bitmap s (leaf-prefix t) (leaf-bitmap t)))
             (else (branch-difference s t)))))
    (diff/leaf
     (lambda (lf t)
       (let*-leaf (((p bm) lf))
         (let lp ((t t))
           (cond ((not t) lf)
                 ((leaf? t)
                  (let*-leaf (((q c) t))
                    (if (fx=? p q)
                        (leaf p (fxand bm (fxnot c)))
                        lf))) ; disjoint
                 (else        ; branch
                  (let*-branch (((q m l r) t))
                    (if (match-prefix? p q m)
                        (if (zero-bit? p m) (lp l) (lp r))
                        lf))))))))
    (branch-difference
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((and (fx=? m n) (fx=? p q))
                (branch p m (difference sl tl) (difference sr tr)))
               ((and (branching-bit-higher? m n) (match-prefix? q p m))
                (if (zero-bit? q m)
                    (branch p m (difference sl t) sr)
                    (branch p m sl (difference sr t))))
               ((and (branching-bit-higher? n m) (match-prefix? p q n))
                (if (zero-bit? p n)
                    (difference s tl)
                    (difference s tr)))
               (else s))))))
    (difference trie1 trie2)))

;; Delete all values described by `bitmap' from `trie'.
(: %trie-delete-bitmap (trie fixnum fixnum -> trie))
(define (%trie-delete-bitmap trie prefix bitmap)
  (cond ((not trie) #f)
        ((leaf? trie)
         (if (fx=? prefix (leaf-prefix trie))
             (leaf prefix (fxand (leaf-bitmap trie) (fxnot bitmap)))
             trie))  ; disjoint
        (else        ; branch
         (let*-branch (((p m l r) trie))
           (if (match-prefix? prefix p m)
               (if (zero-bit? prefix m)
                   (branch p m (%trie-delete-bitmap l prefix bitmap) r)
                   (branch p m l (%trie-delete-bitmap r prefix bitmap)))
               trie)))))

;;;; Copying

(: copy-trie (trie -> trie))
(define (copy-trie trie)
  (cond ((not trie) #f)
        ((leaf? trie) (raw-leaf (leaf-prefix trie) (leaf-bitmap trie)))
        (else
         (raw-branch (branch-prefix trie)
                     (branch-branching-bit trie)
                     (copy-trie (branch-left trie))
                     (copy-trie (branch-right trie))))))

;;;; Size

(: trie-size (trie -> integer))
(define (trie-size trie)
  (let accum ((siz 0) (t trie))
    (cond ((not t) siz)
          ((leaf? t) (+ siz (fxbit-count (leaf-bitmap t))))
          (else (accum (accum siz (branch-left t))
                       (branch-right t))))))

;;;; Comparisons

(: trie=? (trie trie -> boolean))
(define (trie=? trie1 trie2)
  (cond ((not (or trie1 trie2)) #t)
        ((and (leaf? trie1) (leaf? trie2))
         (and (fx=? (leaf-prefix trie1) (leaf-prefix trie2))
              (fx=? (leaf-bitmap trie1) (leaf-bitmap trie2))))
        ((and (branch? trie1) (branch? trie2))
         (let*-branch (((p m l1 r1) trie1) ((q n l2 r2) trie2))
           (and (fx=? m n) (fx=? p q) (trie=? l1 l2) (trie=? r1 r2))))
        (else #f)))

(: subset-compare-leaves ((struct <leaf>) (struct <leaf>) -> symbol))
(define (subset-compare-leaves l1 l2)
  (let*-leaf (((p b) l1) ((q c) l2))
    (if (fx=? p q)
        (if (fx=? b c)
            'equal
            (if (fxzero? (fxand b (fxnot c)))
                'less
                'greater))
        'greater)))  ; disjoint

;; Returns the symbol 'less' if trie1 is a proper subset of trie2,
;; 'equal' if they are the same, and 'greater' otherwise.  NB that
;; disjoint sets will compare as greater.
;;
;; FIXME: Simplify this.
(: trie-subset-compare (trie trie -> symbol))
(define (trie-subset-compare trie1 trie2)
  (letrec
   ((compare
     (lambda (s t)
       (cond ((eqv? s t) 'equal)
             ((not s) 'less)
             ((not t) 'greater)  ; disjoint
             ((and (leaf? s) (leaf? t)) (subset-compare-leaves s t))
             ((leaf? s)             ; leaf / branch
              (let*-leaf (((p _) s))
                (let*-branch (((q m l r) t))
                  (if (match-prefix? p q m)
                      (case (compare s (if (zero-bit? p m) l r))
                        ((greater) 'greater)
                        (else 'less))
                      'greater))))       ; disjoint
             ((leaf? t) 'greater)        ; branch / leaf
             (else (compare-branches s t)))))
    (compare-branches
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((branching-bit-higher? m n) 'greater)
               ((branching-bit-higher? n m)
                (if (match-prefix? p q n)
                    (let ((comp (if (zero-bit? p n)
                                    (compare s tl)
                                    (compare s tr))))
                      (if (eqv? comp 'greater) comp 'less))
                    'greater))
               ((fx=? p q)  ; same prefix, compare subtrees
                (let ((cl (compare sl tl)) (cr (compare sr tr)))
                  (cond ((or (eqv? cl 'greater) (eqv? cr 'greater))
                         'greater)
                        ((and (eqv? cl 'equal) (eqv? cr 'equal))
                         'equal)
                        (else 'less))))
               (else 'greater))))))  ; disjoint
    (compare trie1 trie2)))

(: trie-proper-subset? (trie trie -> boolean))
(define (trie-proper-subset? trie1 trie2)
  (eqv? (trie-subset-compare trie1 trie2) 'less))

(: trie-disjoint? (trie trie -> boolean))
(define (trie-disjoint? trie1 trie2)
  (letrec
   ((disjoint?
     (lambda (s t)
       (or (not s)
           (not t)
           (cond ((and (leaf? s) (leaf? t)) (disjoint/leaf? s t))
                 ((leaf? s) (disjoint/leaf? s t))
                 ((leaf? t) (disjoint/leaf? t s))
                 (else (branches-disjoint? s t))))))
    (disjoint/leaf?
     (lambda (lf t)
       (let*-leaf (((p bm) lf))
         (let lp ((t t))
           (if (leaf? t)
               (if (fx=? p (leaf-prefix t))
                   (fxzero? (fxand bm (leaf-bitmap t)))
                   #t)
               (let*-branch (((q n l r) t))
                 (if (match-prefix? p q n)
                     (if (zero-bit? p n) (lp l) (lp r))
                     #t)))))))
    (branches-disjoint?
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((and (fx=? m n) (fx=? p q))
                (and (disjoint? sl tl) (disjoint? sr tr)))
               ((and (branching-bit-higher? m n) (match-prefix? q p m))
                (if (zero-bit? q m)
                    (disjoint? sl t)
                    (disjoint? sr t)))
               ((and (branching-bit-higher? n m) (match-prefix? p q n))
                (if (zero-bit? p n)
                    (disjoint? s tl)
                    (disjoint? s tr)))
               (else #t))))))      ; the prefixes disagree
    (disjoint? trie1 trie2)))

;;;; Subtrie operations

;; Return a trie containing all the elements of `trie' which are
;; less than k, if `inclusive' is false, or less than or equal to
;; k if `inclusive' is true.
;; Runs in O(min(n, W)) time.
(: subtrie< (trie fixnum boolean -> trie))
(define (subtrie< trie k inclusive)
  (letrec
    ((split
      (lambda (t)
        (cond ((not t) #f)
              ((leaf? t)
               (let*-leaf (((p bm) t))
                 (leaf p (bitmap-split< k inclusive p bm))))
              (else
               (let*-branch (((p m l r) t))
                 (if (match-prefix? k p m)
                     (if (zero-bit? k m)
                         (split l)
                         (trie-union l (split r)))
                     (and (fx<? p k) t))))))))
    (if (and (branch? trie) (fxnegative? (branch-branching-bit trie)))
        (if (fxnegative? k)
            (split (branch-right trie))
            (trie-union (split (branch-left trie)) (branch-right trie)))
        (split trie))))

;; Return a bitmap containing all elements in `bitmap' that are
;; less than/less than or equal to k.
(: bitmap-split< (fixnum boolean fixnum fixnum -> fixnum))
(define (bitmap-split< k inclusive prefix bitmap)
  (let ((kp (iprefix k)) (kb (ibitmap k)))
    (cond ((fx>? kp prefix) bitmap)
          ((fx=? kp prefix)
           (fxand bitmap
                  (fx- (if inclusive
                           (fxarithmetic-shift kb 1)
                           kb)
                       1)))
          (else 0))))

;; Return a trie containing all the elements of `trie' which are
;; greater than k, if `inclusive' is false, or greater than or equal
;; to k if `inclusive' is true.
;; Runs in O(min(n, W)) time.
(: subtrie> (trie fixnum boolean -> trie))
(define (subtrie> trie k inclusive)
  (letrec
   ((split
     (lambda (t)
       (cond ((not t) #f)
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (leaf p (bitmap-split> k inclusive p bm))))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? k p m)
                    (if (zero-bit? k m)
                        (trie-union (split l) r)
                        (split r))
                    (and (fx>? p k) t))))))))
    (if (and (branch? trie) (fxnegative? (branch-branching-bit trie)))
        (if (fxnegative? k)
            (trie-union (split (branch-right trie)) (branch-left trie))
            (split (branch-left trie)))
        (split trie))))

;; Return a bitmap containing all elements in `bitmap' that are
;; greater than/greater than or equal to `k'.
(: bitmap-split> (fixnum boolean fixnum fixnum -> fixnum))
(define (bitmap-split> k inclusive prefix bitmap)
  (let ((kp (iprefix k)) (kb (ibitmap k)))
    (cond ((fx<? kp prefix) bitmap)
          ((fx=? kp prefix)
           (fxand bitmap
                  (fxneg (if inclusive
                             kb
                             (fxarithmetic-shift kb 1)))))
          (else 0))))

;; Return a trie containing all the elements of `trie' which are
;; greater than/greater than or equal to a and less than/less than
;; or equal to b, depending on the truth values of
;; low-/high-inclusive.
(: subtrie-interval (trie fixnum fixnum boolean boolean -> trie))
(define (subtrie-interval trie a b low-inclusive high-inclusive)
  (letrec
   ((interval
     (lambda (t)
       (cond ((not t) #f)
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (leaf p
                      (bitmap-interval p bm a b low-inclusive high-inclusive))))
             (else (branch-interval t)))))
    (branch-interval
     (lambda (t)
       (let*-branch (((p m l r) t))
         (if (match-prefix? a p m)
             (if (zero-bit? a m)
                 (if (match-prefix? b p m)
                     (if (zero-bit? b m)
                         (interval l)  ; all x < b is in l
                         (trie-union (subtrie> l a low-inclusive)
                                     (subtrie< r b high-inclusive)))
                     ;; everything or nothing is less than b
                     (and (fx<? b p)
                          (trie-union (subtrie> l a low-inclusive) r)))
                 (interval r)) ; all x > b is in r
             ;; everything or nothing is greater than a
             (and (fx>? p a) (subtrie< t b high-inclusive)))))))
    (if (and (branch? trie) (fxnegative? (branch-branching-bit trie)))
        (cond ((and (fxnegative? a) (fxnegative? b))
               (interval (branch-right trie)))
              ((and (fxpositive? a) (fxpositive? b))
               (interval (branch-left trie)))
              ;; (a, 0) U (0, b)
              (else (trie-union
                     (subtrie> (branch-right trie) a low-inclusive)
                     (subtrie< (branch-left trie) b high-inclusive))))
        (interval trie))))

;; Return a bitmap containing the elements of bitmap that are within
;; the interval defined by a, b.
(: bitmap-interval
   (fixnum fixnum fixnum fixnum boolean boolean -> fixnum))
(define (bitmap-interval prefix bitmap low high low-inclusive high-inclusive)
  (let ((lp (iprefix low))
        (lb (ibitmap low))
        (hp (iprefix high))
        (hb (ibitmap high)))
    (let ((low-mask (fxneg (if low-inclusive    ; mask everything above `low'
                               lb
                               (fxarithmetic-shift lb 1))))
          (high-mask (fx- (if high-inclusive    ; mask everything below `high'
                              (fxarithmetic-shift hb 1)
                              hb)
                          1)))
      (cond ((fx<? prefix hp)
             (cond ((fx<? prefix lp) 0)
                   ((fx>? prefix lp) bitmap)
                   (else (fxand low-mask bitmap))))
            ((fx>? prefix hp) 0)
            (else (fxand (fxand low-mask high-mask) bitmap))))))

)
