(import (chicken base)
        (chicken condition)
        (chicken fixnum)
        (rename (chicken random) (pseudo-random-integer random-int))
        (chicken syntax)
        (srfi 217)
        (test)
        test-generative
        (srfi 1)
        )

;;; Utility

(define (init xs)
  (if (null? (cdr xs))
      '()
      (cons (car xs) (init (cdr xs)))))

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define mixed-seq (iota 20 -10 3))
(define sparse-seq (iota 20 -10000 1003))

(define pos-set (list->iset pos-seq))
(define pos-set+ (iset-adjoin pos-set 9))
(define neg-set (list->iset neg-seq))
(define mixed-set (list->iset mixed-seq))
(define dense-set (make-range-iset 0 49))
(define sparse-set (list->iset sparse-seq))

(define all-test-sets
  (list pos-set neg-set mixed-set dense-set sparse-set))

(define test-iset-size-bound 128)

;; Return an iset of random size containing random fixnums.
;; FIXME: Find a saner way to generate random integers in the
;; full fixnum range.
(define (random-iset)
  (let ((size (random-int test-iset-size-bound)))
    (iset-unfold (cut >= <> size)
                 (lambda (_i)
                   (let ((neg (random-int 2))
                         (k (random-int most-positive-fixnum)))
                     (if (zero? neg)
                         k
                         (- k))))
                 add1
                 0)))

;; Return a list of random size containing random fixnums.
;; FIXME: Find a saner way to generate random integers in the
;; full fixnum range.
(define (random-list len-bound)
  (list-tabulate
   (random-int len-bound)
   (lambda (_i)
     (let ((neg (random-int 2))
           (k (random-int most-positive-fixnum)))
       (if (zero? neg) k (- k))))))

;; Use test-generative to bind a list of names to random isets
;; over a bunch of test expressions.
(define-syntax test-with-random-isets
  (ir-macro-transformer
   (lambda (exp _inj _same?)
     `(test-generative ,(map (lambda (v) (list v 'random-iset))
                             (cadr exp))
        ,@(cddr exp)))))

;; Evaluates to true if the expression raises a type condition.
(define-syntax raises-type-condition
  (syntax-rules ()
    ((raises-type-condition e)
     (handle-exceptions con
                        ((condition-predicate 'type) con)
       e))))

;;;; Tests

;; Most other test groups use iset=?, so test this first.
(test-group "iset=?"
  (test #t (iset=? (iset) (iset)))
  (test #f (iset=? (iset 1) (iset)))
  (test #f (iset=? (iset) (iset 1)))
  (test #t (iset=? (iset 1 2 3 4) (iset 1 2 3 4)))
  (test #t (iset=? (iset 1 2 3 4) (iset 2 1 4 3) (iset 3 2 1 4)))
  (test #f (iset=? (iset 1 2 3 4) (iset 2 3 4)))
  (test #f (iset=? pos-set neg-set))

  (test-with-random-isets (s1 s2)
    (test #t (iset=? s1 s1))
    (test (iset-empty? s1) (iset=? s1 (iset)))
    (test-equal iset=? s1 (list->iset (iset->list s1)))
    (test (equal? (iset->list s1) (iset->list s2))
          (iset=? s1 s2))
    )
  )

(test-group "Copying and conversion"
  (test-group "iset-copy"
    (test-with-random-isets (s)
      (test #t (not (eqv? (iset-copy s) s)))
      (test #t (iset=? s (iset-copy s)))
      )
    (test #t (raises-type-condition (iset-copy #t)))
    )

  (test-group "iset->list"
    (test '() (iset->list (iset)))

    (test-with-random-isets (s)
      (test (iset-size s) (length (iset->list s)))
      (test #t (every (lambda (k) (iset-contains? s k)) (iset->list s)))
      (test #t
            (let ((ks (iset->list s)))
              (iset-every? (cut memv <> ks) s)))
      )
    (test #t (raises-type-condition (iset->list #t)))
    )

  (test-group "list->iset"
    (test #t (iset-empty? (list->iset '())))

    (test-generative ((ks (lambda () (random-list 64))))
      (test (length ks) (iset-size (list->iset ks)))
      (test #t (iset-every? (cut memv <> ks) (list->iset ks)))
      (test #t
            (let ((s (list->iset ks)))
              (and (every (cut iset-contains? s <>) ks) #t)))
      )
    (test #t (raises-type-condition (list->iset 0.3)))
    )
  )

(test-group "Constructors"
  (test-group "iset"
    (test #t (iset-empty? (iset)))

    (test-equal iset=? (list->iset '(1 3 5 7)) (iset 1 3 5 7))
    (test #t (raises-type-condition (iset 9 3 0.5)))
    )

  (test-group "iset-unfold"
    (test-equal iset=?
                (list->iset (iota 10 0 4))
                (iset-unfold (lambda (i) (> i 36))
                             values
                             (lambda (i) (+ i 4))
                             0))
    (test #t (raises-type-condition (iset-unfold #t values add1 0)))
    (test #t (raises-type-condition (iset-unfold zero? 6 add1 0)))
    (test #t (raises-type-condition (iset-unfold zero? values 'z 0)))
    )

  ;; TODO: More tests for this.
  (test-group "make-range-iset"
    (test #t (iset-empty? (make-range-iset 0 0)))
    (test-equal iset=?
                (list->iset (iota 20 -10))
                (make-range-iset -10 10))
    (test-equal iset=?
                (list->iset (iota 10 -10 2))
                (make-range-iset -10 10 2))
    (test-equal iset=?
                (iset 1000)
                (make-range-iset 1000 1500 500))
    (test-equal iset=?
                (iset 1000)
                (make-range-iset 1000 500 -500))
    (test-error (make-range-iset 50 500 0))
    (test #t (raises-type-condition (make-range-iset 0.3 0 1)))
    (test #t (raises-type-condition (make-range-iset 0 'z 3)))
    (test #t (raises-type-condition (make-range-iset 0 3 #f)))
    )
  )

(test-group "Predicates"
  (test-group "iset-contains?"
    (test #f (iset-contains? (iset) 1))
    (test #t (every (lambda (n) (iset-contains? pos-set n)) pos-seq))
    (test #f (any (lambda (n) (iset-contains? pos-set n))
                  (iota 20 -100 3)))
    (test #t (every (lambda (n) (iset-contains? neg-set n)) neg-seq))
    (test #f (any (lambda (n) (iset-contains? neg-set n))
                  (iota 20 100 3)))
    (test #t (every (lambda (n) (iset-contains? mixed-set n)) mixed-seq))
    (test #f (any (lambda (n) (iset-contains? mixed-set n))
                  (iota 20 1000 3)))
    (test #t (every (lambda (n) (iset-contains? sparse-set n))
                    sparse-seq))
    (test #f (any (lambda (n) (iset-contains? sparse-set n))
                  (iota 20 1500 3)))
    (test #t (raises-type-condition (iset-contains? 10 0)))
    (test #t (raises-type-condition (iset-contains? (iset) "foo")))
    )

  (test-group "iset-empty?"
    (test #t (iset-empty? (iset)))
    (test #f (iset-empty? pos-set))
    (test #f (iset-empty? neg-set))
    (test #f (iset-empty? mixed-set))
    (test #f (iset-empty? sparse-set))
    (test-generative ((k (lambda () (random-int 64))))
      (when (positive? k)
        (test #t (iset-empty? (make-range-iset k k)))
        (test #f (iset-empty? (make-range-iset 0 k)))
        (test #f (iset-empty? (make-range-iset (- k) k)))
        ))
    (test #t (raises-type-condition (iset-empty? 'z)))
    )

  (test-group "iset-disjoint?"
    (test #t (iset-disjoint? (iset) (iset)))
    (test #t (iset-disjoint? pos-set neg-set))
    (test #t (iset-disjoint? (iset) pos-set))
    (test #f (iset-disjoint? dense-set sparse-set))
    (test #f (iset-disjoint? (make-range-iset 20 30)
                             (make-range-iset 29 39)))
    (test-with-random-isets (s1 s2)
      (let ((lis1 (iset->list s1)) (lis2 (iset->list s2)))
        (test (null? (lset-intersection = lis1 lis2))
              (iset-disjoint? s1 s2))
        ))
    (test #t (raises-type-condition (iset-disjoint? 10 (iset))))
    (test #t (raises-type-condition (iset-disjoint? (iset) 'z)))
    )
  )

(test-group "Accessors"
  (test-group "iset-member"
    (test 'z (iset-member (iset) 0 'z))
    (test 103 (iset-member pos-set 103 #f))
    (test 'z (iset-member pos-set 104 'z))
    (test -103 (iset-member neg-set -103 #f))
    (test 'z (iset-member neg-set 10 'z))
    (test 1 (iset-member mixed-set 0 #f))
    (test 'z (iset-member mixed-set -200 'z))
    (test 30 (iset-member sparse-set 30 #f))
    (test 'z (iset-member sparse-set 39 'z))
    )

  (test-group "iset-min"
    (test #f (iset-min (iset)))
    (test 1 (iset-min (iset 1 2 3)))
    (test (car pos-seq) (iset-min pos-set))
    (test (car neg-seq) (iset-min neg-set))
    (test (car mixed-seq) (iset-min mixed-set))
    (test (car sparse-seq) (iset-min sparse-set))
    )

  (test-group "iset-max"
    (test #f (iset-max (iset)))
    (test 3 (iset-max (iset 1 2 3)))
    (test (last pos-seq) (iset-max pos-set))
    (test (last neg-seq) (iset-max neg-set))
    (test (last mixed-seq) (iset-max mixed-set))
    (test (last sparse-seq) (iset-max sparse-set))
    ))

(test-group "Updaters"
  (test-group "iset-adjoin"
    (test '(1) (iset->list (iset-adjoin (iset) 1)))
    (test #t (iset=? (iset 1 -2) (iset-adjoin (iset) 1 -2)))
    (test #t (iset=? (iset 1 -2 3) (iset-adjoin (iset) 1 -2 3)))
    (test #t (iset-contains? (iset-adjoin neg-set 10) 10))
    (test #t (iset-contains? (iset-adjoin dense-set 100) 100))
    (test #t (iset-contains? (iset-adjoin sparse-set 100) 100))

    (test-with-random-isets (s)
      (test (iset-contains? s 10) (iset=? s (iset-adjoin s 10)))
      (test #t (iset-contains? (iset-adjoin s 10) 10))
      (test #t
            (let ((s* (iset-adjoin s -64 128 -256)))
              (and (iset-contains? s* -64)
                   (iset-contains? s* 128)
                   (iset-contains? s* -256))))
      )
    (test #t (raises-type-condition (iset-adjoin #t 10)))
    (test #t (raises-type-condition (iset-adjoin (iset) 10.1)))
    (test #t (raises-type-condition (iset-adjoin (iset) 1 2 10.1)))
    )

  (test-group "iset-delete"
    (test '() (iset->list (iset-delete (iset 1) 1)))
    (test #f (iset-contains? (iset-delete neg-set 10) 10))
    (test #f (iset-contains? (iset-delete dense-set 1033) 1033))
    (test #f (iset-contains? (iset-delete sparse-set 30) 30))

    (test-with-random-isets (s)
      (test (not (iset-contains? s 10)) (iset=? s (iset-delete s 10)))
      (test #f (iset-contains? (iset-delete s 10) 10))
      (test #f
            (let ((s* (iset-delete s -64 128 -256)))
              (or (iset-contains? s* -64)
                  (iset-contains? s* 128)
                  (iset-contains? s* -256))))
      ;; delete after adjoin
      (test #t (iset=? s (iset-delete (iset-adjoin s 10) 10)))
      )
    (test #t (raises-type-condition (iset-delete #t 10)))
    (test #t (raises-type-condition (iset-delete (iset) 10.1)))
    (test #t (raises-type-condition (iset-delete (iset) 1 2 10.1)))
    )

  (test-group "iset-delete-all"
    (test #t (iset-empty? (iset-delete-all (iset) '())))
    (test #t (iset-empty? (iset-delete-all (iset) '(1 2 3))))
    (test-equal iset=?
                (iset 100 103 106)
                (iset-delete-all pos-set (iota 17 109 3)))

    (test-with-random-isets (s)
      (test-equal iset=? s (iset-delete-all s '()))
      (test #t (iset-empty? (iset-delete-all s (iset->list s))))
      (test #f (iset-contains? (iset-delete-all s '(10)) 10))
      (test #f
            (let ((s* (iset-delete-all s '(-64 128 -256))))
              (or (iset-contains? s* -64)
                  (iset-contains? s* 128)
                  (iset-contains? s* -256))))
      ;; delete-all after adjoin
      (test-equal iset=? s (iset-delete-all (iset-adjoin 1 2) '(1 2)))
      )
    (test #t (raises-type-condition (iset-delete-all #t '())))
    (test #t (raises-type-condition (iset-delete-all (iset) 10.1)))
    (test #t (raises-type-condition (iset-delete-all (iset) '(1 2 z))))
    )

  ;; There are a ton of cases to check here.
  (test-group "iset-search"
    ;; Ignore
    (test-assert
     (let-values (((s obj)
                   (iset-search (iset)
                                1
                                (lambda (_ins ignore) (ignore 'z))
                                (lambda (k _up remove) (remove 'y)))))
       (and (iset-empty? s) (eqv? obj 'z))))

    ;; Insert
    (test-assert
     (let-values (((s obj)
                   (iset-search (iset)
                                1
                                (lambda (insert _ig) (insert 'z))
                                (lambda (k update _rem) (update 1 'y)))))
       (and (iset=? s (iset 1)) (eqv? obj 'z))))

    ;; Remove
    (test-assert
     (let-values (((s obj)
                   (iset-search (iset 1)
                                1
                                (lambda (_ins ignore) (ignore 'y))
                                (lambda (k _up remove) (remove 'z)))))
       (and (iset-empty? s) (eqv? obj 'z))))

    ;; Update
    (test-assert
     (let-values (((s obj)
                   (iset-search (iset 1)
                                1
                                (lambda (insert _ig) (insert 'y))
                                (lambda (k update _rem) (update 2 k)))))
       (and (iset=? s (iset 2)) (eqv? obj 1))))

    (test-with-random-isets (s)
      ;; Ignore or update with same element -> same set
      (test-assert
       (let-values (((s* _obj)
                     (iset-search s
                                  64
                                  (lambda (_ins ignore) (ignore #t))
                                  (lambda (k update _rem)
                                    (update k #t)))))
         (iset=? s s*)))

      ;; Insert or update with same element -> possibly extended set
      (test-assert
       (let-values (((s* _obj)
                     (iset-search s
                                  64
                                  (lambda (insert _ig) (insert #t))
                                  (lambda (k update _rem)
                                    (update k #t)))))
         (iset=? (iset-adjoin s 64) s*)))

      ;; Ignore or update with new element.  'found' indicates result.
      (test-assert
       (let-values (((s* found)
                     (iset-search s
                                  64
                                  (lambda (_ins ignore) (ignore #f))
                                  (lambda (_k update _rem)
                                    (update 128 #t)))))
         (if found
             (iset=? s* (iset-adjoin (iset-delete s 64) 128))
             (iset=? s* s))))

      ;; Insert or update with new element.  'found' indicates result.
      (test-assert
       (let-values (((s* found)
                     (iset-search s
                                  64
                                  (lambda (insert _ig) (insert #f))
                                  (lambda (_k update _rem)
                                    (update 128 #t)))))
         (if found
             (iset=? s* (iset-adjoin (iset-delete s 64) 128))
             (iset=? s* (iset-adjoin s 64)))))

      ;; Ignore or remove.  'found' indicates result.
      (test-assert
       (let-values (((s* found)
                     (iset-search s
                                  64
                                  (lambda (_ins ignore) (ignore #f))
                                  (lambda (_k _up remove)
                                    (remove #t)))))
         (if found
             (iset=? s* (iset-delete s 64))
             (iset=? s* s))))

      ;; Insert or remove.  'found' indicates result.
      (test-assert
       (let-values (((s* found)
                     (iset-search s
                                  64
                                  (lambda (insert _ig) (insert #f))
                                  (lambda (_k _up remove)
                                    (remove #t)))))
         (if found
             (iset=? s* (iset-delete s 64))
             (iset=? s* (iset-adjoin s 64)))))
      ))

  ;;; iset-delete-min / -max

  (test-values (values #t #t)
               (let-values (((n mixed-set*) (iset-delete-min mixed-set)))
                 (values (= n (car mixed-seq))
                         (iset=? mixed-set* (list->iset (cdr mixed-seq))))))
  (test-values (values #t #t)
               (let-values (((n sparse-set*) (iset-delete-min sparse-set)))
                 (values (= n (car sparse-seq))
                         (iset=? sparse-set* (list->iset (cdr sparse-seq))))))

  (test-values (values #t #t)
               (let-values (((n mixed-set*) (iset-delete-max mixed-set)))
                 (values (= n (last mixed-seq))
                         (iset=? mixed-set* (list->iset (init mixed-seq))))))
  (test-values (values #t #t)
               (let-values (((n sparse-set*) (iset-delete-max sparse-set)))
                 (values (= n (last sparse-seq))
                         (iset=? sparse-set* (list->iset (init sparse-seq))))))
  )

(test-group "Whole set operations"
  (test 0 (iset-size (iset)))
  (test (length pos-seq) (iset-size pos-set))
  (test (length mixed-seq) (iset-size mixed-set))
  (test (length sparse-seq) (iset-size sparse-set))

  (test 8 (iset-find even? (iset 1 3 5 7 8 9 10) (lambda () #f)))
  (test 'z (iset-find negative? pos-set (lambda () 'z)))

  (test #f (iset-any? even? (iset)))
  (test-assert (iset-any? even? pos-set))
  (test-not (iset-any? negative? pos-set))
  (test-assert (iset-any? (lambda (n) (> n 100)) sparse-set))
  (test-not (iset-any? (lambda (n) (> n 100)) dense-set))

  (test #t (iset-every? even? (iset)))
  (test-not (iset-every? even? pos-set))
  (test-assert (iset-every? negative? neg-set))
  (test-not (iset-every? (lambda (n) (> n 100)) sparse-set))
  (test-assert (iset-every? (lambda (n) (< n 100)) dense-set))

  (test 0 (iset-count even? (iset)))
  (test (count even? pos-seq) (iset-count even? pos-set))
  (test (count even? neg-seq) (iset-count even? neg-set))
  (test (count even? sparse-seq) (iset-count even? sparse-set))
  )

(test-group "Iterators"
  ;;; folds

  (test (fold + 0 pos-seq) (iset-fold + 0 pos-set))
  (test (fold + 0 sparse-seq) (iset-fold + 0 sparse-set))
  (test (iset-size neg-set) (iset-fold (lambda (_ c) (+ c 1)) 0 neg-set))
  (test (reverse pos-seq) (iset-fold cons '() pos-set))
  (test (reverse mixed-seq) (iset-fold cons '() mixed-set))

  (test (fold + 0 pos-seq) (iset-fold-right + 0 pos-set))
  (test (fold + 0 sparse-seq) (iset-fold-right + 0 sparse-set))
  (test (iset-size neg-set) (iset-fold-right (lambda (_ c) (+ c 1)) 0 neg-set))
  (test pos-seq (iset-fold-right cons '() pos-set))
  (test mixed-seq (iset-fold-right cons '() mixed-set))

  ;;; iset-map

  (test-assert (iset-empty? (iset-map values (iset))))
  (test-equal iset=? pos-set (iset-map values pos-set))
  (test-equal iset=?
              (list->iset (map (lambda (n) (* n 2)) mixed-seq))
              (iset-map (lambda (n) (* n 2)) mixed-set))
  (test-equal iset=? (iset 1) (iset-map (constantly 1) pos-set))

  ;;; iset-for-each

  (test (iset-size mixed-set)
        (let ((n 0))
          (iset-for-each (lambda (_) (set! n (+ n 1))) mixed-set)
          n))
  (test (fold + 0 sparse-seq)
        (let ((sum 0))
          (iset-for-each (lambda (n) (set! sum (+ sum n))) sparse-set)
          sum))
  (test (reverse mixed-seq)
        (let ((xs '()))
          (iset-for-each (lambda (n) (set! xs (cons n xs))) mixed-set)
          xs))

  ;;; filter, remove, & partition

  (test-assert (iset-empty? (iset-filter (constantly #f) pos-set)))
  (test-equal iset=?
              pos-set
              (iset-filter (constantly #t) pos-set))
  (test-equal iset=?
              (list->iset (filter even? mixed-seq))
              (iset-filter even? mixed-set))
  (test-assert (iset-empty? (iset-remove (constantly #t) pos-set)))
  (test-equal iset=?
              pos-set
              (iset-remove (constantly #f) pos-set))
  (test-equal iset=?
              (list->iset (remove even? mixed-seq))
              (iset-remove even? mixed-set))
  (test-assert
   (let-values (((in out) (iset-partition (constantly #f) pos-set)))
     (and (iset-empty? in) (iset=? pos-set out))))
  (test-assert
   (let-values (((in out) (iset-partition (constantly #t) pos-set)))
     (and (iset=? pos-set in) (iset-empty? out))))
  (test-assert
   (let-values (((in out) (iset-partition even? mixed-set))
                ((lin lout) (partition even? mixed-seq)))
     (and (iset=? in (list->iset lin))
          (iset=? out (list->iset lout)))))
  )

(test-group "Comparison"
  (test-assert (iset<? (iset) pos-set))
  (test-assert (iset<? pos-set pos-set+))
  (test-not    (iset<? pos-set pos-set))
  (test-not    (iset<? pos-set+ pos-set))
  (test-assert (iset<? (iset) pos-set pos-set+))
  (test-not    (iset<? (iset) pos-set pos-set))
  (test-assert (iset<=? (iset) pos-set))
  (test-assert (iset<=? pos-set pos-set+))
  (test-assert (iset<=? pos-set pos-set))
  (test-not    (iset<=? pos-set+ pos-set))
  (test-assert (iset<=? (iset) pos-set pos-set+))
  (test-assert (iset<=? (iset) pos-set pos-set))
  (test-not    (iset>? (iset) pos-set))
  (test-not    (iset>? pos-set pos-set+))
  (test-not    (iset>? pos-set pos-set))
  (test-assert (iset>? pos-set+ pos-set))
  (test-assert (iset>? pos-set+ pos-set (iset)))
  (test-not    (iset>? pos-set+ pos-set pos-set))
  (test-not    (iset>=? (iset) pos-set))
  (test-not    (iset>=? pos-set pos-set+))
  (test-assert (iset>=? pos-set pos-set))
  (test-assert (iset>=? pos-set+ pos-set))
  (test-assert (iset>=? pos-set+ pos-set (iset)))
  (test-assert (iset>=? pos-set+ pos-set pos-set))
  )

(test-group "Set theory"
  (test-equal iset=? mixed-set (iset-union! (iset) mixed-set))
  (test-equal iset=?
              (list->iset (append (iota 20 100 3) (iota 20 -100 3)))
              (iset-union pos-set neg-set))
  (test-equal iset=? pos-set (iset-union pos-set pos-set))
  (test-equal iset=?
              (list->iset (iota 30 100 3))
              (iset-union pos-set (list->iset (iota 20 130 3))))
  (test-equal iset=?
              (list->iset (iota 10))
              (iset-union (iset 0 1 2) (iset 3 5 8) (iset 4 6 7 9)))

  ;; iset-intersection
  (test-assert (iset-empty? (iset-intersection (iset) mixed-set)))
  (test-equal iset=? neg-set (iset-intersection neg-set neg-set))
  (test-equal iset=? (iset -97) (iset-intersection (iset -97) neg-set))
  (test-equal iset=? (iset) (iset-intersection pos-set neg-set))
  (test-equal iset=?
              (list->iset (drop-while negative? mixed-seq))
              (iset-intersection mixed-set dense-set))
  (test-equal iset=?
              (iset 0 1)
              (iset-intersection (iset 0 1 2) (iset 0 1 3 4) (iset 10 7 0 1)))

  ;; iset-difference
  (test-assert (iset-empty? (iset-difference neg-set neg-set)))
  (test-equal iset=? pos-set (iset-difference pos-set neg-set))
  (test-equal iset=? pos-set (iset-difference pos-set neg-set))
  (test-equal iset=?
              (iset 100)
              (iset-difference pos-set (list->iset (cdr pos-seq))))
  (test-equal iset=?
              (list->iset (take-while negative? mixed-seq))
              (iset-difference mixed-set dense-set))
  (test-equal iset=?
              (iset 0 1)
              (iset-intersection (iset 0 1 2 5) (iset 0 1 3 4) (iset 10 7 0 1)))

  ;; iset-xor
  (test-equal iset=? mixed-set (iset-xor (iset) mixed-set))
  (test-equal iset=?
              (list->iset (append (iota 20 100 3) (iota 20 -100 3)))
              (iset-xor pos-set neg-set))
  (test-equal iset=? (iset) (iset-xor pos-set pos-set))
  (test-equal iset=?
              (list->iset '(100 103 106))
              (iset-xor pos-set (list->iset (iota 17 109 3))))
  )

(test-group "Subsets"
  (test-assert (iset-empty? (iset-open-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 103 106)
              (iset-open-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-open-interval neg-set 0 50)))

  (test-assert (iset-empty? (iset-closed-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 100 103 106 109)
              (iset-closed-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-closed-interval neg-set 0 50)))

  (test-assert (iset-empty? (iset-open-closed-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 103 106 109)
              (iset-open-closed-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-open-closed-interval neg-set 0 50)))

  (test-assert (iset-empty? (iset-closed-open-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 100 103 106)
              (iset-closed-open-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-closed-open-interval neg-set 0 50)))

  ;;; isubset*

  (test-assert (iset-empty? (isubset= pos-set 90)))
  (test-equal iset=? (iset 100) (isubset= pos-set 100))

  (test-assert (iset-empty? (isubset< (iset) 10)))
  (test-equal iset=?
              (iset 100 103 106)
              (isubset< pos-set 109))
  (test-equal iset=?
              (iset -10 -7)
              (isubset< mixed-set -4))
  (test-assert (iset-empty? (isubset< mixed-set -15)))

  (test-assert (iset-empty? (isubset<= (iset) 10)))
  (test-equal iset=?
              (iset 100 103 106 109)
              (isubset<= pos-set 109))
  (test-equal iset=?
              (iset -10 -7 -4)
              (isubset<= mixed-set -4))
  (test-assert (iset-empty? (isubset<= mixed-set -15)))

  (test-assert (iset-empty? (isubset> (iset) 10)))
  (test-equal iset=?
              (iset 151 154 157)
              (isubset> pos-set 148))
  (test-equal iset=?
              (iset 41 44 47)
              (isubset> mixed-set 38))
  (test-assert (iset-empty? (isubset> mixed-set 50)))

  (test-assert (iset-empty? (isubset>= (iset) 10)))
  (test-equal iset=?
              (iset 148 151 154 157)
              (isubset>= pos-set 148))
  (test-equal iset=?
              (iset 38 41 44 47)
              (isubset>= mixed-set 38))
  (test-assert (iset-empty? (isubset>= mixed-set 50)))
  )

(test-exit)
