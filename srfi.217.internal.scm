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
                  trie-disjoint? subtrie< subtrie> subtrie-interval)
  (import scheme
          (chicken base)
          (chicken condition)
          (chicken type)
          (srfi 143)
          typed-records)
  (include "srfi.217.internal.source.scm"))
