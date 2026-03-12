(define-library (srfi 217 internal)
  (import (scheme base) (chicken base)
          (chicken condition) (chicken type)
          (srfi 143) typed-records)
  (export trie-contains? trie-min trie-max trie-insert trie-fold trie-fold-right
          trie-partition trie-filter trie-delete trie-delete-min trie-delete-max
          trie-search trie-union trie-xor trie-intersection trie-difference
          copy-trie trie-size trie=? trie-subset-compare trie-proper-subset?
          trie-disjoint? subtrie< subtrie> subtrie-interval)
  (include "srfi.217.internal.source.scm"))