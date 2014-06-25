;; from https://github.com/philc/emacs-config/blob/master/.emacs.d/elisp/emacs-utils.el

;;
;; Lisp utility functions to serve as building blocks for working with text in Emacs.
;;
(provide 'emacs-utils)

(defun partition (l n)
  "Return a list of L's consecutive sublists of length N."
  (assert (zerop (mod (length l) n)))
  (loop for l on l by #'(lambda (l) (nthcdr n l)) collect (subseq l 0 n)))

(defun util/define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (first pair) (second pair))))

