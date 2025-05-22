;;; virtual-ring.el --- Fixed size rings with virtual rotation -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/virtual-ring
;; Version: 0.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT a part of Gnu Emacs.

;; This work is "part of the world."  You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the
;; creators.  Attribution would be appreciated and is a valuable
;; contribution in itself, but it is not strictly necessary nor
;; required.  If you'd like to learn more about this way of doing
;; things and how it could lead to a peaceful, efficient, and creative
;; world, and how you can help, visit https://drym.org.
;;
;; This paradigm transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.

;;; Commentary:

;; Fixed size rings with virtual rotation.

;;; Code:

(require 'ring)

(defconst virtual-ring-default-size 20)

(defun virtual-ring-make (&optional size)
  "Make a virtual ring of size SIZE.

A virtual ring is an ordinary fixed-size ring with a virtual head in
order to model rotation."
  (let* ((size (or size virtual-ring-default-size))
         (ring (make-ring size)))
    (vector ring 0)))

(defconst virtual-ring--index-ring 0
  "The index of the underlying ring in a virtual ring.")

(defconst virtual-ring--index-head 1
  "The index of the virtual head in a virtual ring.")

(defun virtual-ring-ring (vring)
  "Get the underlying ring in VRING."
  (seq-elt vring virtual-ring--index-ring))

(defun virtual-ring-head (vring)
  "Get the virtual head of VRING."
  (seq-elt vring virtual-ring--index-head))

(defun virtual-ring-set-head (vring new-head)
  "Set head on VRING to NEW-HEAD."
  (aset vring virtual-ring--index-head new-head))

(defun virtual-ring-reset-head (vring)
  "Reset head on VRING to 0 (most recent addition)."
  (virtual-ring-set-head vring 0))

(defun virtual-ring-head-rotated-p (vring)
  "Whether VRING's virtual head is rotated from its true head."
  (not (= (virtual-ring-head vring)
          0)))

(defun virtual-ring-empty-p (vring)
  "Is VRING empty?"
  (ring-empty-p (virtual-ring-ring vring)))

(defun virtual-ring-last-entry (vring)
  "The last command stored on the virtual ring VRING."
  (let ((ring (virtual-ring-ring vring)))
    (ring-ref ring 0)))

(defun virtual-ring-current-entry (vring)
  "The command stored on the virtual ring VRING at the current virtual head."
  (let ((ring (virtual-ring-ring vring))
        (head (virtual-ring-head vring)))
    (ring-ref ring head)))

(defun virtual-ring-rotate-forwards (vring)
  "Rotate VRING forwards."
  (let ((head (virtual-ring-head vring))
        (ring (virtual-ring-ring vring)))
    (virtual-ring-set-head vring
                           (ring-minus1 head
                                        (ring-length ring)))))

(defun virtual-ring-rotate-backwards (vring)
  "Rotate VRING backwards."
  (let ((head (virtual-ring-head vring))
        (ring (virtual-ring-ring vring)))
    (virtual-ring-set-head vring
                           (ring-plus1 head
                                       (ring-length ring)))))

(defun virtual-ring-remove-last (vring)
  "Remove the last (most recent) entry from VRING.

Adjust head if necessary."
  (ring-remove (virtual-ring-ring vring)
               0))

(defun virtual-ring-contents (vring)
  "Contents of virtual ring VRING."
  (ring-elements
   (virtual-ring-ring vring)))

(defun virtual-ring-store (vring entry &optional preserve-head)
  "Store ENTRY in VRING.

Unless PRESERVE-HEAD is true, reset the virtual head to the most
recently stored element, i.e., to ENTRY."
  (let ((ring (virtual-ring-ring vring)))
    (ring-insert ring entry)
    (unless preserve-head
      (virtual-ring-reset-head vring))))

(provide 'virtual-ring)
;;; virtual-ring.el ends here
