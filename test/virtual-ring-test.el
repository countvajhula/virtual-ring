;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.
;; But, to avoid having to evaluate the changes (which may affect the live
;; environment), it may be preferable to `make test` at the shell, instead.

;; Notes:
;; - If you see "lisp nesting exceeds max-lisp-eval-depth"
;;   while running these tests, it could be that you have a duplicate
;;   "body" invocation within one of the nested fixtures. Since these
;;   are dynamically bound, every fixture needs to have a distinct
;;   name for the body argument.
;; - If you see errors like "(void-function t)", "(void-function nil)"
;;   and "invalid function nil . 0"
;;   then you probably are using a fixture without wrapping the body
;;   in a lambda

;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar virtual-ring-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat virtual-ring-test-setup-directory dir)))

;;

(require 'virtual-ring)

;;
;; Fixtures
;;


;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html


(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

(defconst fixture-test-element "abcd")
(defconst fixture-test-element-2 "efgh")
(defconst fixture-test-element-3 "ijkl")

(defun fixture-0-ring (body)
  (let ((vring nil))
    (unwind-protect
        (progn (setq vring (virtual-ring-make))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq vring nil))))

(defun fixture-1-ring (body-1)
  (with-fixture fixture-0-ring
    (ring-insert (virtual-ring-ring vring)
                 fixture-test-element)
    (funcall body-1)))

(defun fixture-2-ring (body-2)
  (with-fixture fixture-1-ring
    (ring-insert (virtual-ring-ring vring)
                 fixture-test-element-2)
    (funcall body-2)))

(defun fixture-3-ring (body-3)
  (with-fixture fixture-2-ring
    (ring-insert (virtual-ring-ring vring)
                 fixture-test-element-3)
    (funcall body-3)))

;;
;; Tests
;;

(ert-deftest virtual-ring-test ()
  ;; null constructor
  (should (vectorp (virtual-ring-make)))
  (should (vectorp (virtual-ring-make 10)))
  (should (= 10 (ring-size
                 (virtual-ring-ring
                  (virtual-ring-make 10)))))
  (should (= 0 (virtual-ring-head
                (virtual-ring-make))))

  ;; virtual-ring-ring
  (with-fixture fixture-0-ring
    (should (ring-p (virtual-ring-ring vring))))

  ;; virtual-ring-head
  (with-fixture fixture-0-ring
    (should (= 0 (virtual-ring-head vring))))

  ;; virtual-ring-set-head
  (with-fixture fixture-0-ring
    (virtual-ring-set-head vring 1)
    (should (= 1 (virtual-ring-head vring))))

  ;; virtual-ring-reset-head
  (with-fixture fixture-0-ring
    (virtual-ring-set-head vring 1)
    (virtual-ring-reset-head vring)
    (should (= 0 (virtual-ring-head vring))))

  ;; virtual-ring-head-rotated-p
  (with-fixture fixture-3-ring
    (virtual-ring-set-head vring 1)
    (should (virtual-ring-head-rotated-p vring)))
  (with-fixture fixture-3-ring
    (virtual-ring-set-head vring 0)
    (should-not (virtual-ring-head-rotated-p vring)))

  ;; virtual-ring-empty-p
  (with-fixture fixture-0-ring
    (should (virtual-ring-empty-p vring)))
  (with-fixture fixture-1-ring
    (should-not (virtual-ring-empty-p vring))))

(ert-deftest virtual-ring-last-entry-test ()
  (with-fixture fixture-1-ring
    (should (equal (virtual-ring-last-entry vring)
                   fixture-test-element))))

(ert-deftest virtual-ring-current-entry-test ()
  (with-fixture fixture-3-ring
    (virtual-ring-set-head vring 1)
    (should (equal (virtual-ring-current-entry vring)
                   fixture-test-element-2))))

(ert-deftest virtual-ring-remove-last-test ()
  (with-fixture fixture-3-ring
    (virtual-ring-remove-last vring)
    (should (equal (virtual-ring-last-entry vring)
                   fixture-test-element-2))))

(ert-deftest virtual-ring-store-test ()
  (with-fixture fixture-0-ring
    (virtual-ring-store vring fixture-test-element)
    (should (equal fixture-test-element
                   (virtual-ring-last-entry vring))))
  (with-fixture fixture-1-ring
    (let ((new-element "new-element"))
      (virtual-ring-store vring new-element)
      (should (equal new-element
                     (virtual-ring-last-entry vring)))
      (should (= 2
                 (ring-length
                  (virtual-ring-ring vring))))))
  (with-fixture fixture-3-ring
    ;; should reset head upon storing new element
    (virtual-ring-set-head vring 1)
    (virtual-ring-store vring fixture-test-element)
    (should (= 0 (virtual-ring-head vring))))
  (with-fixture fixture-3-ring
    ;; preserve head
    (virtual-ring-set-head vring 1)
    (virtual-ring-store vring fixture-test-element :preserve-head)
    (should (= 1 (virtual-ring-head vring))))
  (with-fixture fixture-3-ring
    ;; should store elements in order of recency
    (virtual-ring-store vring fixture-test-element)
    (should (equal (list fixture-test-element ; most recently added
                         fixture-test-element-3
                         fixture-test-element-2
                         fixture-test-element)
                   (virtual-ring-contents vring))))
  (with-fixture fixture-3-ring
    ;; should store new element at underlying ring head
    ;; (i.e., in order of most recently stored)
    ;; even if surface ring "virtual" head is different
    (virtual-ring-set-head vring 1)
    (virtual-ring-store vring fixture-test-element)
    (should (equal (list fixture-test-element ; most recently added
                         fixture-test-element-3
                         fixture-test-element-2
                         fixture-test-element)
                   (virtual-ring-contents vring)))))

(ert-deftest virtual-ring-contents-test ()
  (with-fixture fixture-0-ring
    (should-not (virtual-ring-contents vring)))
  (with-fixture fixture-1-ring
    (should (virtual-ring-contents vring))))

(ert-deftest virtual-ring-rotate-test ()
  (with-fixture fixture-3-ring
    (virtual-ring-rotate-forwards vring)
    (should (equal fixture-test-element
                   (virtual-ring-current-entry vring))))
  (with-fixture fixture-3-ring
    (virtual-ring-rotate-backwards vring)
    (should (equal fixture-test-element-2
                   (virtual-ring-current-entry vring)))))
