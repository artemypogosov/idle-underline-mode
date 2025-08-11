;;; idle-underline-mode.el --- Highlight the word the point is on -*- lexical-binding: t -*-

;; This package was inspired by 'idle-highlight-mode'.

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2008-2011 Phil Hagelberg, Cornelius Mika

;; Author: Phil Hagelberg, Cornelius Mika, Campbell Barton
;; Maintainer: Campbell Barton <ideasman42@gmail.com>
;; URL: https://codeberg.org/ideasman42/emacs-idle-underline-mode
;; Version: 1.1.4
;; Created: 2008-05-13
;; Keywords: convenience
;; EmacsWiki: IdleHighlight
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; M-x idle-underline-mode sets an idle timer that underlines all
;; occurrences in the buffer of the symbol under the point
;; (optionally underlineing in all other buffers as well).

;; Enabling it in a hook is recommended if you don't want it enabled
;; for all buffers, just programming ones.
;;
;; Example:
;;
;; (defun my-prog-mode-hook ()
;;   (idle-underline-mode t))
;;
;; (add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;; Code:

(eval-when-compile
  ;; For `pcase-dolist'.
  (require 'pcase))

(require 'thingatpt)

;; Custom Variables

(defgroup idle-underline nil
  "Highlight other occurrences of the word at point."
  :group 'faces)

(defface idle-underline '((t (:inherit region)))
  "Face used to underline other occurrences of the word at point.")

(defcustom idle-underline-exceptions nil
  "List of words to be excepted from underlineing."
  :type
  '(choice (repeat :tag "A list of string literals that will be excluded." string)
    (function :tag "A function taking a string, non-nil result excludes.")))

(defcustom idle-underline-exceptions-face '(font-lock-keyword-face font-lock-string-face)
  "List of exception faces."
  :type
  '(choice (repeat :tag "A list of face symbols that will be ignored." symbol)
    (function :tag "A function that takes a list of faces, non-nil result excludes.")))

(defcustom idle-underline-exceptions-syntax "^w_"
  "Syntax table to to skip.

See documentation for `skip-syntax-forward', nil to ignore."
  :type '(choice (const nil) string))

(defcustom idle-underline-exclude-point nil
  "Exclude the current symbol from underlineing."
  :type 'boolean)

(defcustom idle-underline-before-point nil
  "Highlight the text directly before the cursor."
  :type 'boolean)

(defcustom idle-underline-visible-buffers nil
  "Apply the current underline to all other visible buffers."
  :type 'boolean)

(defcustom idle-underline-idle-time 0.35
  "Time after which to underline the word at point (in seconds)."
  :type 'float)

(defcustom idle-underline-ignore-modes nil
  "List of major-modes to exclude when `idle-underline' has been enabled globally."
  :type '(repeat symbol))

(define-obsolete-variable-alias
  'global-idle-underline-ignore-buffer 'idle-underline-global-ignore-buffer "1.1.4")

(defvar-local idle-underline-global-ignore-buffer nil
  "When non-nil, the global mode will not be enabled for this buffer.
This variable can also be a predicate function, in which case
it'll be called with one parameter (the buffer in question), and
it should return non-nil to make Global `idle-underline' Mode not
check this buffer.")

;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local idle-underline--overlays nil
  "Buffer-local list of overlays.")


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun idle-underline--faces-at-point (pos)
  "Add the named faces that the `read-face-name' or `face' property use.
Argument POS return faces at this point."
  (declare (important-return-value t))
  (let ((faces nil) ; List of faces to return.
        ;; NOTE: use `get-text-property' instead of `get-char-property' so overlays are excluded,
        ;; since this causes overlays with `hl-line-mode' (for example) to mask keywords, see: #1.
        (faceprop (or (get-text-property pos 'read-face-name) (get-text-property pos 'face))))
    (cond
     ((facep faceprop)
      (push faceprop faces))
     ((face-list-p faceprop)
      (dolist (face faceprop)
        (when (facep face)
          (push face faces)))))
    faces))

(defun idle-underline--merge-overlapping-ranges (ranges)
  "Destructively modify and return RANGES with overlapping values removed.

Where RANGES is an unordered list of (min . max) cons cells."
  (declare (important-return-value t))
  (cond
   ((cdr ranges)
    ;; Simple < sorting of cons cells.
    (setq ranges
          (sort ranges
                (lambda (x y)
                  (or (< (car x) (car y)) (and (= (car x) (car y)) (< (cdr x) (cdr y)))))))
    ;; Step over `ranges', de-duplicating & adjusting elements as needed.
    (let ((ranges-iter ranges)
          (ranges-next (cdr ranges)))
      (while ranges-next
        (let ((head (car ranges-iter))
              (next (car ranges-next)))
          (cond
           ((< (cdr head) (car next))
            (setq ranges-iter ranges-next)
            (setq ranges-next (cdr ranges-next)))
           (t
            (when (< (cdr head) (cdr next))
              (setcdr head (cdr next)))
            (setq ranges-next (cdr ranges-next))
            (setcdr ranges-iter ranges-next)))))
      ranges))

   (t ; No need for complex logic single/empty lists.
    ranges)))


;; ---------------------------------------------------------------------------
;; Internal Context Checking Functions

(defun idle-underline--check-symbol-at-point (pos)
  "Return non-nil if the symbol at POS can be used."
  (declare (important-return-value t))
  (cond
   (idle-underline-exceptions-syntax
    (save-excursion
      (cond
       (idle-underline-before-point
        (or (progn
              (goto-char pos)
              (zerop (skip-syntax-forward idle-underline-exceptions-syntax (1+ pos))))
            (progn
              (goto-char pos)
              (zerop (skip-syntax-backward idle-underline-exceptions-syntax pos)))))
       (t
        (goto-char pos)
        (zerop (skip-syntax-forward idle-underline-exceptions-syntax (1+ pos)))))))
   (t
    t)))

(defun idle-underline--check-faces-at-point (pos)
  "Check if the position POS has faces that match the exclude argument."
  (declare (important-return-value t))
  (cond
   (idle-underline-exceptions-face
    (let ((result t))
      (let ((faces-at-pos (idle-underline--faces-at-point pos)))
        (when faces-at-pos
          (cond
           ((functionp idle-underline-exceptions-face)
            (when (funcall idle-underline-exceptions-face faces-at-pos)
              (setq result nil)))
           (t
            (while faces-at-pos
              (let ((face (pop faces-at-pos)))
                (when (memq face idle-underline-exceptions-face)
                  (setq result nil)
                  ;; Break.
                  (setq faces-at-pos nil))))))))
      result))
   (t ; Default to true, if there are no exceptions.
    t)))

(defun idle-underline--check-word (target)
  "Return non-nil when TARGET should not be excluded."
  (declare (important-return-value t))
  (not
   (cond
    ((functionp idle-underline-exceptions)
     (funcall idle-underline-exceptions target))
    (t
     (member target idle-underline-exceptions)))))


;; ---------------------------------------------------------------------------
;; Internal Highlight Functions

(defun idle-underline--ununderline ()
  "Clear current underline."
  (declare (important-return-value nil))
  (when idle-underline--overlays
    (mapc #'delete-overlay idle-underline--overlays)
    (setq idle-underline--overlays nil)))

(defun idle-underline--underline (target target-beg target-end visible-ranges)
  "Highlight TARGET found between TARGET-BEG and TARGET-END.

Argument VISIBLE-RANGES is a list of (min . max) ranges to underline."
  (declare (important-return-value nil))
  (idle-underline--ununderline)
  (save-excursion
    (save-match-data
      ;; Disable case folding so "Image" won't match "image"
      (let ((case-fold-search nil)
            (target-regexp (concat "\\_<" (regexp-quote target) "\\_>")))
        (pcase-dolist (`(,beg . ,end) visible-ranges)
          (goto-char beg)
          (while (re-search-forward target-regexp end t)
            (let ((match-beg (match-beginning 0))
                  (match-end (match-end 0)))
              (unless (and idle-underline-exclude-point
                           (eq target-beg match-beg)
                           (eq target-end match-end))
                (let ((ov (make-overlay match-beg match-end)))
                  (overlay-put ov 'face 'idle-underline)
                  (push ov idle-underline--overlays))))))))))


(defun idle-underline--word-at-point-args ()
  "Return arguments for `idle-underline--underline'."
  (declare (important-return-value t))
  (when (idle-underline--check-symbol-at-point (point))
    (let ((target-range (bounds-of-thing-at-point 'symbol)))
      (when (and target-range (idle-underline--check-faces-at-point (point)))
        (pcase-let ((`(,target-beg . ,target-end) target-range))
          (let ((target (buffer-substring-no-properties target-beg target-end)))
            (when (idle-underline--check-word target)
              (cons target target-range))))))))

(defun idle-underline--word-at-point-underline (target target-range visible-ranges)
  "Highlight the word under the point across all VISIBLE-RANGES.

Arguments TARGET and TARGET-RANGE
should be the result of `idle-underline--word-at-point-args'."
  (declare (important-return-value nil))
  (idle-underline--ununderline)
  (when target
    (pcase-let ((`(,target-beg . ,target-end) target-range))
      (idle-underline--underline target target-beg target-end visible-ranges))))


;; ---------------------------------------------------------------------------
;; Internal Timer Management
;;
;; This works as follows:
;;
;; - The timer is kept active as long as the local mode is enabled.
;; - Entering a buffer runs the buffer local `window-state-change-hook'
;;   immediately which checks if the mode is enabled,
;;   set up the global timer if it is.
;; - Switching any other buffer wont run this hook,
;;   rely on the idle timer it's self running, which detects the active mode,
;;   canceling it's self if the mode isn't active.
;;
;; This is a reliable way of using a global,
;; repeating idle timer that is effectively buffer local.
;;

;; Global idle timer (repeating), keep active while the buffer-local mode is enabled.
(defvar idle-underline--global-timer nil)
;; When t, the timer will update buffers in all other visible windows.
(defvar idle-underline--dirty-flush-all nil)
;; When true, the buffer should be updated when inactive.
(defvar-local idle-underline--dirty nil)

(defun idle-underline--time-callback-or-disable ()
  "Callback that run the repeat timer."
  (declare (important-return-value nil))

  ;; Ensure all other buffers are underlineed on request.
  (let ((is-mode-active (bound-and-true-p idle-underline-mode))
        (buf-current (current-buffer))
        (dirty-buffer-list (list))
        (force-all idle-underline-visible-buffers))

    ;; When this buffer is not in the mode, flush all other buffers.
    (cond
     (is-mode-active
      (setq idle-underline--dirty t))
     (t
      ;; If the timer ran when in another buffer,
      ;; a previous buffer may need a final refresh, ensure this happens.
      (setq idle-underline--dirty-flush-all t)))

    (when force-all
      (setq idle-underline--dirty-flush-all t))

    ;; Accumulate visible ranges in each buffers `idle-underline--dirty'
    ;; value which is temporarily used as a list to store ranges.
    (dolist (frame (frame-list))
      (dolist (win (window-list frame -1))
        (let ((buf (window-buffer win)))
          (when (cond
                 (idle-underline--dirty-flush-all
                  (and (buffer-local-value 'idle-underline-mode buf)
                       (or (buffer-local-value 'idle-underline--dirty buf) force-all)))
                 (t
                  (eq buf buf-current)))

            (unless (memq buf dirty-buffer-list)
              (push buf dirty-buffer-list))

            (with-current-buffer buf
              (when (eq idle-underline--dirty t)
                (setq idle-underline--dirty nil))
              ;; Push a (min . max) cons cell,
              ;; expanded to line bounds (to avoid clipping words).
              (save-excursion
                (push (cons
                       (progn
                         (goto-char (max (point-min) (window-start win)))
                         (pos-bol))
                       (progn
                         (goto-char (min (point-max) (window-end win)))
                         (pos-eol)))
                      idle-underline--dirty)))))))


    (let ((target-args (and force-all (idle-underline--word-at-point-args))))
      (dolist (buf dirty-buffer-list)
        (with-current-buffer buf
          (let ((visible-ranges idle-underline--dirty))
            ;; Restore this values status as a boolean.
            (setq idle-underline--dirty nil)

            (setq visible-ranges (idle-underline--merge-overlapping-ranges visible-ranges))

            (unless force-all
              (setq target-args (idle-underline--word-at-point-args)))

            (pcase-let ((`(,target . ,target-range) target-args))
              (when (and force-all (not (eq buf buf-current)))
                (setq target-range nil))
              (idle-underline--word-at-point-underline target target-range visible-ranges))))))

    (cond
     (is-mode-active
      ;; Always keep the current buffer dirty
      ;; so navigating away from this buffer will refresh it.
      (setq idle-underline--dirty t))
     (t ; Cancel the timer until the current buffer uses this mode again.
      (idle-underline--time-ensure nil)))))

(defun idle-underline--time-ensure (state)
  "Ensure the timer is enabled when STATE is non-nil, otherwise disable."
  (declare (important-return-value nil))
  (cond
   (state
    (unless idle-underline--global-timer
      (setq idle-underline--global-timer
            (run-with-idle-timer
             idle-underline-idle-time
             :repeat #'idle-underline--time-callback-or-disable))))
   (t
    (when idle-underline--global-timer
      (cancel-timer idle-underline--global-timer)
      (setq idle-underline--global-timer nil)))))

(defun idle-underline--time-reset ()
  "Run this when the buffer change was changed."
  (declare (important-return-value nil))
  ;; Ensure changing windows doesn't leave other buffers with stale underline.
  (cond
   ((bound-and-true-p idle-underline-mode)
    (setq idle-underline--dirty-flush-all t)
    (setq idle-underline--dirty t)
    (idle-underline--time-ensure t))
   (t
    (idle-underline--time-ensure nil))))

(defun idle-underline--time-buffer-local-enable ()
  "Ensure buffer local state is enabled."
  (declare (important-return-value nil))
  ;; Needed in case focus changes before the idle timer runs.
  (setq idle-underline--dirty-flush-all t)
  (setq idle-underline--dirty t)
  (idle-underline--time-ensure t)
  (add-hook 'window-state-change-hook #'idle-underline--time-reset nil t))

(defun idle-underline--time-buffer-local-disable ()
  "Ensure buffer local state is disabled."
  (declare (important-return-value nil))
  (kill-local-variable 'idle-underline--dirty)
  (idle-underline--time-ensure nil)
  (remove-hook 'window-state-change-hook #'idle-underline--time-reset t))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun idle-underline--enable ()
  "Enable the buffer local minor mode."
  (declare (important-return-value nil))
  (idle-underline--set-default-exceptions)
  (idle-underline--time-buffer-local-enable))


(defun idle-underline--disable ()
  "Disable the buffer local minor mode."
  (declare (important-return-value nil))
  (idle-underline--time-buffer-local-disable)
  (idle-underline--ununderline)
  (kill-local-variable 'idle-underline--overlays))

(defun idle-underline--set-default-exceptions ()
  "Set default exceptions for idle-underline-mode based on the current major mode."
  (cond
   ;; JavaScript / TypeScript
   ((derived-mode-p 'js-mode 'js2-mode 'rjsx-mode 'typescript-mode 'tsx-mode)
    (setq-local idle-underline-exceptions
                '("import" "from" "export" "const" "let" "var" "function" "return")))
   ;; Python
   ((derived-mode-p 'python-mode)
    (setq-local idle-underline-exceptions
                '("list" "tuple" "int" "float" "str" "bool")))
   ;; Default
   (t
    (setq-local idle-underline-exceptions nil))))

(defun idle-underline--turn-on ()
  "Enable command `idle-underline-mode'."
  (declare (important-return-value nil))
  (when (and
         ;; Not already enabled.
         (not (bound-and-true-p idle-underline-mode))
         ;; Not in the mini-buffer.
         (not (minibufferp))
         ;; Not a special mode (package list, tabulated data ... etc)
         ;; Instead the buffer is likely derived from `text-mode' or `prog-mode'.
         (not (derived-mode-p 'special-mode))
         ;; Not explicitly ignored.
         (not (memq major-mode idle-underline-ignore-modes))
         ;; Optionally check if a function is used.
         (or (null idle-underline-global-ignore-buffer)
             (cond
              ((functionp idle-underline-global-ignore-buffer)
               (not (funcall idle-underline-global-ignore-buffer (current-buffer))))
              (t
               nil))))
    (idle-underline-mode 1)))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(define-minor-mode idle-underline-mode
  "Idle-Highlight Minor Mode."
  :global nil

  (cond
   (idle-underline-mode
    (idle-underline--enable))
   (t
    (idle-underline--disable))))

;;;###autoload
(define-globalized-minor-mode idle-underline-global-mode
  idle-underline-mode
  idle-underline--turn-on)

(define-obsolete-function-alias 'global-idle-underline-mode #'idle-underline-global-mode "1.1.4")

(provide 'idle-underline-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; idle-underline-mode.el ends here
