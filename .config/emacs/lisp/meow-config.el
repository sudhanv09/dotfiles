;;; meow.el --- Meow modal editing config

(setq emacs-local-leader-prefix "SPC")
(setq meow-local-leader-prefix "/")
(setq meow-local-leader-insert-prefix "C-/")
(setq meow-expand-hint-remove-delay 3.0)

(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(add-hook 'git-commit-mode-hook 'meow-insert-mode)

(defun meow-expanding-p ()
  "Return non-NIL when `meow' is either expanding or selecting text."
  (meow--selection-type))

(defmacro def-meow-digit-action (func digit)
  "Create FUNC to expand DIGIT while expanding, otherwise pass digit arg."
  (let ((expand (intern (format "meow-expand-%d" digit))))
    `(defun ,func ()
       (interactive)
       (if (meow-expanding-p)
           (,expand)
         (meow-digit-argument)))))

(dotimes (i 10)
  (eval `(def-meow-digit-action ,(intern (format "meow-%d" i)) ,i)))

(fmakunbound 'def-meow-digit-action)

(defun reload-config ()
  "Reload Emacs config"
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(setq global-leader-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "c") 'meow-keypad-start)
         (define-key map (kbd "g") 'meow-keypad-start)
         (define-key map (kbd "h") 'meow-keypad-start)
         (define-key map (kbd "m") 'meow-keypad-start)
         (define-key map (kbd "x") 'meow-keypad-start)
         map))

(meow-motion-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 `("SPC" . ,global-leader-map))

(meow-normal-define-key
 `(,meow-local-leader-prefix . ,emacs-local-leader-prefix)
 '("1" . meow-1) '("2" . meow-2) '("3" . meow-3) '("4" . meow-4)
 '("5" . meow-5) '("6" . meow-6) '("7" . meow-7) '("8" . meow-8)
 '("9" . meow-9) '("0" . meow-0)
 '("-" . negative-argument)
 '(";" . meow-reverse) '("," . meow-inner-of-thing) '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing) '("]" . meow-end-of-thing)
 '("a" . meow-append) '("A" . meow-open-below)
 '("b" . meow-back-word) '("B" . meow-back-symbol)
 '("c" . meow-change) '("d" . meow-delete) '("D" . meow-backward-delete)
 '("e" . meow-next-word) '("E" . meow-next-symbol)
 '("f" . meow-find) '("g" . meow-cancel-selection) '("G" . meow-grab)
 '("h" . meow-left) '("H" . meow-left-expand)
 '("i" . meow-insert) '("I" . meow-open-above)
 '("j" . meow-next) '("J" . meow-next-expand)
 '("k" . meow-prev) '("K" . meow-prev-expand)
 '("l" . meow-right) '("L" . meow-right-expand)
 '("m" . meow-join) '("n" . meow-search)
 '("o" . meow-block) '("O" . meow-to-block)
 '("p" . meow-yank) '("q" . keyboard-quit) '("Q" . meow-goto-line)
 '("r" . meow-replace) '("R" . meow-swap-grab)
 '("s" . meow-kill) '("t" . meow-till)
 '("u" . meow-undo) '("U" . meow-undo-in-selection)
 '("v" . meow-visit) '("w" . meow-mark-word) '("W" . meow-mark-symbol)
 '("x" . meow-line) '("X" . meow-goto-line)
 '("y" . meow-save) '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection) '("'" . repeat)
 '("<escape>" . keyboard-quit))

(define-key meow-normal-state-keymap (kbd "y") #'kill-ring-save)

(define-key meow-insert-state-keymap
	    (kbd meow-local-leader-insert-prefix)
	    (meow--parse-def emacs-local-leader-prefix))

(meow-leader-define-key
 '("rr" . reload-config))

(provide 'meow-config)

