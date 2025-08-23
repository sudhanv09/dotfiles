 ;;; -*- lexical-binding: t; -*-

(defvar-keymap Buffer
  :doc "Buffers"
  "b" #'switch-to-buffer
  "l" #'electric-buffer-list
  "d" #'kill-buffer
  "p" #'previous-buffer
  "n" #'next-buffer
  "B" #'switch-to-buffer
  "r" #'rename-buffer
  "R" #' revert-buffer)

(defvar-keymap Window
  :doc "Window Management"

  "H" #'shrink-window-horizontally
  "L" #'enlarge-window-horizontally
  "J" #'shrink-window
  "K" #'enlarge-window

  "s" #'split-window-below
  "v" #'split-window-right

  
  "o" #'delete-other-windows
  
  "u" #'winner-undo
  "r" #'winner-redo

  "w" #'ace-window)

(defvar-keymap Git
  :doc "Magit / Git Commands"
  "g" #'magit-status
  "l" #'magit-log
  "d" #'magit-diff
  "F" #'magit-file-dispatch
  "x" #'magit-dispatch)

(defalias 'Git Git)
(defalias 'Window Window)
(defalias 'Buffer Buffer)

(define-key global-map (kbd "C-c b") 'Buffer)
(define-key global-map (kbd "C-c w") 'Window)
(define-key global-map (kbd "C-c g") 'Git)


(provide 'keybinds)
