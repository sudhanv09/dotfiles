(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

(setq user-emacs-directory (file-name-directory load-file-name))
(setq user-full-name "Sudhanv"
      user-mail-address "mystogun125@gmail.com")

(defvar user-cache-directory (concat user-emacs-directory "cache/"))

(add-to-list 'load-path "~/.config/emacs/lisp")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(electric-pair-mode t)
(show-paren-mode 1)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(global-font-lock-mode t)
(winner-mode 1)

(setq auto-save-default nil)
(setq font-lock-maximum-decoration t)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)

(setq package-enable-at-startup nil) ;; Disables the default package manager.
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package darcula-theme
  :ensure t
  :config
  (load-theme 'darcula t))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  (dashboard-items '((agenda . 5)
                     (projects . 5)
                     (recents . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator nil)
  (dashboard-set-init-info t)
  (dashboard-set-footer t)
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :straight t
  :defer t
  :custom
  (doom-modeline-buffer-file-name 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name t)
  (doom-modeline-vcs-max-length 25)
  :config
  (setq doom-modeline-icon t)
    :hook
    (after-init . doom-modeline-mode))

(use-package which-key
    :ensure t
  :diminish
  :config (which-key-mode))

(use-package ace-window
  :ensure t
  :init (setq aw-scope 'global))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t) )

(use-package vc
  :ensure nil                        ;; This is built-in, no need to fetch it.
  :defer t
  :bind
  (("C-x v d" . vc-dir)              ;; Open VC directory for version control status.
   ("C-x v =" . vc-diff)             ;; Show differences for the current file.
   ("C-x v D" . vc-root-diff)        ;; Show differences for the entire repository.
   ("C-x v v" . vc-next-action))     ;; Perform the next version control action.
  :config
  ;; Better colors for <leader> g b  (blame file)
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

(use-package org
  :ensure nil
  :defer t)

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))


(use-package orderless
  :ensure t
  :straight t
  :defer t                                    ;; Load Orderless on demand.
  :after vertico                              ;; Ensure Vertico is loaded before Orderless.
  :init
  (setq completion-styles '(orderless basic)  ;; Set the completion styles.
        completion-category-defaults nil      ;; Clear default category settings.
        completion-category-overrides '((file (styles partial-completion))))) ;; Customize file completion styles.

(use-package magit
  :ensure t
)

(use-package meow
  :ensure t
  :demand t
  :config
  (require 'meow-config)
  (require 'keybinds)
  (meow-global-mode 1))

(use-package json)

(use-package lsp-mode
  :ensure t
  :straight t
  :defer t
  :hook (
	 (
	  (js-mode
	   tsx-ts-mode
	   typescript-ts-base-mode
	   css-mode
	   go-ts-mode-hook
	   js-ts-mode
	   web-mode)
	  . lsp-deferred))
  :commands lsp
  :custom

  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-inlay-hint-enable nil)                           ;; Usage of inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0.5)                                  ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color t)                    ;; Enable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all t)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable t)                                   ;; Enable lens support.
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)   ;; Enable symbol numbers in the headerline.
  (lsp-headerline-arrow "▶")                            ;; Set arrow for headerline.
  (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics in headerline.
  (lsp-headerline-breadcrumb-icons-enable nil)          ;; Disable icons in breadcrumb.
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover t
	lsp-ui-doc-enable t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "┃") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "┃")
                                  (unknown . "┆")
                                  (ignored . "i"))))


(use-package indent-guide
  :defer t
  :straight t
  :ensure t
  :hook
  (prog-mode . indent-guide-mode)  ;; Activate indent-guide in programming modes.
  :config
  (setq indent-guide-char "│"))

(use-package treesit-auto
  :ensure t
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package darcula-theme
  :ensure t)

(defvar user-cache-directory (expand-file-name "cache/" user-emacs-directory))
(make-directory user-cache-directory t)

(use-package projectile
  :ensure t
  :straight t
  :demand t
  :init
  (setq projectile-project-search-path '("~/code"))
  ;; ensure projectile saves its files in a nice location
  (setq projectile-cache-file
        (concat user-cache-directory "projectile.cache"))
  (setq projectile-known-projects-file
        (concat user-cache-directory "projectile-bookmarks.eld"))
 
  :config
  (projectile-mode 1)
 (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-globally-ignored-file-suffixes
        '(
          ;; images
          ".png" ".xcf" ".jpg" ".jpeg" ".tif"
          ;; fonts
          ".ttf"
          ;; misc
          ".pdf"
          ))
  (setq projectile-indexing-method 'hybrid))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))
