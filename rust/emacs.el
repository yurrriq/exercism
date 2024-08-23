;; See also: https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

(column-number-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(require 'package)
(setq-default frames-only-mode t
              indent-tabs-mode nil
              inhibit-splash-screen t
              package-archives nil
              package-enable-at-startup nil)
(package-initialize)

(load-theme 'wombat)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)

(set-face-attribute 'default nil :family "Iosevka Custom" :height 110)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)

(use-package company
  :custom
  (company-idle-begin 0.5)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-<" . company-select-first)
        ("M->" . company-select-last)))

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))

(use-package direnv
  :config
  (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package fill-column-indicator
  :config
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode))

(use-package flycheck)

(use-package hl-todo
  :demand
  :config (global-hl-todo-mode t))

(use-package lsp-mode
  :hook (rustic-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-modeline-code-actions-enable nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-server 'rust-analyzer)
  :config
  (advice-add 'lsp :before #'direnv-update-environment))

(use-package lsp-ui
  :hook (rustic-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-hover t)
  :config
  ;; NOTE: https://github.com/emacs-lsp/lsp-ui/issues/285
  (set-face-attribute 'markdown-code-face nil :family "Iosevka Custom" :height 110))

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-status)))

(use-package nix-mode)

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :hook (rustic-mode . eb/rustic-mode-hook)
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-on-save t)
  :custom
  ;; https://rust-lang.github.io/rustfmt/
  (rustic-rustfmt-config-alist
   '(("blank_lines_upper_bound" . 2)
     ("combine_control_expr" . "false")
     ("comment_width" . 80)
     ("format_code_in_doc_comments" . "true")
     ("format_strings" . "true")
     ("group_imports" . "StdExternalCrate")
     ("imports_granularity" . "Module")
     ("match_block_trailing_comma" . "true")
     ("max_width" . 80)
     ("reorder_impl_items" . "true")
     ("space_after_colon" . "true")
     ("space_before_colon" . "false")
     ("struct_field_align_threshold" . 20)
     ("use_try_shorthand" . "true")
     ("wrap_comments" . "true"))))

(defun eb/rustic-mode-hook ()
  ;; NOTE: https://github.com/brotzeit/rustic/issues/253
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package smex
  :demand
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t))

(use-package yaml-mode)
