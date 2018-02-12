
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (treemacs flycheck-rust magit irony go-mode flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#0F1011" :foreground "#AAAAAA" :box (:line-width 1 :color "gray16") :family "DejaVu Sans")))))

(require 'package)

;adding melpa repository
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)


;remove tool bar
(tool-bar-mode -1)

;adding colour theme
(add-to-list 'custom-theme-load-path "/home/joedao/.emacs.d/themes/")
(load-theme 'atom-one-dark t)

;add linums and set tab size
(global-linum-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;setup flycheck for c++ and rust
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
(setq-default flycheck-disabled-checkers '(c/c++-clang))
                                        ;(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;company mode
(add-hook 'after-init-hook 'global-company-mode)

;set indentation style for c and c++
;(setq c++-default-style "linux" c-basic-offset 4)
;setq c-default-style "linux" c-basic-offset 4)
(defun my-c-mode-common-hook ()
 
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list '(0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; irony setup
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;;set gdb window settings
(setq
 gdb-many-windows t
 gdb-show-main t
 )

;;setup for treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window))
  )

;;hot keys setup for magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;toggle transparentcy
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


(provide 'init)
;;; init.el ends here
