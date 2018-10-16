
;;---------------------------- package ------------------------------------------
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/awesome-tab/"))
;;github: https://github.com/peter112358/company-english-helper
(add-to-list 'load-path (expand-file-name "~/.emacs.d/github/company-english-helper/"))

;;---------------------------- UI ------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(global-hl-line-mode)
(global-linum-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;---------------------------- custom ------------------------------------------
(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;未选中的窗口不显示光标
(setq-default cursor-in-non-selected-windows nil)
;;美化lambda
(global-prettify-symbols-mode t)
(setq initial-major-mode (quote text-mode))

;; brew install terminal-notifier
(defun notify-osx (title message)
	(call-process "/usr/local/bin/terminal-notifier"
		nil 0 nil
        "-group" "emacs"
        "-title" title
		"-message" message))

;;auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(setq company-english-helper-fuzz-search-p t)
;;---------------------------- theme ------------------------------------------
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
 )

;;---------------------------- mode ------------------------------------------
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'dired-collapse-mode)

;;treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              10
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
        ([f8]        . treemacs-toggle)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package calfw
  :ensure t)

;;(global-git-gutter-mode +1)

;;(use-package org-brain :ensure t
;;  :init
;;  (setq org-brain-path "directory/path/where-i-want-org-brain")
;;  ;; For Evil users
;;  (with-eval-after-load 'evil
;;    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;  :config
;;  (setq org-id-track-globally t)
;;  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
;;  (push '("b" "Brain" plain (function org-brain-goto-end)
;;          "* %i%?" :empty-lines 1)
;;        org-capture-templates)
;;  (setq org-brain-visualize-default-choices 'all)
;;  (setq org-brain-title-max-length 12))

(require 'company-english-helper)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(add-hook 'java-mode-hook
          (lambda () (setq-local tab-width 2)))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 3);;"path/to/your/image.png"
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))


(use-package sublimity
  :config
  (sublimity-mode 1)
  )

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-move-cursor-after-expanding nil)
  (setq emmet-expand-jsx-className? t)
  )


(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package better-defaults)

(use-package pallet
  :config
  (pallet-mode t))

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure t)

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package indent-guide
      :ensure t
      :hook (after-init . indent-guide-global-mode))

(use-package which-key
  :config
  (which-key-mode))

;;(use-package meghanada
;;  :config
;;  (add-hook 'java-mode-hook
;;          (lambda ()
;;            ;; meghanada-mode on
;;            (meghanada-mode t)
;;            (flycheck-mode +1)
;;            (setq c-basic-offset 2)
;;            ;; use code format
;;            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

(use-package neotree
  :init
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

(defun neotree-projectile-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

;; removes line numbers from neotree, still need to remove from home
(add-hook 'neo-after-create-hook (lambda (_unused) (linum-mode -1)))

(global-company-mode t)

(use-package multi-term
  :init
  (setq multi-term-program "/usr/local/bin/zsh"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package anzu
  :config
  (global-anzu-mode +1))

(use-package dired-rainbow
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;(use-package symon
;;  :ensure t
;;  :config
;;  (symon-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun new-empty-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

;; pomodoro notifier
(defun notify-osx (title message)
	(call-process "terminal-notifier"
		nil 0 nil
		"-group" "Emacs"
		"-title" title
		"-message" message))
;;;add emoji supopport
;;(add-hook 'after-init-hook #'global-emojify-mode)

(add-hook 'org-pomodoro-started-hook
	(lambda ()
	(notify-osx "🍅🍅🍅" "Pomodoro is started.")))

(add-hook 'org-pomodoro-finished-hook
	(lambda ()
	(notify-osx "🍅🍅🍅" "Time for a break.")))
(add-hook 'org-pomodoro-break-finished-hook
	(lambda ()
        (notify-osx "🍅🍅🍅" "Ready for Another?")))
(add-hook 'org-pomodoro-long-break-finished-hook
	(lambda ()
		(notify-osx "🍅🍅🍅" "Ready for Another?")))
(add-hook 'org-pomodoro-killed-hook
	(lambda ()
		(notify-osx "🍅🍅🍅" "One does not simply kill a pomodoro!")))
		 
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "SPC" 'counsel-M-x
    "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "ff" 'counsel-find-file
    "fn" 'new-empty-buffer
    "bk" 'kill-current-buffer
    "gs" 'magit-status
    "bl" 'counsel-ibuffer
    "qr," 'anzu-query-replace
    "qr." 'anzu-query-replace-at-cursor
    "fs" 'swiper
    "jj" 'dumb-jump-go
    "jb" 'dumb-jump-back
    "pp" 'projectile-switch-project
    "ps" 'counsel-ag
    "pd" 'neotree-projectile-dir
    "pf" 'counsel-git
    "bms" 'bookmark-set
    "aeh" 'toggle-company-english-helper
    "bml" 'bookmark-bmenu-list

    "hf" 'find-function
    ))

;;---------------------------- keymap ------------------------------------------
(global-set-key [f8] 'neotree-projectile-dir)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)


;;---------------------------- custom ------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.01)
 '(custom-safe-themes
   (quote
    ("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(org-agenda-files (quote ("~/sjtu.org")))
 '(package-selected-packages
   (quote
    (emojify xpm major-mode-icons all-the-icons-ivy treemacs-projectile treemacs-evil async-await ggtags calfw git-gutter org-brain solarized-theme hide-mode-line org-pomodoro dired-narrow dired-sidebar dired-collapse all-the-icons-dired 2048-game which-key web-mode use-package treemacs toc-org symon sublimity smartparens smart-tabs-mode prodigy popwin paredit pallet org-download org-cliplink org-bullets nyan-mode neotree multiple-cursors multi-term meghanada markdown-mode magit latex-preview-pane java-snippets indent-guide idle-highlight-mode htmlize helm go-snippets flycheck-cask expand-region exec-path-from-shell evil-leader evil-collection evil-anzu emmet-mode dumb-jump drag-stuff doom-themes doom-modeline dired-rainbow dashboard counsel company-emacs-eclim company-auctex better-defaults auctex-latexmk))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;server start
(require 'server)
(unless (server-running-p)
  (server-start))
