(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(if (equal system-type 'windows-nt)
    (progn (setq explicit-shell-file-name
                 "C:/Program Files (x86)/Git/bin/sh.exe")
           (setq shell-file-name explicit-shell-file-name)
           (setq explicit-sh.exe-args '("--login" "-i"))
           (setenv "SHELL" shell-file-name)
           (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(package-initialize)

(setq package-list
      '(boron-theme
        anzu
        buffer-move
        company
        company-web
        dired+
        emmet-mode
        color-theme

        ;; Evil
        evil
        evil-leader
        flycheck

        ;; Git
        git-gutter
        git-timemachine

        ;; Helm
        helm
        helm-ag
        helm-company
        helm-projectile
        helm-swoop

        highlight-symbol
        magit
        multi-eshell
        multi-term
        omnisharp
        smex
        web-mode
        ac-html-bootstrap
        ac-html-csswatcher
        weechat))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path
             (expand-file-name "themes" user-emacs-directory))

(mapc (lambda (map)
        (define-key input-decode-map
          (read-kbd-macro (cadr map))
          (read-kbd-macro (car map))))
      '(("<backtab>"    "ESC [ Z")

        ("<S-up>"       "ESC [1;2A")
        ("<S-down>"     "ESC [1;2B")
        ("<S-right>"    "ESC [1;2C")
        ("<S-left>"     "ESC [1;2D")

        ("<M-up>"       "ESC [1;3A")
        ("<M-down>"     "ESC [1;3B")
        ("<M-right>"    "ESC [1;3C")
        ("<M-left>"     "ESC [1;3D")

        ("<M-S-up>"     "ESC [1;4A")
        ("<M-S-down>"   "ESC [1;4B")
        ("<M-S-right>"  "ESC [1;4C")
        ("<M-S-left>"   "ESC [1;4D")

        ("<C-up>"       "ESC [1;5A")
        ("<C-down>"     "ESC [1;5B")
        ("<C-right>"    "ESC [1;5C")
        ("<C-left>"     "ESC [1;5D")

        ("<C-S-up>"       "ESC [1;6A")
        ("<C-S-down>"     "ESC [1;6B")
        ("<C-S-right>"    "ESC [1;6C")
        ("<C-S-left>"     "ESC [1;6D")

        ("<C-prior>"    "ESC [5;5~")
        ("<C-next>"     "ESC [6;5~")
        ("<C-delete>"   "ESC [3;5~")
                ))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq inhibit-startup-message t)

(setq magit-last-seen-setup-instructions "1.4.0")

(setq omnisharp-server-executable-path "~/omnisharp-roslyn/scripts/Omnisharp.cmd")
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(if (display-graphic-p)
    (set-face-attribute 'default nil
                    :height (if (eq system-type 'windows-nt) 90 100)
                    :font (if (eq system-type 'windows-nt) "Consolas" "Inconsolata")))

(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)

  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))
(defun my-emacs-theme ()
  (global-hl-line-mode)
  ;;(load- 'sanityinc-tomorrow-night t)
  (require 'color-theme)
  (color-theme-initialize)
  (require 'color-theme-less)
  (color-theme-less))

(defun my-hilight-symbol-hook ()
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(defun my-anzu-mode ()
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

(defun my-buffer-move ()
  (windmove-default-keybindings)
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right)
  (global-set-key (kbd "C-<left>")   'shrink-window-horizontally)
  (global-set-key (kbd "C-<right>")  'enlarge-window-horizontally)
  (global-set-key (kbd "C-<down>")   'shrink-window)
  (global-set-key (kbd "C-<up>")     'enlarge-window))

(defun my-smex-mode ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun my-evil-conf ()
  (evil-mode 1)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (add-hook 'neotree-mode-hook
            (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))


(setq weechat-modules '(weechat-button
                        weechat-complete
                        weechat-tracking))

(eval-after-load 'weechat
  '(progn
     (setq weechat-color-list
           '(unspecified "black" "dark gray" "dark red" "red"
                         "dark green" "light green" "brown"
                         "yellow" "RoyalBlue3"
                         "light blue"
                         "dark magenta" "magenta" "dark cyan"
                         "light cyan" "gray" "white")
           weechat-prompt "> "
           weechat-auto-monitor-buffers t
           weechat-complete-nick-ignore-self nil
           weechat-button-buttonize-nicks nil
           weechat-sync-active-buffer t)
     
     (setq weechat-conf-file (expand-file-name "weechat-conf.el" user-emacs-directory))
     (if (file-exists-p weechat-conf-file)
         (load weechat-conf-file))
     (require 'gnutls)
     (add-to-list 'gnutls-trustfiles (expand-file-name (concat user-emacs-directory "/relay.cert")))
     (set-face-background 'weechat-highlight-face "dark red")
     (set-face-foreground 'weechat-highlight-face "light grey")

     (tracking-mode)))

(defun my-company-mode ()
  (require 'company)
  (require 'company-web-html))

(defun my-web-mode ()
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

  (setq web-mode-content-alist
        '(("xml" . "*\\.config\\'"))))
  

(defun my-after-init-hook ()
  (my-web-mode)
  (my-company-mode)
  (my-emacs-theme)
  (projectile-global-mode)
  (global-company-mode)
  (global-flycheck-mode)
  (my-hilight-symbol-hook)
  (my-anzu-mode)
  (my-buffer-move)
  (my-smex-mode)
  (winner-mode 1)
  (eldoc-mode)
  (my-evil-conf))

(setq projectile-require-project-root nil
      projectile-enable-caching t)

(defun my-csharp-hooks ()
  (omnisharp-mode))

(add-hook 'after-init-hook '(lambda () (require 'dired+)))
(add-hook 'after-init-hook 'my-after-init-hook)
(add-hook 'csharp-mode-hook 'my-csharp-hooks)

(require 'server)
(unless (server-running-p) (server-start))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-chromium)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
