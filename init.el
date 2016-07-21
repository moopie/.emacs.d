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
      '(
        ;; themes
        base16-theme

        ;; Evil
        evil
        evil-leader

        ;; Git
        magit
        git-gutter
        git-timemachine

        ;; Helm
        helm
        helm-ag
        helm-company
        helm-projectile
        helm-swoop

        ;; languages

        ;; c#
        omnisharp

        ;; web stuff
        company-web
        emmet-mode
        web-mode
        ac-html-bootstrap
        ac-html-csswatcher

        ;; javascript
        js2-mode
        tern
        company-tern

        ;; typescript
        tide
        tss

        ;; Utils
        company
        anzu
        buffer-move
        highlight-symbol
        multi-term
        flycheck
        smex
        ))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path
             (expand-file-name "themes" user-emacs-directory))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq inhibit-startup-message t)

(setq magit-last-seen-setup-instructions "1.4.0")

;;(setq omnisharp-server-executable-path "~/omnisharp-roslyn/scripts/Omnisharp.cmd")
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(if (display-graphic-p)
    (set-face-attribute 'default nil
                    :height 90
                    :font (if (eq system-type 'windows-nt) "Consolas" "Source Code Pro")))

(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)

  (global-git-gutter-mode +1)

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
  (load-theme 'base16-default-dark t))

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
  (global-set-key (kbd "C-h")         'buf-move-left)
  (global-set-key (kbd "C-l")         'buf-move-right)
  (global-set-key (kbd "C-j")         'buf-move-down)
  (global-set-key (kbd "C-k")         'buf-move-up))

(defun my-smex-mode ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun my-tern-mode ()
  (tern-mode t))
(add-hook 'js-mode-hook 'my-tern-mode)

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
  
;; Typescript stuff
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(require 'tss)

;; Key binding
;(setq tss-popup-help-key "C-:")
;(setq tss-jump-to-definition-key "C->")
;(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
(tss-config-default)

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

(add-hook 'after-init-hook 'my-after-init-hook)
(add-hook 'csharp-mode-hook 'my-csharp-hooks)

(require 'server)
(unless (server-running-p) (server-start))
