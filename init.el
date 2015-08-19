;; set exec-path from $PATH
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
	 (replace-regexp-in-string
	  "[[:space:]\n]*$" ""
	  (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)


;; ----------------------------------------------------------------------------
;; .clang_complete reading
;; ----------------------------------------------------------------------------

;; (defun read-c-flags ()
;;   "list of flags from upward-found .clang_complete file, nil if not found"

;;   (defun upward-find-file (filename &optional startdir)
;;     (let ((dirname (expand-file-name (if startdir startdir ".")))
;;           (found nil)
;;           (top nil))
;;       (while (not (or found top))
;;              (if (string= (expand-file-name dirname) "/") (setq top t))
;;              (if (file-exists-p (expand-file-name filename dirname))
;;                (setq found t)
;;                (setq dirname (expand-file-name ".." dirname))))
;;       (if found (concat dirname "/") nil)))

;;   (defun read-lines (path)
  ;;   (with-temp-buffer
  ;;     (insert-file-contents path)
  ;;     (split-string (buffer-string) "\n" t)))

  ;; (let ((path (upward-find-file ".clang_complete")))
  ;;   (if path (read-lines (concat path ".clang_complete")) nil)))


;; ----------------------------------------------------------------------------
;; packages
;; ----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun require-package (package)
  "same as require, but installs package if needed"
  (progn
    (unless (package-installed-p package)
      (unless (assoc package package-archive-contents)
        (package-refresh-contents))
      (package-install package))
    (require package)))


;; --- theme ------------------------------------------------------------------

(require-package 'monokai-theme)
(load-theme 'monokai t)


;; --- org ---------------------------------------------------------------------

(require-package 'org)


;; --- evil -------------------------------------------------------------------

; compatibility with org mode (TAB conflict)
;; (setq evil-want-C-i-jump nil)

(setq evil-want-C-u-scroll t)

(require-package 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(setq-default evil-cross-lines t)


;; --- evil-jumper --------------------------------------------------------------
(require-package 'evil-jumper)
(add-hook 'after-init-hook #'(lambda () (evil-jumper-mode 1)))

;; --- powerline --------------------------------------------------------------

(require-package 'powerline)
(powerline-default-theme)


; ;; --- diminish ---------------------------------------------------------------
;
; (require-package 'diminish)
; (when (display-graphic-p)
;   (eval-after-load "magit"
;                    '(diminish 'magit-auto-revert-mode))
;   (eval-after-load "git-gutter"
;                    '(diminish 'git-gutter-mode))
;   (eval-after-load "undo-tree"
;                    '(diminish 'undo-tree-mode))
;   (eval-after-load "abbrev"
;                    '(diminish 'abbrev-mode))
;   (eval-after-load "auto-complete"
;                    '(diminish 'auto-complete-mode " ac"))
;   (eval-after-load "flycheck"
;                    '(diminish 'flycheck-mode " fly"))
;   (eval-after-load "projectile"
;                    '(diminish 'projectile-mode " pr"))
;   (eval-after-load "flyspell"
;                    '(diminish 'flyspell-mode " flysp"))
;   (eval-after-load "yasnippet"
;                    '(diminish 'yas-minor-mode " yas")))
;
;
;; --- evil-nerd-commenter ----------------------------------------------------

(require 'cl)
(require-package 'evil-nerd-commenter)
(define-key evil-normal-state-map ",cl" 'evilnc-comment-or-uncomment-to-the-line)
(define-key evil-normal-state-map ",cc" 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
(define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)


;; --- flx-ido ----------------------------------------------------------------

(require-package 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)


;; --- ido-ubiquitos ----------------------------------------------------------

(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode)


;; --- smex -------------------------------------------------------------------

(require-package 'smex)
(smex-initialize)


; ;; --- projectile -------------------------------------------------------------
;
; (require-package 'projectile)
; (projectile-global-mode)
; (setq projectile-enable-caching t)
;
;
; ;; --- project-explorer -------------------------------------------------------
;
; (require-package 'project-explorer)
; (add-to-list 'evil-emacs-state-modes 'project-explorer-mode)
; (setq pe/width 23)


; ;; --- flycheck ---------------------------------------------------------------
;
; (require-package 'flycheck)
; (add-hook 'after-init-hook #'global-flycheck-mode)
;
; ;; c
; (defun my-flycheck-c-config ()
;   (defun read-c-includes ()
;     (defun include-path-flag-p (s)
;       (cond ((>= (length s) (length "-I"))
;              (string-equal (substring s 0 (length "-I")) "-I"))
;             (t nil)))
;     (mapcar #'(lambda (s) (substring s 2))
;             (remove-if-not 'include-path-flag-p (read-c-flags))))
;   (setq flycheck-clang-include-path (read-c-includes)))
; (add-hook 'c-mode-common-hook 'my-flycheck-c-config)
;
; (add-hook 'c++-mode-hook (lambda ()
;                            (setq flycheck-clang-language-standard "c++11")))
;
;
;; --- perspective ------------------------------------------------------------

(require-package 'perspective)

(add-hook 'after-init-hook #'(lambda () (persp-mode 1)))

(define-key evil-normal-state-map ";w" 'persp-switch)
(define-key evil-normal-state-map ";r" 'persp-rename)
(define-key evil-normal-state-map ";k" 'persp-kill)
(define-key evil-normal-state-map ",a" 'persp-add-buffer)
(define-key evil-normal-state-map ",i" 'persp-import)
(define-key evil-normal-state-map ",k" 'persp-remove-buffer)


; ;; --- magit ------------------------------------------------------------------
;
; (require-package 'magit)
; (define-key evil-normal-state-map ",gs" 'magit-status)
; (define-key evil-normal-state-map ",gl" 'magit-log)
;
;
; ;; --- diff-hl ----------------------------------------------------------------
;
; (require-package 'diff-hl)
; (global-diff-hl-mode)
;
;
; ;; --- pcmpl-git --------------------------------------------------------------
;
; (require-package 'pcmpl-git)
;
;
; ;; --- window-numbering -------------------------------------------------------
;
; (require-package 'window-numbering)
; (window-numbering-mode)
;
;
;; --- buffer-move ------------------------------------------------------------

(require-package 'buffer-move)
(global-set-key (kbd "M-h") 'buf-move-left)
(global-set-key (kbd "M-l") 'buf-move-right)
(global-set-key (kbd "M-k") 'buf-move-up)
(global-set-key (kbd "M-j") 'buf-move-down)


; ;; --- dired-details+ ---------------------------------------------------------
;
; (require-package 'dired-details+)
;
;
; ;; --- dired-subtree ----------------------------------------------------------
;
; (require-package 'dired-subtree)
;
;
; ;; --- maxframe ---------------------------------------------------------------
;
; (require-package 'maxframe)
; (add-hook 'window-setup-hook 'maximize-frame t)
;
;
;; ----------------------------------------------------------------------------
;; manual
;; ----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")

; ;; --- gud (with lldb) --------------------------------------------------------
; (require 'gud)
;
; ;; --- magit config -----------------------------------------------------------
; (require 'my-magit)
;
;
;; ----------------------------------------------------------------------------
;; interface
;; ----------------------------------------------------------------------------

; ;; reduce startup flicker?
; (setq redisplay-dont-pause t)
;
;; don't save backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; paren matching
(show-paren-mode 1)

;; disable menu bar
(menu-bar-mode 0)

; ;; cleanup
; (tool-bar-mode -1)
; (scroll-bar-mode -1)
; (set-default 'truncate-lines t)
; (setq initial-scratch-message "")
; (setq inhibit-startup-message t)
; (defalias 'yes-or-no-p 'y-or-n-p)

;; scrolling
(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(0.01))
(defun disable-scroll-margin ()
  (set (make-local-variable 'scroll-margin) 0))
(add-hook 'shell-mode-hook 'disable-scroll-margin)
(add-hook 'eshell-mode-hook 'disable-scroll-margin)
(add-hook 'gud-mode-hook 'disable-scroll-margin)
(add-hook 'magit-mode-hook 'disable-scroll-margin)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

; ;; gdb
; (setq gdb-many-windows t)
;
; ;; dired
; (setq-default dired-listing-switches "-alhv")
;
;; uniquify
(require 'uniquify)

; ;; continuous scroll for doc-view
; (setq doc-view-continuous t)
;
; ;; allow undo of window config
; (winner-mode 1)
;
; ;; disable popup dialogs
; (defadvice yes-or-no-p (around prevent-dialog activate)
;   "Prevent yes-or-no-p from activating a dialog"
;   (let ((use-dialog-box nil))
;     ad-do-it))
; (defadvice y-or-n-p (around prevent-dialog-yorn activate)
;   "Prevent y-or-n-p from activating a dialog"
;   (let ((use-dialog-box nil))
;     ad-do-it))

;; reload changed files
(global-auto-revert-mode 1)


;; ----------------------------------------------------------------------------
;; hideshow
;; ----------------------------------------------------------------------------

(global-set-key (kbd "<backtab>") 'hs-toggle-hiding)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)


;; ----------------------------------------------------------------------------
;; keys
;; ----------------------------------------------------------------------------

; separate <tab> and "C-i"
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(global-set-key (kbd "C-i") 'forward-word)

;; ;; find
;; (define-key evil-normal-state-map ",f" 'projectile-find-file)
;; (define-key evil-normal-state-map " " 'projectile-find-file)

;; apps
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (define-key evil-normal-state-map ",t" 'eshell)
;; (define-key evil-normal-state-map ",p" 'project-explorer-open)

;; splits
(define-key evil-normal-state-map ",s" 'split-window-below)
(define-key evil-normal-state-map ",v" 'split-window-right)
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))
(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k")    'windmove-up)
(global-set-key (kbd "C-j")  'windmove-down)
(define-key evil-normal-state-map ",q" 'delete-window)

;; buffers
(define-key evil-normal-state-map "\C-p" nil)
(global-set-key (kbd "C-p") 'previous-buffer)
(define-key evil-normal-state-map "\C-n" nil)
(global-set-key (kbd "C-n") 'next-buffer)


;; --- linum-mode --------------------------------------------------------------

(global-linum-mode 1)
(define-key evil-normal-state-map "\C-q" nil)
(global-set-key (kbd "C-q") 'linum-mode)


;; ----------------------------------------------------------------------------
;; formatting
;; ----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq tab-width 8)

;; c
(require 'cc-mode)
(setq c-default-style "bsd" c-basic-offset 8)
(c-set-offset 'case-label '+)
(define-key c-mode-base-map (kbd "RET") 'c-indent-new-comment-line)

; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


; ;; ----------------------------------------------------------------------------
; ;; eshell
; ;; ----------------------------------------------------------------------------
;
; (defun eshell/clear ()
;   (interactive)
;   (let ((inhibit-read-only t))
;     (erase-buffer)))
;
; (add-hook 'eshell-mode-hook
;           '(lambda () (setenv "TERM" "eterm-color")))
; (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)
;
;
;; --- column-marker ------------------------------------------------------------

(require-package 'column-marker)
(add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 100)))


;; --- ace-jump ---------------------------------------------------------------

(require-package 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; --- yasnippet --------------------------------------------------------------

(require-package 'yasnippet)
(yas-global-mode 1)

(setq yas-keymap (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "C-o") 'yas-next-field-or-maybe-expand)
                   (define-key map (kbd "C-i") 'yas-prev-field)
                   (define-key map (kbd "C-g") 'yas-abort-snippet)
                   (define-key map (kbd "C-d") 'yas-skip-and-clear-or-delete-char)
                   map))


;; --- company ------------------------------------------------------------

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 0)
(setq company-idle-delay 0)


;; --- irony ----------------------------------------------------------

(require-package 'irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; --- company-irony ----------------------------------------------------------

(require-package 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
