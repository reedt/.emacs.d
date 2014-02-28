;; ----------------------------------------------------------------------------
;; .clang_complete reading
;; ----------------------------------------------------------------------------

(defun read-c-flags ()
  "list of flags from upward-found .clang_complete file, nil if not found"

  (defun upward-find-file (filename &optional startdir)
    (let ((dirname (expand-file-name (if startdir startdir ".")))
          (found nil)
          (top nil))
      (while (not (or found top))
             (if (string= (expand-file-name dirname) "/") (setq top t))
             (if (file-exists-p (expand-file-name filename dirname))
               (setq found t)
               (setq dirname (expand-file-name ".." dirname))))
      (if found (concat dirname "/") nil)))

  (defun read-lines (path)
    (with-temp-buffer
      (insert-file-contents path)
      (split-string (buffer-string) "\n" t)))

  (let ((path (upward-find-file ".clang_complete")))
    (if path (read-lines (concat path ".clang_complete")) nil)))


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


;; --- color-theme-sanityinc-tomorrow -----------------------------------------

(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)


;; --- evil -------------------------------------------------------------------

(setq evil-want-C-u-scroll t)

(require-package 'evil)
(evil-mode 1)


;; --- evil-nerd-commenter ----------------------------------------------------

(require-package 'evil-nerd-commenter)
(define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
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


;; --- projectile -------------------------------------------------------------

(require-package 'projectile)
(projectile-global-mode)


;; --- multi-term -------------------------------------------------------------

(require-package 'multi-term)
(setq multi-term-program "/bin/bash")
(setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-y" "<ESC>"
                             "C-h" "C-l" "C-k" "C-j"))


;; --- project-explorer -------------------------------------------------------

(require-package 'project-explorer)
(add-to-list 'evil-emacs-state-modes 'project-explorer-mode)
(setq pe/width 23)


;; --- git-gutter-fringe ------------------------------------------------------

(require-package 'git-gutter-fringe)
(global-git-gutter-mode t)


;; --- yasnippet --------------------------------------------------------------

(require-package 'yasnippet)
(yas-global-mode 1)


;; --- company-mode -----------------------------------------------------------

(require-package 'company)
(defun my-company-c-config ()
 (setq company-clang-arguments (read-c-flags)))
(add-hook 'c-mode-common-hook 'my-company-c-config)

(global-company-mode t)
(setq company-idle-delay 0.2)


;; --- flycheck ---------------------------------------------------------------

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; c
(defun my-flycheck-c-config ()
  (defun read-c-includes ()
    (defun include-path-flag-p (s)
      (cond ((>= (length s) (length "-I"))
             (string-equal (substring s 0 (length "-I")) "-I"))
            (t nil)))
    (mapcar #'(lambda (s) (substring s 2))
            (remove-if-not 'include-path-flag-p (read-c-flags))))
  (setq flycheck-clang-include-path (read-c-includes)))
(add-hook 'c-mode-common-hook 'my-flycheck-c-config)


;; --- perspective ------------------------------------------------------------

(require-package 'perspective)
(add-hook 'after-init-hook #'(lambda () (persp-mode 1)))


;; --- magit ------------------------------------------------------------------

(require-package 'magit)
;(require-package 'magit-filenotify)


;; --- haskell-mode -----------------------------------------------------------

(require-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))


;; --- w3m --------------------------------------------------------------------

(require-package 'w3m)
(setq browse-url-browser-function 'w3m-goto-url-new-session)
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
(setq w3m-default-display-inline-images t)
(add-hook 'w3m-mode-hook (lambda () (evil-normal-state)))

(defun reddit (reddit)
  "Opens the REDDIT in w3m-new-session"
  (interactive (list
                 (read-string "Enter the reddit (default: gamedev): " nil nil "gamedev" nil)))
  (browse-url (format "http://m.reddit.com/r/%s" reddit))
  )

(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
    (let ((term (if mark-active
                  (buffer-substring (region-beginning) (region-end))
                  (word-at-point))))
      (list
        (read-string
          (format "Wikipedia (%s):" term) nil nil term)))
    )
  (browse-url
    (concat
      "http://en.m.wikipedia.org/w/index.php?search="
      search-term
      ))
  )


;; --- switch-window ----------------------------------------------------------

(require-package 'switch-window)
(global-set-key (kbd "C-9") 'switch-window)
(global-set-key (kbd "C-;") 'switch-window)


;; --- win-switch -------------------------------------------------------------

(require-package 'win-switch)
(setq win-switch-window-threshold 0)
(setq win-switch-other-window-first nil)
(setq win-switch-idle-time 5)

;; move
(win-switch-set-keys '("h") 'left)
(win-switch-set-keys '("k") 'up)
(win-switch-set-keys '("j") 'down)
(win-switch-set-keys '("l") 'right)
(win-switch-set-keys '("n") 'next-window)
(win-switch-set-keys '("p") 'previous-window)
(win-switch-set-keys '("O") 'other-frame)

;; resize
(win-switch-set-keys '("K") 'enlarge-vertically)
(win-switch-set-keys '("J") 'shrink-vertically)
(win-switch-set-keys '("H") 'enlarge-horizontally)
(win-switch-set-keys '("L") 'shrink-horizontally)

;; modify
(win-switch-set-keys '("v") 'split-horizontally)
(win-switch-set-keys '("s") 'split-vertically)
(win-switch-set-keys '("d") 'delete-window)

(win-switch-set-keys '("\M-\C-g") 'emergency-exit)

(global-set-key (kbd "C-'") 'win-switch-dispatch)


;; --- buffer-move ------------------------------------------------------------

(require-package 'buffer-move)
(global-set-key (kbd "M-h") 'buf-move-left)
(global-set-key (kbd "M-l") 'buf-move-right)
(global-set-key (kbd "M-k") 'buf-move-up)
(global-set-key (kbd "M-j") 'buf-move-down)


;; --- dired-details+ ---------------------------------------------------------

(require-package 'dired-details+)


;; --- auctex -----------------------------------------------------------------

(require 'tex)
(require 'preview)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(TeX-global-PDF-mode t)
(setq-default TeX-master nil)

(require 'tex-site)
(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-output-view-style
                         '("^pdf$" "."
                           "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))


;; ----------------------------------------------------------------------------
;; languages
;; ----------------------------------------------------------------------------

(setq lua-indent-level 4)
(require-package 'lua-mode)

(require-package 'cmake-mode)


;; ----------------------------------------------------------------------------
;; manual
;; ----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; --- gud (with lldb) --------------------------------------------------------
(require 'gud)


;; ----------------------------------------------------------------------------
;; interface
;; ----------------------------------------------------------------------------

;; don't use mac fullscreen
(setq ns-use-native-fullscreen nil)

;; cleanup
(menu-bar-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))

;; initial frame size
(when window-system (set-frame-size (selected-frame) 141 53))

;; font
(defvar *default-font*
  "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
(when window-system (set-face-font 'default *default-font*))

;; gdb
(setq gdb-many-windows t)

;; dired
(setq-default dired-listing-switches "-alhv")

;; uniquify
(require 'uniquify)


;; ----------------------------------------------------------------------------
;; keys
;; ----------------------------------------------------------------------------

;; find
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map " " 'projectile-find-file)

;; apps
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(define-key evil-normal-state-map ",t" 'eshell)
(define-key evil-normal-state-map ",p" 'project-explorer-open)

;; persp
(define-key evil-normal-state-map ";w" 'persp-switch)
(define-key evil-normal-state-map ";r" 'persp-rename)
(define-key evil-normal-state-map ";k" 'persp-kill)
(define-key evil-normal-state-map ",a" 'persp-add-buffer)
(define-key evil-normal-state-map ",i" 'persp-import)
(define-key evil-normal-state-map ",k" 'persp-remove-buffer)

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

;; save-load
(defun cgame-scratch () (interactive) (write-file "scratch.lua"))
(defun cgame-scratch-region ()
  (interactive)
  (write-region (region-beginning) (region-end) "scratch.lua"))
(define-key evil-normal-state-map ",c" 'cgame-scratch-region)
(define-key evil-visual-state-map ",c" 'cgame-scratch-region)


;; ----------------------------------------------------------------------------
;; formatting
;; ----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)

;; c
(require 'cc-mode)
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'case-label '+)
(define-key c-mode-base-map (kbd "RET") 'c-indent-new-comment-line)


;; ----------------------------------------------------------------------------
;; eshell
;; ----------------------------------------------------------------------------

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


