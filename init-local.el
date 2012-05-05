; Set TAB width to 4
(setq tab-width 4)
; Set default indent width
(setq standard-indent 4)

; Use spaces instead of tabs for indention
(setq-default indent-tabs-mode nil)
; Place backup of all files to the specified directory
(setq backup-directory-alist (quote ((".*" . "~/tmp/.emacs_backups"))))
; Auto break lines when they become too wide
(setq auto-fill-mode 1)

(setq column-number-mode t)
(setq default-fill-column 70)
; Set frame title
(setq frame-title-format "emacs@%b")
; Turn on GUI menu bar
(menu-bar-mode 1)
; Turn off GUI tool bar
(tool-bar-mode 0)
; Copy to clipboard
(setq x-select-enable-clipboard t)
; Disable the welcome page
(setq inhibit-splash-screen t)

; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
; Turn on ido mode
(ido-mode t)
; Highlight current line globally
(global-hl-line-mode t)

; Enable ibus client
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

; Make F11 key toggle full-screen state
(global-set-key [f11] 'toggle-fullscreen)
;; Make new frames fullscreen by default. Note: this hook doesn't do
;; anything to the initial frame if it's in your .emacs, since that file is
;; read _after_ the initial frame is created.
(add-hook 'after-make-frame-functions 'toggle-fullscreen)

; Compile display color remove ^[[0m
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

; Org mode settings
(add-to-list 'load-path "~/mydata/conf/elisp/org/lisp/")
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
; Default directory to look up Org files.
(setq org-directory "~/mydata/org/")
; Record time when todo is done
(setq org-log-done 'time)
(setq org-agenda-files '("~/mydata/org/work.org"))
; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
; Show source block syntax highlighting
(setq org-src-fontify-natively t)
; Use current window for agenda
(setq org-agenda-window-setup 'current-window)
(require 'org-latex)
(setq org-export-latex-listings t)
(add-to-list 'org-export-latex-classes
             '("org-article"
               "\\documentclass{org-article}
                 [NO-DEFAULT-PACKAGES]
                 [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(define-skeleton org-beamer-skeleton
  "Insert org beamer header"
  ""
  "#+startup: beamer
#+LaTeX_CLASS: beamer
#+LaTeX_CLASS_OPTIONS: [bigger]
#+BEAMER_FRAME_LEVEL: 2
#+COLUMNS: %40ITEM %10BEAMER_env(Env) %9BEAMER_envargs(Env Args) %4BEAMER_col(Col) %10BEAMER_extra(Extra)")
(setq org-latex-to-pdf-process '("xelatex %f"))

; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

; Use M-/ to expand abbrevs
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

; Use C-. and C-, to quickly mark position and jump to it
(global-set-key [(control ?\.)] 'ska-point-to-register)
(global-set-key [(control ?\,)] 'ska-jump-to-register)

(provide 'init-local)

