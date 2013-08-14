; Change color theme
(load-theme 'sanityinc-solarized-dark t)
; Set TAB width to 4
(setq tab-width 4)
; Set default indent width
(setq standard-indent 4)

; Use spaces instead of tabs for indention
(setq-default indent-tabs-mode nil)
; Auto break lines when they become too wide
(setq auto-fill-mode 1)

(setq column-number-mode t)
(setq fill-column 70)
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
;(require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
;(add-hook 'after-init-hook 'ibus-mode-on)
;; Choose your key to toggle input status:
;;(global-set-key (kbd "C-\\") 'ibus-toggle)
;; Archlinux uses Python 3, so add this
;(custom-set-variables '(ibus-python-shell-command-name "python2"))

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

;; Make Ctrl-j eval and print last s-exp
(global-set-key "\C-j" 'eval-print-last-sexp)

; Compile display color remove ^[[0m
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Use M-/ to expand abbrevs
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

;; Make AUCTeX aware of style files and multi-file documents right away
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; Use XeTeX as the default TeX engine
(setq TeX-engine 'xetex)
;; Generate PDF instead of DVI
(setq TeX-PDF-mode t)

;; Turn on RefTeX automatically
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; Stop creating those backup~ files
(setq make-backup-files nil)
;; Stop creating those #autosave# files
(setq auto-save-default nil)

;; org2blog settings
;; Wordpress 需要安装 SyntaxHiglighter Evolved 插件才能正常进行语法高亮
(setq org2blog/wp-blog-alist
      '(("混沌猫的窝"
         :url "http://chaoslawful.info/xmlrpc.php"
         :username "chaoslawful"
         :default-title ""
         :default-categories ("Misc")
         :tags-as-categories nil
         )
        ))
;; org2blog/wp-use-wp-latex 为 t 时需要安装 WP Latex 插件才能显示 LaTeX 片段
;; 但其默认用 Wordpress 提供的转换服务生成 LaTeX 片段对应图片，在国内因墙的原因
;; 无法正常显示，故推荐关闭该选项并使用其他能支持裸 LaTeX 片段的基于 MathJax
;; 显示的插件，如 LaTeX for Wordpress 等
(setq org2blog/wp-use-wp-latex nil)

;; 避免 Evil 模式光标总是黑色
(setq evil-default-cursor t)

(require 'my-org-settings)

(provide 'init-local)
