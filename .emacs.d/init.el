
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(beacon-color "#f2777a")
 '(company-ghc-show-info t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("8cb818e0658f6cc59928a8f2b2917adc36d882267bf816994e00c5b8fcbf6933" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "a914fce8ef585e826ea136da5478bc929a3a42161cd478e4f4ff6df00d8da1c3" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "7f3ef7724515515443f961ef87fee655750512473b1f5bf890e2dc7e065f240c" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "dc9a8d70c4f94a28aafc7833f8d05667601968e6c9bf998791c39fcb3e4679c9" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" default)))
 '(elpy-rpc-python-command "/home/namo/anaconda2/bin/python")
 '(fci-rule-color "#073642")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-capture-bookmark nil)
 '(org-capture-templates
   (quote
    (("p" "Personal")
     ("pt" "TODO list" entry
      (file+headline "~/Dropbox/personal.org" "Capture")
      "\"**\"" :prepend t)
     ("w" "Work")
     ("wt" "TODO list" entry
      (file+headline "~/Dropbox/work.org" "Capture")
      (file "~/Dropbox/todo.txt")
      :prepend t))))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "okular %s"))))
 '(org-fontify-emphasized-text t)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-hide-emphasis-markers t)
 '(org-highlight-latex-and-related (quote (latex)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))
 '(org-refile-use-outline-path t)
 '(org-reverse-note-order t)
 '(org-tags-column 90)
 '(package-selected-packages
   (quote
    (markdown-mode company-jedi company-anaconda anaconda-mode solarized-theme vimish-fold plan9-theme ein company-auctex auto-complete-auctex sunburn-theme apropospriate-theme auto-complete matlab-mode elpy sicp racket-mode auctex slime intero magit company-ghc hindent ghc haskell-mode pdf-tools color-theme-sanityinc-tomorrow gruvbox-theme color-theme-sanityinc-solarized hc-zenburn-theme zenburn-theme linum-relative org-edna)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-shell-interpreter "/home/namo/anaconda2/bin/ipython" t)
 '(server-mode t)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(tabbar-background-color "#ffffff")
 '(temp-buffer-resize-mode t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(window-combination-resize t)
 '(winner-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)

;;evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'haskell-interactive-mode 'emacs)
(evil-set-initial-state 'slime-mode 'emacs)
(evil-set-initial-state 'racket-repl-mode 'emacs)
(evil-set-initial-state 'ein:notebooklist-mode 'emacs)
;;(define-key evil-normal-state-map "\C-r" 'undo-tree-redo)



;;keybindings
(intero-global-mode 1)

;;autocompletion
(require 'company)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'c-mode-common-hook 'company-mode)

;;evil haskell fix
(defun haskell-evil-open-above ()
  (interactive)
  (evil-digit-argument-or-evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun haskell-evil-open-below ()
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))

(evil-define-key 'normal haskell-mode-map "o" 'haskell-evil-open-below
  "O" 'haskell-evil-open-above)

;;Common Lisp
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(set-default 'tab-always-indent 'complete)
(show-paren-mode 1)

;;keybindings
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f9>") (kbd "C-u - 3 C-x ^"))
(global-set-key (kbd "<f8>") (kbd "C-- - 3 C-x ^"))
(global-set-key (kbd "M-g") 'magit-status)
(global-set-key (kbd "M-a") 'align-regexp)
(global-set-key (kbd "M-[") (kbd "C-u - 3 C-x ^"))
(global-set-key (kbd "M-]") (kbd "C-- - 3 C-x ^"))

;;global settings
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)


(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;(setq org-log-done t)

(setq org-hide-emphasis-markers t)
(setq org-agenda-files (list "~/Dropbox/work.org"
			     "~/Dropbox/personal.org"))



;;increase LaTeX size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq TeX-PDF-mode t)

;;diary location
(setq diary-file "~/Dropbox/diary")

;;fancy display
(setq view-diary-entries-initially t
       mark-diary-entries-in-calendar t
       number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)


(setq org-agenda-include-diary t)
;(setq calendar-mark-diary-entries-flag t)

(defun kill-to-end-of-buffer() "Deletes all lines after the current line"
  (interactive)
  (progn
    (forward-line 1)
    (delete-region (point) (point-max))))

;; google cal
(setq mark-diary-entries-in-calendar t)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(defun getcal (url)
  "Download ics file and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile "~/Dropbox/diary" t)
    (kill-buffer (car (last (split-string tmpfile "/"))))
    )
  )
(load "~/Dropbox/gcal.el")
(defun getcals ()
  (interactive)
  (find-file "~/Dropbox/diary")
  (flush-lines "^.")
  (dolist (url google-calendars) (getcal url))
  (beginning-of-buffer)
  (replace-string "&" "")
  (save-buffer)
  (kill-buffer "diary"))

;;longer agenda
(setq org-agenda-span 10
      org-agenda-start-day "-3d")

;;line numbers in appropriate buffers
(require 'linum)
(require 'linum-relative)
(setq linum-disabled-modes-list '(speedbar-mode
				  org-mode
				  compilation-mode
				  dired-mode
				  doc-view-mode
				  pdf-virtual-view-mode))
;;overwrite default linum-on
(defun linum-on ()
  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list)
	      (and (not (eq (buffer-name) "*scratch*"))
		   (string-match "*" (buffer-name))))
    (linum-mode t)))
(global-linum-mode t)
(linum-relative-toggle)
;;(setq linum-eager nil)
;;(add-hook 'prog-mode-hook 'linum-on) ;;prevents pdf from freezing

;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 149 :width normal :foundry "PfEd" :family "Inconsolata for Powerline"))))
;; '(font-lock-comment-face ((t (:foreground "#6c71c4"))))
;; '(org-agenda-date-today ((t (:inherit org-agenda-date :background "#268bd2" :foreground "#002b36" :inverse-video t :overline nil :weight bold))))
;; '(org-verbatim ((t (:foreground "#2AA198" :height 130 :width normal :foundry "PfEd" :family "Courier Prime Code"))))
;; '(slime-repl-inputed-output-face ((t (:foreground "#b58900")))))

;org-mode capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;winner shortcuts
(global-set-key [f4] 'winner-undo)
(global-set-key [f5] 'winner-redo)

;; matlab mode
(add-hook 'matlab-mode
          (lambda ()
            (auto-complete-mode 1)))

;; python

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")



;;prettifier
;;(defun my-add-pretty-lambda ()
;;  "convert stuff to unicode"
;;  (setq prettify-symbols-alist '(("lambda" . 955))))
;;(add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
;; resize split
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(defun halve-current-window-height ()
  "Shrink current window to half"
  (interactive)
  (shrink-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "<f7>") 'halve-other-window-height)
(global-set-key (kbd "<f6>") 'halve-current-window-height)


;; solarized
(setq solarized-distinct-fringe-background t)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; Use less bolding
(setq solarized-use-less-bold t)

;; Use more italics
(setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

(load-theme 'solarized-light t)

;; Fonts
(set-face-attribute 'default nil :font "Droid Sans Mono for Powerline-13")

(global-auto-revert-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
