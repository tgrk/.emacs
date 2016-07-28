;; ===== Customizations go in emacs.d =====
(add-to-list 'load-path "~/.emacs.d")

;; ===== Disable toolbar =====
(tool-bar-mode -1)

;; ===== Default to better frame titles =====
(setq frame-title-format (concat  "%b - emacs@" system-name))

;; ===== Disable system beep =====
(setq visual-bell t)

;; ===== Set default language =====
(setq current-language-environment "UTF-8")
(define-coding-system-alias 'UTF-8 'utf-8)

;; ===== Set the highlight current line minor mode =====
(global-hl-line-mode 1)

;; ===== Support Wheel Mouse Scrolling =====
(mouse-wheel-mode t)

;; ===== Prevent Emacs from making backup files =====
(setq make-backup-files nil)

;; ===== Nyan Mode =====
(add-to-list 'load-path "/home/wiso/.emacs.d/nyan-mode/")
(require 'nyan-mode)
(setq nyan-wavy-tail t)
(setq nyan-bar-length 40)
(setq nyan-animate-nyancat t)
(nyan-mode)

;; ===== Enable on the fly spelll checking =====
;;(defun turn-on-flyspell () (flyspell-mode 1))
;;(add-hook 'find-file-hooks 'turn-on-flyspell)

;; =====  Confirm on exit :-) =====
(setq confirm-kill-emacs 'y-or-n-p)

;; ===== Enable Line and Column Numbering =====
(line-number-mode 1)
(column-number-mode 1)

;; ===== Disable *GNU Emacs* startup buffer =====
(setq inhibit-start-screen 1)
(setq inhibit-splash-screen 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message 0)

;; ===== Set colours =====
(set-cursor-color "white")
(set-mouse-color "goldenrod")
(set-face-background 'region "gray")
(set-background-color "black")
(set-foreground-color "white")
;;(set-face-font 'default '"8x13")
(set-default-font "Inconsolata-9")

;; ===== C-x,C-c,C-v copy/paste =====
(cua-mode 1)

;; ===== Delete seleted text when typing =====
(delete-selection-mode 1)

;; ===== Turn on parent match highlighting =====
(show-paren-mode 1)

;; ===== Show line number the cursor is on, in status bar (the mode line) =====
(line-number-mode 1)

;; ===== Always show line numbers =====
(global-linum-mode 1)

(column-number-mode 1)

(global-visual-line-mode 1)

;; ===== Copy/paste with X =====
(setq x-select-enable-clipboard t)

;; ===== Shortening yes=y, no=n =====
(defalias 'yes-or-no-p 'y-or-n-p)

; ===== Autocompletition on buffers/file names =====
;;(ido-mode 1)

;; ===== Cycle through buffers with Ctrl-Tab (like Firefox) =====
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; ===== Goto-line short-cut key =====
(global-set-key "\C-l" 'goto-line)

;; ===== Editing .emacs config =====
(global-set-key (kbd "<f12>") ;
  (lambda()(interactive)(find-file "~/.emacs")))

;; =====  Run terminal =====
(global-set-key (kbd "<f2>") ;
  (lambda()(interactive)(ansi-term "/bin/bash")))

;; ===== Buffer navigation =====
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

;; ===== Interactively Do Things (smart tab-completion in find file etc.) =====
(require 'ido)
(ido-mode t)

;; ===== Fixed line length =====
(setq whitespace-line-column 80)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;;FIXME: this is not working
;; ===== Highlight TODO/FIXME/BUG keywords in Erlang comments ====
;;(set-face-underline 'font-lock-warning-face "yellow")
;;(add-hook 'c-mode-common-hook
;;               (lambda ()
;;                (font-lock-add-keywords nil
;;                 '(("<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; ===== Set key bindings =====
(global-set-key [f3] 'shell)
(global-set-key [f5] 'query-replace)
(global-set-key [f6] 'switch-to-buffer)
(global-set-key [f7] 'hippie-expand)
(global-set-key [f9] 'ispell)
(global-set-key [(control o)] 'find-file)

;; ===== Alt buffer navigation =====
(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer window

;; ===== Save/restore buffers =====
(desktop-save-mode 0)

;; ===== First define a variable which will store the previous column position =====
(defvar previous-column nil "Save the column position")


;; ===== Split window into N horizontal buffers =====
;; TODO: check number of buffers before increasing it by 3
(defun create-vertical-buffers (screens)
  "Split window into N vertical buffers"
  (interactive)
  (setq buff-number 1)
  ;;(length (buffer-list)
  (while (< buff-number screens)
    (split-window-horizontally)
    (setq buff-number (1+ buff-number)))
  (balance-windows))

;; ===== On bigger display automatically create 3 buffers otherwise 2 =====
(if (< 1900 (x-display-pixel-width) )
    (funcall 'create-vertical-buffers 3)
  (funcall 'create-vertical-buffers 2) )

;; ===== Key bindings for tree vertical buffers =====
(global-set-key (kbd "C-x 4") (lambda () (interactive) (create-vertical-buffers 3)))

;; ===== Delete any trailing whitespace before saving =====
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'package)
;; ==================================================================================
;; PACKAGES
;; ==================================================================================
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; ==================================================================================
;; LANGUAGE SPECIFIC SETTINGS
;; ==================================================================================

;; ===== ActionScript mode =====
;;(load-file "~/.emacs.d/actionscript-mode.el")
;;(autoload 'actionscript-mode "javascript" nil t)
;;(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;;===== Flex specific =====
;;(setq auto-mode-alist (append (list
;; '("\\.as\\'"   . actionscript-mode)
;; '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|mxml\\)\\'" . nxml-mode)
;; ;; add more modes here
;; ) auto-mode-alist))

;; ===== Magic for XML Mode =====
(setq nxml-mode-hook
    '(lambda ()
 (setq tab-width        2
       indent-tabs-mode nil)
       (set-variable 'nxml-child-indent     2)
       (set-variable 'nxml-attribute-indent 2)
       ))

;; ===== GO Mode =====
(setenv "GOPATH" "/home/wiso/Projects/personal/golang/")
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode-load)

;; ===== GO Mode - godef + gofmt =====
(defun my-go-mode-hook ()
  ; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Use goimports instead of go-fmt
  ;(setq gofmt-command "goimports") ;NOTE: unable to find goimports!
  ; Compile
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Go oracle
  ;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  ; godef jump key binding
  (local-set-key (kbd 2"M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; ===== GO Mode - godoc =====
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; ===== GO Mode - autocomplete =====
;;NOTE: this is not working!
;(eval-after-load 'go-mode
;   (require 'go-autocomplete))
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

;; ===== Rust Mode =====
;(require 'rust-mode)

;; ===== Erlang Emacs Mode -- Configuration End =====
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; ===== EDTS =====
(add-to-list 'load-path "~/Projects/libs/edts")
  (require 'edts-start)

;; ===== Disable popup due to buffer limit in EDTS =====
;(add-to-list 'warning-suppress-types '(undo discard-info))

;; ===== Apache Pig Latin =====
;(load-file "~/.emacs.d/piglatin.el")

;; ===== Clojure Mode =====
;(unless (package-installed-p 'clojure-mode)
;  (package-refresh-contents)
;  (package-install 'clojure-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(edts-inhibit-package-check t)
 '(edts-man-root "/home/wiso/.emacs.d/edts/doc/17.3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
