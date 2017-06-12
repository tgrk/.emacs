;; ===== Customizations go in emacs.d =====
(add-to-list 'load-path "~/.emacs.d/lisp/")

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
(add-to-list 'load-path "/home/tgrk/.emacs.d/lisp/nyan-mode/")
(require 'nyan-mode)
(setq nyan-wavy-tail t)
(setq nyan-bar-length 4)
(setq nyan-animate-nyancat t)
(nyan-mode)

;; ===== Enable on the fly spelll checking =====
;(defun turn-on-flyspell () (flyspell-mode 1))
;(add-hook 'find-file-hooks 'turn-on-flyspell)

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

;; ===== Autocompletition on buffers/file names =====
;;(ido-mode 1)

;; ===== Cycle through buffers with Ctrl-Tab (like Firefox) =====
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; ===== Editing .emacs config =====
(global-set-key (kbd "<f12>") ;
  (lambda()(interactive)(find-file "~/.emacs")))

; ===== Buffer navigation =====
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

;; ===== Interactively Do Things (smart tab-completion in find file etc.) =====
(require 'ido)
(ido-mode t)

;; ===== Fixed line length =====
(setq whitespace-line-column 80)

;(require 'whitespace)
;(setq whitespace-style '(face empty tabs lines-tail trailing))
;(global-whitespace-mode t)

;;FIXME: this is not working
;; ===== Highlight TODO/FIXME/BUG keywords in Erlang comments ====
;;(set-face-underline 'font-lock-warning-face "yellow")
;;(add-hook 'c-mode-common-hook
;;               (lambda ()
;;                (font-lock-add-keywords nil
;;                 '(("<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; ===== Set key bindings =====
(global-set-key [f2] 'goto-line)
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


;; ===== Fixed buffers =====
;(setq special-display-buffer-names '("*Completions*" "*Gofmt Errors*" "*Warnings*"))

;; ===== Split window into N horizontal buffers =====
(defun create-vertical-buffers (screens)
  "Split window into N vertical buffers"
  (interactive)
  (setq buff-number 1)
  ;;(length (buffer-list)
  (while (< buff-number screens)
    ;(let (message-log-max) (message "test: %d"), buff-number))
    (split-window-horizontally)
    (setq buff-number (1+ buff-number))
    (message "bufr-number: %s" buff-number)
    ;;TODO split third window horizontally for output buffers
    ;if (= buff-number 1)
    ;   (split-window-vertically))
    )
  (balance-windows))

;; ===== On bigger display automatically create 3 buffers otherwise 2 =====
(if (< 1900 (x-display-pixel-width) )
    (funcall 'create-vertical-buffers 3)
  (funcall 'create-vertical-buffers 2) )

;; ===== Key bindings for tree vertical buffers =====
(global-set-key (kbd "C-x 4") (lambda () (interactive) (create-vertical-buffers 3)))


;; ===== Delete any trailing whitespace before saving =====
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ==================================================================================
;; PACKAGES
;; ==================================================================================
(require 'package)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; ==================================================================================
;; LANGUAGE SPECIFIC SETTINGS
;; ==================================================================================

;; ===== ActionScript mode =====
;;(load-file "~/.emacs.d/lisp/actionscript-mode.el")
;;(autoload 'actionscript-mode "javascript" nil t)
;;(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;; ===== Flex specific =====
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
(setenv "GOPATH" "/home/tgrk/go")
(add-to-list 'load-path "~/.emacs.d/lisp/go-mode")
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
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

;; ===== GO Mode - popwin for gofmt errors =====
(require 'popwin)
(popwin-mode 1)
(push '("*Gofmt Errors*" :noselect t) popwin:special-display-config)

;; ===== Rust Mode =====
;(require 'rust-mode)

;; ===== Erlang Emacs Mode + EDTS =====
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (require 'edts-start))

;; ===== EditorConfig =====
(require 'editorconfig)
(editorconfig-mode 1)

;; ===== Disable popup due to buffer limit in EDTS =====
;(add-to-list 'warning-suppress-types '(undo discard-info))

;; ===== Apache Pig Latin =====
;(load-file "~/.emacs.d/lisp/piglatin.el")

;; ===== Web Mode =====
(require 'web-mode)
(require 'flycheck-flow)

(load-file "~/.emacs.d/lisp/flow-types.el")

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq js2-basic-offset 2)
(setq js2-indent-switch-body 1)
(setq electric-indent-local-mode -1)
(setq js2-bounce-indent-p t)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defun web-mode-indent-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-code-indent-offset 2)
  (message "My Web Mode hook"))
(add-hook 'web-mode-hook 'web-mode-indent-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; ===== TypeScript Tide settings =====
(require 'web-mode)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
;; turn on flychecking globally
;(add-hook 'after-init-hook #'global-flycheck-mode)
;; disable json-jsonlist checking for json files
;(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
;; disable jshint since we prefer eslint checking
;(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; disable jshint since we prefer eslint checking
(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint
                        scss
                        scss-lint
                        )))


;; (setq-default flycheck-idle-change-delay 2.5)
(setq-default flycheck-check-syntax-automatically '(mode-enabled save))

;; ===== Clojure Mode =====
;(unless (package-installed-p 'clojure-mode)
;  (package-refresh-contents)
;  (package-install 'clojure-mode))

;; ===== Elixir Alchemist mode =====
(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))
(setq alchemist-mix-command "/home/tgrk/.kiex/elixirs/elixir-1.4.4/bin/mix")
(setq alchemist-mix-test-task "espec")
(setq alchemist-mix-test-default-options '()) ;; default
(setq alchemist-goto-erlang-source-dir "/home/tgrk/erlang/19.3/")
(setq alchemist-goto-elixir-source-dir "/home/tgrk/.kiex/builds/elixir-git/")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(edts-inhibit-package-check t)
 '(package-selected-packages
   (quote
    (ac-alchemist company-ansible company-erlang cypher-mode yaml-tomato yaml-mode web-mode typoscript-mode typescript tss tide projector popwin php-mode php-completion php+-mode moz markdown-mode jsx-mode js2-mode haskell-mode haskell-emacs-base go-autocomplete elm-yasnippets elm-mode edts editorconfig ansible-vault ansible-doc ansible angular-mode alchemist))))
 (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

'(ac-auto-show-menu t)
 '(ac-auto-start t)

(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

(setq erlang-root-dir "~/.emacs.d/edts/doc/19.3")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
