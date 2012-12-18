;; ===== Customizations go in emacs.d =====
(add-to-list 'load-path "~/emacs.d")

;; disable toolbar
(tool-bar-mode -1)

;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

;; ===== Disable system beep =====
(setq visual-bell t)

;; ===== Set default language =====	
(setq current-language-environment "UTF-8")

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

;; ===== enable tabbar =====
(require 'tabbar)
(tabbar-mode t)

;; tabar look customization
(set-face-attribute 'tabbar-default nil    :background "gray60")
(set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
(set-face-attribute 'tabbar-selected nil   :background "#f2f2f6" :foreground "black" :box nil)
(set-face-attribute 'tabbar-button nil     :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute 'tabbar-separator nil  :height 0.7)

;; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”, “User Buffer”.
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"
     )
    ))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [alt j] 'tabbar-backward)
(global-set-key [alt k] 'tabbar-forward)

;; ===== Enable Line and Column Numbering =====
(line-number-mode 1)
(column-number-mode 1)

;; Disable *GNU Emacs* startup buffer
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
(set-face-font 'default '"8x13") 

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

;; ===== Editing .emacs config =====
(global-set-key (kbd "<f12>") ; 
  (lambda()(interactive)(find-file "~/.emacs"))) 

;; run terminal 
(global-set-key (kbd "<f2>") ;
  (lambda()(interactive)(ansi-term "/bin/bash")))

;; ===== Buffer navigation =====
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

;; ===== CEDET =====
;;(load-file "/usr/share/emacs23/site-lisp/cedet-common/cedet.el")
;;(global-ede-mode t)                      ; Enable the Project management system
;;(semantic-load-enable-code-helpers)    ; Enable prototype help and smart completion 
;;(global-srecode-minor-mode t)          ; Enable template insertion menu

;; eproject
;;(add-to-list 'load-path "~/.emacs.d/eproject")
;;(require 'eproject)
;;(require 'eproject-extras)

;; eproject - list TODO's M-x eproject-todo
;;(rgrep "TODO" "*" (eproject-root))

;; ==== Custom full screen mode =====
;(defun toggle-fullscreen ()
;  (interactive)
;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;)
;(toggle-fullscreen)

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

;;===== Fixed line length =====
(setq whitespace-line-column 80)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Erlang Emacs Mode -- Configuration End
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; Interactively Do Things (smart tab-completion in find file etc.)
(require 'ido)
(ido-mode t)

;; ===== Highlight TODO/FIXME/BUG keywords in Erlang comments ====
(set-face-underline 'font-lock-warning-face "yellow")
(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;;
;; ------------------ Magic for XML Mode ----------------
;;
(setq nxml-mode-hook
    '(lambda ()
 (setq tab-width        2
       indent-tabs-mode nil)
       (set-variable 'nxml-child-indent     2)
       (set-variable 'nxml-attribute-indent 2)
       ))

;; ===== ESence for erlang =====
(add-to-list 'load-path "~/.emacs.d/esense/")
(require 'esense-start)
(setq esense-indexer-program "~/.emacs.d/esense/esense.sh")
(setq esense-completion-display-method 'frame)

;; execute esense indexer after saving file
(defun esense-after-save-hook ()
  (if buffer-file-name
      (progn
        (setq is-erl-file (numberp (string-match "\.erl$" buffer-file-name)))
        (if is-erl-file
            (progn
              (setq cmd (concat (getenv "B") "~/.emacs.d/esense/esense.sh "))
              (shell-command (concat cmd buffer-file-name))
              (message "Updated esense index for %s" buffer-file-name))))))
(add-hook 'after-save-hook 'esense-after-save-hook)

;; ===== Distel mode =====
(let ((distel-dir "~/.emacs.d/distel/elisp"))
   (unless (member distel-dir load-path)
;; Add distel-dir to the end of load-path
(setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; Creating a new menu pane in the menu bar to the right of “Tools” menu
;;(define-key-after
;;  global-map
;;  [menu-bar erlang-dializer]
;;  (cons "Dialyzer" (make-sparse-keymap "hoot hoot"))
;;  'erlang )

;; Creating a menu item, under the menu by the id “[menu-bar mymenu]”
;;(define-key
;;  global-map
;;  [menu-bar erlang-dializer nl]
;;  '("Next Line" . next-line))

;; creating another menu item
;;(define-key
;;  global-map
;;  [menu-bar erlang-dializer pl]
;;  '("Previous Line" . previous-line))

;; code to remove the whole menu panel
;; (global-unset-key [menu-bar mymenu])


;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
                                        (lambda ()
                                                ;; add some Distel bindings to the Erlang shell
                                                (dolist (spec distel-shell-keys)
                                                        (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; ===== Flymake for erlang =====
(require 'flymake)
(setq flymake-log-level 3)

(defun flymake-erlang-init ()
(let* ((temp-file (flymake-init-create-temp-buffer-copy
'flymake-create-temp-inplace))
(local-file (file-relative-name
temp-file
(file-name-directory buffer-file-name))))
(list "~/.erlang_code/eflymake" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
'("\\.erl\\'" flymake-erlang-init))

(defun my-erlang-mode-hook ()
(flymake-mode 1))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; ===== Wrangler =====
;;(add-to-list 'load-path "/usr/local/share/wrangler/elisp")
;;(require 'wrangler)

;; ===== Set key bindings =====
(global-set-key [f3] 'shell)
(global-set-key [f5] 'query-replace)
(global-set-key [f6] 'switch-to-buffer)
(global-set-key [f7] 'hippie-expand)
(global-set-key [f9] 'ispell)

(global-set-key [(control o)] 'find-file)              ; use Ctrl-o to open a (new) file
(global-set-key [(control n)] 'find-file-other-frame)  ; open a file in a new window with Ctrl-n

;; ===== Alt buffer navigation =====
(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer window

;; save/restore buffers
(desktop-save-mode 0)

;; ===== Function to delete a line =====

;; First define a variable which will store the previous column position
(defvar previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (setq previous-column (current-column))

  ;; Now move to the end of the current line
  (end-of-line)

  ;; Test the length of the line. If it is 0, there is no need for a
  ;; kill-line. All that happens in this case is that the new-line character
  ;; is deleted.
  (if (= (current-column) 0)
    (delete-char 1)

    ;; This is the 'else' clause. The current line being deleted is not zero
    ;; in length. First remove the line by moving to its start and then
    ;; killing, followed by deletion of the newline character, and then
    ;; finally restoration of the column position.
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))

;; Now bind the delete line function to the F8 key
(global-set-key [f8] 'nuke-line)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

