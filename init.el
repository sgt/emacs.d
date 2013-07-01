(require 'cl)
(require 'hippie-exp)

;; packages and starter kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                                  starter-kit-js starter-kit-ruby
                                  scala-mode tabbar ipython anything-ipython
                                  python-mode clojure-mode clojure-test-mode
                                  slime nrepl qsimpleq-theme
                                  haskell-mode ghc
                                  auto-complete ac-nrepl rainbow-delimiters
                                  slamhound nrepl-ritz)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; my own preferences
;;(require 'qsimpleq-theme)

(menu-bar-mode)

;; Key bindings
(global-set-key (kbd "C-x m") 'shell)
(global-set-key (kbd "C-s-SPC") 'complete-symbol)

;; Russian key modifiers
(loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to))))))

;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE
;; (set-clipboard-coding-system 'utf-16le-dos)

(setq visible-bell nil)

;; disable highlighting current line
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; additional extensions for speedbar to show
(require 'speedbar)
(dolist (element '(".rb" ".rhtml" ".ru" ".clj" ".pom" ".css"))
  (speedbar-add-supported-extension element))

;; tabs on the top
(require 'tabbar)
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(org-agenda-files (quote ("~/Dropbox/notes/comics.org")))
 '(show-paren-mode t)
 '(tabbar-separator (quote (0.5)))
 '(tool-bar-mode nil)
 '(virtualenv-root "~/tmp/virtualenv/"))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))
(tabbar-mode)

(defun my-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs Buffer")
    ((eq major-mode 'dired-mode) "Dired")
    (t "User Buffer"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(require 'scala-mode-auto)

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;(load (concat dotfiles-dir "vendor/jinja2-mode/jinja2-mode.el"))
;;(require 'jinja2-mode)

;;(add-to-list 'load-path (concat dotfiles-dir "/vendor/rhtml"))
;;(require 'rhtml-mode)
;;(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(setq visible-bell t)

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)

(scroll-bar-mode 'right)

(setq c-basic-offset 2)
(setq tab-width 2)

(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

;; === clojure

;; indent midje facts properly
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (fact 'defun)
     (facts 'defun)
     (against-background 'defun)
     (provided 0)))

(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

;; Configure nrepl.el
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl nil)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")

;; Some default eldoc facilities
(add-hook 'nrepl-connected-hook
          (defun pnh-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

;; Repl mode hook
(add-hook 'nrepl-mode-hook 'subword-mode)

;; Make C-c C-z switch to the *nrepl* buffer in the current window:
(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

(require 'paredit)
(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)

;; -- commenting out autocomplete -- i don't think i like it anymore

;; (require 'auto-complete-config)
;; ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

(require 'ac-nrepl)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode 'clojure-mode))
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; doesn't work
;; (define-key nrepl-interaction-mode-map (kbd "C-c C-d")
;; 'ac-nrepl-popup-doc)

;;(add-auto-mode 'gfm-mode "\\.md\\'")
(add-auto-mode 'clojure-mode "\\.cljs\\'")

(add-hook 'gfm-mode-hook 'turn-off-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; === javascript
(add-auto-mode 'js2-mode "\\.js\\'")

;; === erlang

(setq erlang-root-dir
      (if (eq system-type 'windows-nt)
          "C:/Program Files/erl5.10.1"
        "/usr/lib/erlang"))
;;(setq erlang-man-root-dir (concat erlang-root-dir "/man"))
;;(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.10/emacs"))
(require 'erlang-start)
(add-to-list 'load-path "~/.emacs.d/vendor/distel/elisp")
(require 'distel)
(distel-setup)

(add-auto-mode 'erlang-mode "\\.app.src$" "\\.rel$")

(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name temp-file
		(file-name-directory buffer-file-name))))
    (list "~/.emacs.d/erlang_flymake" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

(defun my-erlang-mode-hook ()
  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs" "-pz" "deps/*/ebin"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ;; customize keys
  (local-set-key [return] 'newline-and-indent)

  (flymake-mode 1))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

(windmove-default-keybindings 'super)
(setq shift-select-mode t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; === go

(eval-after-load "go-mode"
  '(require 'flymake-go))

;; === haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'ghc-init "ghc" nil t)
;;(setq ghc-flymake-command t) ;; hlint
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(add-to-list 'exec-path "~/.cabal/bin")

(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (server-start))

;; === python
(add-hook 'python-mode-hook (lambda () (flymake-mode)))

(when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "epylint" (list local-file))))

      (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (if (eq system-type 'windows-nt)
     '(default ((t (:family "Consolas" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
     '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 108 :width normal)))))
 '(flymake-errline ((((class color)) (:background "Pink"))))
 '(flymake-warnline ((((class color)) (:background "LightBlue")))))
