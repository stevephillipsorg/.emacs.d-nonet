;;=========================================================================== 
;; 
;; ~/.emacs.d/init.el 
;; Author: Steve Phillips 
;; 
;;--------------------------------------------------------------------------- 

;; Add my personal elisp lib to the load path 
(setq load-path (cons "~/.emacs.d/elisp" load-path)) 

;; Set my name and email
(setq user-full-name "Steve Phillips" 
      user-mail-address "sphillips@marvell.com") 

;; A common optimization is to temporarily disable garbage collection 
;; during initialization, but in general the default is too low for 
;; madern machines. Here, we set the =gc-cons-threshold= to a 
;; ridiculously large number during initialization, and then set is to 
;; a more reasonable number afterwards. Report the Emacs startup time 
;; in *messages*. The default is 800 kilobytes.  Measured in bytes. 

(setq gc-cons-threshold most-positive-fixnum) 

;; Profile emacs startup 
(add-hook 'emacs-startup-hook 
          (lambda () 
            (message "*** Emacs loaded in %s with %d garbage collections." 
                     (format "%.2f seconds" 
                             (float-time 
                              (time-subtract after-init-time before-init-time)))
                     
                     (setq gc-cons-threshold (* 20 1000 1000)) 
                     ))
          ) 
  
;; There are a few things I like to do after the init has finished, 
;; like load the custom.el stuff and start the server. I also like to 
;; keep a few settings private, so we load a =private.el= if it exists 
;; after the init-file has loaded.  
(add-hook 
 'after-init-hook 
 (lambda () 
   (let
       (
        (custom-file (concat user-emacs-directory "custom.el")) 
        (private-file (concat user-emacs-directory "private.el"))
        ) 
     (when (file-exists-p custom-file)  (load-file custom-file)) 
     (when (file-exists-p private-file) (load-file private-file)) 
     )
   )
 ) 


;; This macro that is used for setting custom variables. The builtin
;; custom-set-variables comes close to this, but this is invisible to
;; the customize stuff that emacs saves. Cribbed from
;; https://oremacs.com/2015/01/17/setting-up-ediff/  
(defmacro sjp/csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; These two function allow me to quickly switch between two points in 
;; a buffer. It uses the "register" functions but in a way that I can 
;; bind them to keys to use quickly. I personally set "C-." to 
;; sjp/point-to-register and "S-." to sjp/jump-to-register. Hit "C-." 
;; to set your initial point, then you can go else where in the buffer 
;; and use "S-." to toggle back and forth between the current position 
;; and the point you saved.  
(defun sjp/point-to-register () 
  "Store cursorposition _fast_ in a register. Use sjp/jump-to-register 
to jump back to the stored position." 
  (interactive) 
  (point-to-register 8)) 

(defun sjp/jump-to-register () 
  "Switches between current cursorposition and position 
that was stored with sjp/point-to-register." 
  (interactive) 
  (let ((tmp (point-marker))) 
    (jump-to-register 8) 
    (set-register 8 tmp))) 

(global-set-key (kbd "C-.") 'sjp/point-to-register) 
(global-set-key (kbd "s-.") 'sjp/jump-to-register) 

;; I like to switch between themes depending on time of day and my 
;; mood. By default Emacs loads themes on top of each other which can 
;; lead to odd conflicts. By using =counsel-load-theme=, I get the Ivy 
;; selection method.  
(defun sjp/switch-theme () 
  (interactive) 
  (while custom-enabled-themes 
    (disable-theme (car custom-enabled-themes))) 
  (counsel-load-theme)) 
(global-set-key (kbd "C-x t t") 'sjp/switch-theme) 

;;--------------------------------------------------------------------------- 
;;
;; The following settings for Ediff were cribbed from
;; https://oremacs.com/2015/01/17/setting-up-ediff/
;;
;; A couple of useful commands:
;; - *vc-ediff*: Compares current buffer with last checked in version of file
;;- *ediff-current-file*: Compares current buffer to version of file on disk
;;
;; Don't use the weird setup with the control panel in a separate
;; frame. Make it a bottom window in the current frame.
(sjp/csetq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split the windows horizontally instead of vertically. This way,
;; it's much easier to follow the changes.
(sjp/csetq ediff-split-window-function 'split-window-horizontally)

;; Ignore white space. Not sure I I like this setting or not
(sjp/csetq ediff-diff-options "-w")

;; When you quit an Ediff session with q, it just leaves the two diff
;; windows around, instead of restoring the window configuration from
;; when Ediff was started. Here's the (slightly hacky) code to restore
;; the old window configuration: 
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)


;;--------------------------------------------------------------------------- 
;;
;; Some random settings...
;;
(setq inhibit-splash-screen t)        ;; no splash screen at startup 
(tool-bar-mode -1)                    ;; no graphical toolbar 
(setq inhibit-default-init t)         ;; disable loading of 
                                      ;; "default.el" at startup 
  
(setq transient-mark-mode t)          ;; enable visual feedback on 
                                      ;;    selections  
(set-scroll-bar-mode 'right)          ;; Put scrollbar on right to 
                                      ;;   match other windows. Replace 
      ;;   'right with 'left to place it 
      ;;   to the left     
(setq frame-title-format              ;; default to better frame titles  
      (concat  "%b - emacs@" (system-name))) 
(defalias 'list-buffers 'ibuffer)     ;; Use ibuffer instead of plain buffer list 
(setq diff-switches "-u")             ;; default to unified diffs 
(setq require-final-newline 'query)   ;; always end a file with a 
;;   newline 
(fset 'yes-or-no-p 'y-or-n-p)         ;; brevity 

;; Try some mouse wheel settings 
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) 
(setq mouse-wheel-progressive-speed nil) 
  
;; Use isearch by default 
(global-set-key (kbd "C-s") 'isearch-forward-regexp) 
(global-set-key (kbd "C-r") 'isearch-backward-regexp) 
(global-set-key (kbd "C-M-s") 'isearch-forward) 
(global-set-key (kbd "C-M-r") 'isearch-backward) 
  
(show-paren-mode 1)                   ;; Highlight matching paren 
(setq-default indent-tabs-mode nil)   ;;  
;;(setq x-select-enable-clipboard t)    ;; Under X, use X clipboard 
;;(setq x-select-enable-primary t)      ;; Under X, use X clipboard 
(setq save-interprogram-paste-before-kill t)  
(setq apropos-do-all t) 
(setq mouse-yank-at-point t)          ;; Mouse yanking inserts at the 
                                      ;;   point instead of the 
                                      ;;   location of the click 
(setq require-final-newline t)        ;; require file to end with newline  
(setq visible-bell t) 
(setq load-prefer-newer t) 
;;(setq ediff-window-setup-function 'ediff-setup-windows-plain) 
(setq save-place-file (concat user-emacs-directory "places")) 
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory 
       "backups")))) 
  
  
;;--------------------------------------------------------------------------- 
;; 
;; Packages 
;; 
;; Lets use package.el and the Melpa repo 
(require 'package)  

;; This init.el is intended to be used in the Sockeye compute server
;; farm, which does not have an internet connection. So we use the
;; environment variable EMACSNONET to indicate if we have an internet
;; connection. If we do then we can access elpa/melpa as usual and
;; update packages. If not, then we use the already loaded packages in
;; elpa/

;; IF EMACSNONET 
(when (getenv "EMACSNONET") ;; If NONET is set then we should only use local repo 
  ;; Original value of package-archives is (("gnu" . "https://elpa.gnu.org/packages/")) 
  ;; Lets delete the elpa entry and add our local repos 
  (setq package-archives (butlast package-archives)) ;; delete last entry in list 
  (add-to-list 'package-archives '("elpa" . "~/.emacs.d/elpa/")) 
  (add-to-list 'package-archives '("sjp" . "~/.emacs.d/packages/")) 
  ) 
;; ELSE IF !EMACSNONET 
(unless (getenv "EMACSNONET") ;; If NONET is not set then we should only use ELPA/MELPA 
  (add-to-list 'package-archives '("sjp" . "~/.emacs.d/packages/")) 
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) 
  ) 
;; END 

;; Now that papckage-archives is set up, intialize  
(package-initialize) 

;; Lets use the "use-package" package to make loading of packages easier 
;; See https://goo.gl/LtWMy for details  
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package)) 

(require 'use-package) 
(setq use-package-always-ensure t 
      use-package-verbose t) 

;; I use auto-package-update to automatically update packages. With
;; this setup, packages will be updated every 4 days, and the old
;; packages will be removed. This creates a risk that an updated
;; pacjage may break something, but thats a risk I'm willling to take,
;; especially because out-of-date packages can also break things. Got
;; this from user *cslux* on StackExchange
;; (https://tinyurl.com/yghmmwvw). 

(unless (getenv "EMACSNONET") ;; If NONET is not set then we should do auto-updates
  (use-package auto-package-update
    :ensure t
    :config
    (setq auto-package-update-delete-old-versions t
          auto-package-update-interval 4)
    (auto-package-update-maybe))
  )
  
;; Try - Allows one to try a new package without adding it to 
;; use-package. The package will go away the next time emacs is 
;; restarted 
(use-package try 
  :ensure t) 
   
;; which-key - https://goo.gl/vYPnea 
;; Displays key bindings for buffer. After starting a command 
;; sequence, it will show possible completions. For instance, hit C-x, 
;; wait a sec, and a help window will pop up  
(use-package which-key 
  :ensure t  ;; make sure it loads correctly 
  :init 
  (which-key-mode)  ;; turn on which-key mode 
  ;; try to use a side window if there is room, otherwise 
  ;;   use a bottom window  
  (which-key-setup-side-window-right-bottom)) 

;; -------------------------------------------------------------------- 
;; Color Themes  
;; 
;; Some other themes I like. Load them here and then select them with 
;; <F5>  
;;  (use-package anti-zenburn-theme) ;; blues/purple on medium grey 
;;  (use-package doneburn-theme)     ;; muted colors on white 
(use-package hc-zenburn-theme)   ;; muted tans/green on dark grey 
(use-package labburn-theme)      ;; muted tans/green on dark grey 
(use-package zenburn-theme)      ;; tans/green on dark grey 
;;  (use-package material-theme)     ;; too many flavors 
;;  (use-package alect-theme)        ;; Not found? 
;;  (use-package colorless-theme)    ;; Not found? 
(use-package modus-themes) 
(use-package leuven-theme) 
(use-package color-theme-sanityinc-solarized) 
(use-package organic-green-theme) 
;;  (use-package base16-theme) ;; too many flavors 
(use-package poet-theme) 

;; Set default theme 
(load-theme 'zenburn t) 

;; ------------------------------------------------------------------------------
;;
;; Make some thing pretty
;;
;; --- Dired and Dirvish
;; First, lets set the options for the =ls= command for the best listing
;;; set the options for the =ls= command for the best listing
(setq dired-listing-switches
      "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

;; Use dirvish with default settings
(use-package dirvish)

;; Expand the directories in-place when hitting TAB
(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))


;; -------------------------------------------------------------------- 
;; Vertico/Orderless/Marginalia

;; Enable vertico
(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :bind (:map vertico-map
            ("<tab>" . #'vertico-insert)  ; Insert selected candidate into text are;a
            ("<escape>" . #'minibuffer-keyboard-quit) ; Close minibuffer
            ;; NOTE 2022-02-05: Cycle through candidate groups
            ;;"C-M-n" #'vertico-next-group
            ;;"C-M-p" #'vertico-previous-group
            )
  :config
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Marginalis adds addtional information to the list items
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . #'marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; Orderless makes the search look for any order of the search terms
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Example configuration for Consult, taken from
;;   https://github.com/minad/consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;sjp;; ;; -------------------------------------------------------------------- 
;;sjp;; ;; Ivy/Counsel/Swiper - https://goo.gl/uv6e2k 
;;sjp;; ;; Configure to use ivy-mode for completion 
;;sjp;; ;; These config lines stolen from - https://tinyurl.com/yxas68kw 
;;sjp;; (use-package ivy 
;;sjp;;   :defer 0.1 
;;sjp;;   :diminish 
;;sjp;;   :bind (("C-c C-r" . ivy-resume) 
;;sjp;;          ("C-x B" . ivy-switch-buffer-other-window)) 
;;sjp;;   :custom 
;;sjp;;   (ivy-count-format "(%d/%d) ") 
;;sjp;;   (ivy-use-virtual-buffers t) 
;;sjp;;   :config (ivy-mode)) 
;;sjp;; 
;;sjp;; (use-package counsel 
;;sjp;;   :after ivy 
;;sjp;;   :config (counsel-mode) 
;;sjp;;   :bind ("<f5>" . counsel-load-theme) 
;;sjp;;   ) 
;;sjp;; 
;;sjp;; (use-package ivy-rich 
;;sjp;;   :ensure t 
;;sjp;;   :after (:all ivy counsel) 
;;sjp;;   :init (setq ivy-rich-parse-remote-file-path t) 
;;sjp;;   :config (ivy-rich-mode 1)) 
;;sjp;; 
(use-package swiper 
;;  :after ivy 
  :bind (("C-s" . swiper) 
         ("C-r" . swiper))) 


;; Magit - https://magit.vc/ 
;;   Interface to Git 
(use-package magit 
  :ensure t 
  :bind ("C-x g" . magit-status) 
  ) 

;; Diff-hl uses the VC system to determine changes and highlights them in the fringe.  
(use-package diff-hl
  :ensure t 
  :init
  ;; enables diff-hl for all buffers
  (global-diff-hl-mode)
  ;; diff-hl marks changes on the fly
  (diff-hl-flydiff-mode)
  ;;; adds diff-hl for dired mode
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  )

;; Set fringe width to 16 pixels on the left and the default of 8 on the right
;;sjp;; (fringe-mode '(16 . 8))

;;sjp;; (use-package git-emacs
;;sjp;;   :ensure t 
;;sjp;;   )

;; This package provides an emacs interface for Gerrit, a great code review tool
(use-package gerrit
  :ensure t
  :custom
  (gerrit-host "gerrit.my.domain")  ;; is needed for REST API calls
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
    (global-set-key (kbd "C-x o") 'gerrit-download)))

;; smart-mode-line - https://goo.gl/cJjp28 
;; makes your modeline smarter 
(use-package smart-mode-line 
  :init 
  (setq sml/no-confirm-load-theme t) ;; see web page 
  (sml/setup)) 

;; flycheck - linter for many languages - https://www.flycheck.org/ 
(use-package flycheck 
  :ensure t 
  :init (global-flycheck-mode)) 

;; markdown-mode 
(use-package markdown-mode 
  :mode ("[file://.//(m//(ark//)?down\\|md\\)$]\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode) 
  :config) 

;; Smartscan - https://goo.gl/FWI0XF 
;; Quickly search for symbol at point with M-n and M-p 
;; Stored in ~/.emacs.d/packages/ 
(use-package smartscan 
  :init 
  (global-smartscan-mode 1)) 

;; vlf - Very Large File - Allows one to visit part of large
;;       file without loading it entirely
(use-package vlf
  :ensure t 
  )



;;-------------------------------------------------------------- 
;; 
;; Verilog mode settings 
;; 
;; Verilog-mode is invoked in the site-settings.el file so we don't 
;; need to do it ourselves. Keeping this here in case there are some 
;; settings I'd like to change from the site-settings defaults. 
;; 
;;sjp;; ---- Original Verilog Mode setting --- 
;;sjp;; (use-package verilog-mode 
;;sjp;;   :mode ("[file://.[ds]%3Fvh%3F//]\\.[ds]?vh?\\'" . verilog-mode) 
;;sjp;;   :init (setq verilog-auto-newline nil) ;; Non-nil means automatically newline after semicolons. 
;;sjp;;   ) 
;;sjp;; ;; This is how I always used to load verilog mode  
;;sjp;; (autoload 'verilog-mode "verilog-mode" "Verilog mode" t ) 
;;sjp;; (add-to-list 'auto-mode-alist '("[file://.[ds]%3Fvh%3F//]\\.[ds]?vh?\\'" . verilog-mode)) 
;;sjp;; (add-hook 'verilog-mode-hook 
;;sjp;;           '(lambda () 
;;sjp;;              (setq verilog-auto-newline nil) 
;;sjp;;              (setq ggtags-mode t) ;; always run ggtags-mode in verilog mode 
;;sjp;;              ))  

;;------------------------------------------------------------------------------
;;
;;  Org Mode
;;
;; Org Mode is one of the hallmark features of Emacs.  It is a rich
;; document editor, project planner, task and time tracker, blogging
;; engine, and literate coding utility all wrapped up in one package. 
;;
;; --- Better Font Faces
;;
;; The =sjp/org-font-setup= function configures various text faces to
;; tweak the sizes of headings and use variable width fonts in most
;; cases so that it looks more like we're editing a document in
;; =org-mode=.  We switch back to fixed width (monospace) fonts for
;; code blocks and tables so that they display correctly. 

(defun sjp/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 2.4)
                  (org-level-2 . 2.0)
                  (org-level-3 . 1.8)
                  (org-level-4 . 1.6)
                  (org-level-5 . 1.4)
                  (org-level-6 . 1.2)
                  (org-level-7 . 1.2)
                  (org-level-8 . 1.2)))
    )
  
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil     :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil     :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil   :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil  :inherit '(shadow fixed-pitch))
  
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil   :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil      :inherit '(font-lock-comment-face fixed-pitch)))

;; --- Basic Config
;;
;; This section contains the basic configuration for =org-mode= plus
;; the configuration for Org agendas and capture templates. 

(defun sjp/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  ;;sjp      :pin org    ;; This causes errors now that we aren't using 
  ;;sjp                  ;; seperate package repo for org-mode
  :commands (org-capture org-agenda)
  :hook (org-mode . sjp/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (sjp/org-font-setup)
  )

  ;;;sjp;    (setq org-agenda-files
  ;;;sjp;          '("~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org"
  ;;;sjp;            "~/Projects/Code/emacs-from-scratch/OrgFiles/Habits.org"
  ;;;sjp;            "~/Projects/Code/emacs-from-scratch/OrgFiles/Birthdays.org"))

;; There is a lot more Org config from EFS example, mostly for TODOs,
;; Agenda and Capture that I left out here. Perhaps I'll go back and
;; add some of that in when I get better at Org files. 

;; --- Nicer Heading Bullets

;; org-bullets replaces the heading stars in =org-mode= buffers with
;; nicer looking characters that you can control.  

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; --- Configure Babel Languages
;;
;; To execute or export code in =org-mode= code blocks, you'll need to
;; set up =org-babel-load-languages= for each language you'd like to
;; use.

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; --- Structure Templates
;;
;; Org Mode's structure templates feature enables you to quickly
;; insert code blocks into your Org files in combination with
;; =org-tempo= by typing =<= followed by the template name like =el=
;; or =py= and then press =TAB=.  For example, to insert an empty
;; =emacs-lisp= block below, you can type =<el= and press =TAB= to
;; expand into such a block.

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;;--- Auto-tangle Configuration Files
;;
;; This snippet adds a hook to =org-mode= buffers so that
;; =sjp/org-babel-tangle-config= gets executed each time such a buffer
;; gets saved.  This function checks to see if the file being saved is
;; the Emacs.org file you're looking at right now, and if so,
;; automatically exports the configuration here to the associated
;; output files. 

;; Automatically tangle our Emacs.org config file when we save it
(defun sjp/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'sjp/org-babel-tangle-config)))

;; One last thing before we are finished: Start the emacs daemon
;; if it isn't already running
(require 'server)
(unless (server-running-p)
  (server-start))
;; This will also kill the server in addition to the frame. To kill
;; just the frame use "C-x 5 0"
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs) 


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gerrit git-gutter-fringe git-gutter embark-consult embark consult project org-bullets elpa-mirror smartscan markdown-mode flycheck smart-mode-line magit ivy-rich counsel ivy dired-subtree dirvish all-the-icons-dired all-the-icons poet-theme organic-green-theme color-theme-sanityinc-solarized leuven-theme modus-themes zenburn-theme labburn-theme hc-zenburn-theme which-key try auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "gray80" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#008B45" :underline t)))))
