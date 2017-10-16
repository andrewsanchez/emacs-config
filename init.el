(custom-set-variables
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(safe-local-variable-values
   (quote
    ((projectile-project-test-cmd . "~/anaconda3/envs/genbankfilter/bin/pytest -s test/test_*")))))

(package-initialize)
(setq package-archives
    '(("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")
      ("melpa" . "http://melpa.milkbox.net/packages/")
      ("melpa-stable" . "http://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(setq user-full-name "Andrew Sanchez"
      user-mail-address "inbox.asanchez@gmail.com")

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
    :config
    (evil-mode 1))

(use-package evil-nerd-commenter
    :config
    (evilnc-default-hotkeys))

(use-package evil-surround
    :config
    (global-evil-surround-mode 1))

(use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)
    :config (evil-lisp-state-leader "<SPC> k"))

(use-package bind-map) 

(use-package evil-matchit
  :config
  (require 'evil-matchit)
  (global-evil-matchit-mode 1))

(add-to-list 'evil-emacs-state-modes 'dired-mode)
(add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
(add-to-list 'evil-emacs-state-modes 'nov-mode)
(add-to-list 'evil-emacs-state-modes 'gnus-group-mode)
(evil-set-initial-state 'Info-mode 'emacs)

(evil-leader/set-key
  "!" 'shell-command)

(use-package smart-mode-line
    :init
    (setq sml/no-confirm-load-theme t)
    :config
    (sml/setup))
(display-time-mode 1)

(use-package helm
      :init
      (require 'helm-config)
      (evil-leader/set-key
	"sg" 'helm-google-suggest
	"<SPC>" 'helm-M-x
	"sj" 'helm-semantic-or-imenu
	"ha" 'helm-apropos
	"ss" 'helm-occur)
      :commands helm-M-x
      :bind (("M-y" . helm-show-kill-ring)
	     ("C-h a" . helm-apropos)
	     ("M-x" . helm-M-x)
	     ("C-x b" . helm-buffers-list)))

  (use-package helm-descbinds
    :bind (("C-h b" . helm-descbinds)))

(use-package hydra)

(defhydra hydra-buffers (:color blue)
  "Buffers"
  ("d" kill-this-buffer "kill buffer")
  ("s" (lambda ()
	 (interactive)
	 (pop-to-buffer "*scratch*"))
   "scratch")
  ("b" helm-buffers-list "helm buffers list"))

(evil-leader/set-key "b" 'hydra-buffers/body)

(evil-leader/set-key "f" 'hydra-files/body)

(defhydra hydra-files (:color blue)
  "Files"

  ("d" (lambda () (interactive)
	 (find-file "~/projects/emacs-config/init.org"))
   "dot-file")

  ("g" (lambda () (interactive)
	(find-file "~/org/agenda/gtd.org"))
   "gtd")
  ("f" helm-find-files "helm-find-files")
  ("m" helm-multi-files "helm-multi-files")
  ("b" helm-filtered-bookmarks "helm-filtered-bookmarks")
  ("t" neotree-toggle "neotree-toggle"))

(use-package org
  :load-path "~/.emacs.d/packages/org-mode/lisp"
  :init
  (evil-leader/set-key
      "oa" 'org-agenda
      "oc" 'org-capture
      "ot" 'hydra-org-clock/body)
  :config
  (setq org-hide-leading-stars t)
  (setq org-default-notes-file "/Users/andrew/org/notes.org")
  (setq org-todo-keywords
    '((sequence "NEXT" "TODO" "|" "DONE")))
  (setq org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 2)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-fontify-natively t)
  (evil-leader/set-key-for-mode 'org-mode
    "m" 'hydra-org-headings/body)
  ;; Hydras
  (defhydra hydra-org-headings ()
    "Headings"
	("t" org-todo "org-todo")
	(":" org-set-tags-command "org-set-tags-command")
	("n" org-narrow-to-subtree "org-narrow-to-subtree")
	("w" widen "widen")
	("s" org-sort)
	("l" org-demote-subtree "org-demote-subtree")
	("h" org-promote-subtree "org-promote-subtree")
	("K" outline-up-heading "org-backward-heading-same-level")
	("J" org-forward-heading-same-level "org-forward-heading-same-level")
	("k" outline-previous-visible-heading "outline-previous-visible-heading")
	("j" outline-next-visible-heading "outline-next-visible-heading")
	("*" org-toggle-heading "org-toggle-heading")
	("r" org-refile "org-refile"))

  (defhydra hydra-org-clock (:color blue :hint nil)
      "

      Clock   In/out^     ^Edit^   ^Summary     (_?_)
      -----------------------------------------
	      _i_n         _e_dit   _g_oto entry
	      _c_ontinue   _q_uit   _d_isplay
	      _o_ut        ^ ^      _r_eport
	      _p_omodoro
      "
      ("i" org-clock-in)
      ("o" org-clock-out)
      ("c" org-clock-in-last)
      ("e" org-clock-modify-effort-estimate)
      ("q" org-clock-cancel)
      ("p" org-pomodoro)
      ("g" org-clock-goto)
      ("d" org-clock-display)
      ("r" org-clock-report)
      ("?" (org-info "Clocking commands"))))

  (use-package org-pomodoro
    :commands org-pomodoro
    :config
    (setq mindfulness-bell "/Users/andrew/Music/Miscellaneous/Timer_Sounds/mindfullness_bell.mp3") 
    (setq mindfulness-chimes "/Users/andrew/Music/Miscellaneous/Timer_Sounds/chimes.mp3") 
    (setq org-pomodoro-length 15)
    (setq org-pomodoro-short-break-length .5)
    (setq org-pomodoro-start-sound mindfulness-bell)
    (setq org-pomodoro-finished-sound mindfulness-bell)
    (setq org-pomodoro-short-break-sound mindfulness-bell)
    (setq org-pomodoro-long-break-sound mindfulness-chimes)
    (setq org-pomodoro-start-sound-p t))

(setq org-babel-load-languages
      '((emacs-lisp . t) (shell . t)))

(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "/Users/andrew/org/agenda/gtd.org" "Tasks")
	 "* TODO %? \n%U\n" :empty-lines 1)
	("n" "NEXT" entry (file+headline "/Users/andrew/org/agenda/gtd.org" "Tasks")
	 "* NEXT %? \n%U\n" :empty-lines 1)
	("h" "New Headline" entry (file+headline "/Users/andrew/agenda/gtd.org" "Notes")
	   "* %?\n")
	("p" "Plan" entry (file+headline "/Users/andrew/org/agenda/gtd.org" "Plans")
	"* %?\n")
	("j" "Journal" entry (file+datetree "/Users/andrew/org/agenda/journal.org")
	"* %?\nEntered on %U\n")))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

     (setq org-agenda-sorting-strategy
	   '((agenda habit-down timestamp-down priority-down category-keep)
	    (todo priority-down timestamp-down category-keep)
	    (tags priority-down timestamp-down category-keep)
	    (search category-keep timestamp-down)))

     (setq org-agenda-files '("~/org/agenda" "~/org/projects"))
     (setq org-agenda-custom-commands
	 '(("!" "ASAP" tags-todo "asap-TODO=\"DONE\"") 
	     ("n" . "Next")
	     ("np" "Next PMI" tags-todo "TODO=\"NEXT\"+category=\"PMI\""
	      ((org-agenda-overriding-header "Next PMI")))
	     ("na" "Next ABB" tags-todo "TODO=\"NEXT\"+category=\"ABB\""
	      ((org-agenda-overriding-header "Next ABB")))
	     ("nm" "Next Miscellaneous" tags-todo "TODO=\"NEXT\"+category=\"misc\""
	      ((org-agenda-overriding-header "Next Miscellaneous")))
	     ("A" . "All")
	     ;("am" "All Miscellaneous" tags-todo "TODO={TODO\\|NEXT}+category=\"misc\"")
	     ("Am" "All Miscellaneous"
	     ((tags-todo "TODO=\"NEXT\"+category=\"misc\"")
	     (tags-todo "TODO=\"TODO\"+category=\"misc\"")
	     (tags-todo "TODO=\"DONE\"+category=\"misc\""))
	     ((org-agenda-overriding-header "All Miscellaneous")))
	     ("Ap" "All PMI"
	     ((tags-todo "TODO=\"NEXT\"+category=\"PMI\"")
	     (tags-todo "TODO=\"TODO\"+category=\"PMI\"")
	     (tags-todo "TODO=\"DONE\"+category=\"PMI\""))
	     ((org-agenda-overriding-header "")))
	     ("Aa" "ALL"
	     ((tags-todo "TODO=\"NEXT\"")
	     (tags-todo "TODO=\"TODO\"")
	     (tags-todo "TODO=\"DONE\""))
	     ((org-agenda-overriding-header "All")))))

(tool-bar-mode -1)

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-next)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-right)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-left)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-down)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-up))

(use-package winner
  :commands
  (winner-undo winner-redo)
  :config
  (winner-mode)
  (evil-leader/set-key
    "wu" 'winner-undo
    "wr" 'winner-redo))

(use-package zoom-frm
  :commands hydra-zoom)

(defhydra hydra-zoom (global-map "C-=")
  "zoom"
  ("g" text-scale-increase)
  ("l" text-scale-decrease)
  ("i" zoom-in)
  ("o" zoom-out))

;; Special dir for backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package magit
  :init
  (evil-leader/set-key "gs" 'magit-status)
  :commands magit-status
  :config
  (setq magit-git-executable "~/usr/bin/git"))

(show-paren-mode 1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(fset 'yes-or-no-p 'y-or-n-p)

;; This doesn't actually seem to be faster...
;; (setq tramp-default-method "ssh")
(setq tramp-inline-compress-start-size 1000000)

(use-package which-key
    :defer 10
    :config
    (which-key-mode))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (evil-leader/set-key-for-mode 'python-mode "m" 'hydra-python/body)
  (add-hook 'before-save-hook 'py-isort-before-save)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (setq python-shell-exec-path '("~/anaconda3/bin/python"))
  (use-package anaconda-mode
    :commands hydra-python/body
    :config
    (anaconda-mode)
    (anaconda-eldoc-mode))
  (use-package sphinx-doc
    :commands (sphinx-doc)
    :config
    (sphinx-doc-mode))
  (use-package helm-pydoc :commands helm-pydoc)
  (use-package py-isort :commands py-isort-buffer
    :config
    (require 'py-isort))
  (use-package elpy
    :init (with-eval-after-load 'python (elpy-enable))
    :commands elpy-enable
    :config
    (defhydra elpy-hydra (:color red)
      "
      Elpy in venv: %`pyvenv-virtual-env-name
      "
      ("t" (progn (call-interactively 'elpy-test-pytest-runner) (elpy-nav-errors/body)) "current test, pytest runner" :color blue)
      ("w" (venv-workon) "workon venv…")
      ("q" nil "quit")
      ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue))
    (defhydra elpy-nav-errors (:color red)
      " Navigate errors "
      ("n" next-error "next error")
      ("p" previous-error "previous error")
      ("s" (progn
	     (switch-to-buffer-other-window "*compilation*")
	     (goto-char (point-max))) "switch to compilation buffer" :color blue)
      ("w" (venv-workon) "Workon venv…")
      ("q" nil "quit")
      ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue)))
  (defhydra hydra-python (:color blue :hint nil)
  "
  ^Navigation^                  ^Elpy^                 ^Formatting^
  -------------------------------------------------------------------------
  _d_: find definitions       _w_: venv-workon       _y_: yapfify-buffer
  _a_: find assignments       _W_: venv-deactivate   _i_: py-isort-buffer
  _r_: find references        _t_: elpy-test         _f_: flycheck
  _b_: go back
  _s_: show doc
  _v_: pythonic-activate
  _V_: pythonic-deactivate
  "
      ("d" anaconda-mode-find-definitions)
      ("a" anaconda-mode-find-assignments)
      ("r" xref-find-references)
      ("b" anaconda-mode-go-back)
      ("s" anaconda-mode-show-doc)
      ("y" yapfify-buffer)
      ("v" pythonic-activate)
      ("V" pythonic-deactivate)
      ("i" py-isort-buffer)
      ("w" pyvenv-activate)
      ("W" pyvenv-deactivate)
      ("f" hydra-flycheck/body)
      ("t" elpy-hydra/body)))

(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (let ((python-shell-completion-native-enable t)
        (python-shell-completion-native-output-timeout
          python-shell-completion-native-try-output-timeout))
     (python-shell-completion-native-get-completions
      (get-buffer-process (current-buffer))
      nil "_")))

(use-package yapfify :commands yapfify-buffer)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package smartparens
    :config
    (autoload 'smartparens-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'smartparens-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
    (add-hook 'ielm-mode-hook             #'smartparens-mode)
    (add-hook 'lisp-mode-hook             #'smartparens-mode)
    (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
    (add-hook 'scheme-mode-hook           #'smartparens-mode)
  :init
  (require 'smartparens-config)
  (defhydra hydra-smartparens (:hint nil)
    "
Sexps (quit with _q_)

^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _<left>_:    slurp forward   _R_:      splice
_b_: backward    _<right>_:   barf forward    _r_:      raise
_u_: backward ↑  _C-<left>_:  slurp backward  _<up>_:   raise backward
_d_: forward ↓   _C-<right>_: barf backward   _<down>_: raise forward
_p_: backward ↓
_n_: forward ↑

^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (a) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (a) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (a) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (a) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp)))

(use-package projectile
  :load-path "~/.emacs.d/packages/projectile"
  :init
  (evil-leader/set-key "p" 'projectile-command-map)
  :commands projectile-command-map
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (use-package helm-projectile
    :config
    (require 'helm-projectile)
    (helm-projectile-on)))

(use-package yasnippet
  :load-path "~/.emacs.d/packages/yasnippet"
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  :init
  (evil-leader/set-key
    "y" 'hydra-yasnippet/body)

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
	      ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
	 _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all)))

(use-package solarized-theme
  :config
  (evil-leader/set-key "tt" 'toggle-theme))
(load-theme 'solarized-light t)
(setq active-theme 'solarized-light)
(defun toggle-theme ()
  (interactive)
  (if (eq active-theme 'solarized-light)
      (setq active-theme 'solarized-dark)
    (setq active-theme 'solarized-light))
  (load-theme active-theme))
(set-face-attribute 'default t :font 
  "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :height 140)

(use-package company
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-anaconda))

(use-package helm-wordnet
  :commands helm-wordnet
  :load-path "packages/helm-wordnet"
  :config
  (setq helm-wordnet-prog "/usr/local/bin/wn"))
  (evil-leader/set-key
    "wd" 'helm-wordnet)

(use-package google-translate
  :commands (google-translate-at-point google-translate-smooth-translate)
  :config
  (setq google-translate-default-source-language "nl")
  (setq google-translate-default-target-language "en")
  (evil-leader/set-key
    "wp" 'google-translate-at-point
    "ww" 'google-translate-smooth-translate))

(use-package linum-relative
    :config
    (linum-relative-global-mode))
(setq column-number-mode t)

(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
    :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
    :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil)))

(use-package deft
  :config
  (setq deft-directory "~/org")
  (setq deft-extensions '("txt" "org"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (deft-find-file "/Users/andrew/org/agenda/gtd.org")
  (deft-find-file "/Users/andrew/org/agenda/PMI.org")
  (deft-find-file "/Users/andrew/org/agenda/projects.org"))

(use-package elfeed
  :commands elfeed
  :config
  (require 'elfeed-org)
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

(use-package elfeed-org :defer t)

(use-package keyfreq
  :config
  (require 'keyfreq)
  (setq keyfreq-excluded-commands
	'(self-insert-command
	  abort-recursive-edit
	  forward-char
	  backward-char
	  previous-line
	  next-line
	  evil-a-WORD
	  evil-append
	  evil-backward-char
	  evil-backward-word-begin
	  evil-change
	  evil-change-line
	  evil-complete-next
	  evil-complete-previous
	  evil-delete
	  evil-delete-backward-char-and-join
	  evil-delete-char
	  evil-delete-line
	  evil-emacs-state
	  evil-end-of-line
	  evil-escape-emacs-state
	  evil-escape-insert-state
	  evil-escape-isearch
	  evil-escape-minibuffer
	  evil-escape-motion-state
	  evil-escape-visual-state))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package restart-emacs
  :init
  (evil-leader/set-key "qr" 'restart-emacs)
  :commands restart-emacs)

(use-package neotree :load-path "~/.emacs.d/packages/neotree"
  :commands  neotree-toggle
  :config
  (require 'neotree)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

(use-package nov
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )
