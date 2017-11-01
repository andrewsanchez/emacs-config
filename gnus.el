(use-package gnus
  :commands gnus
  :config
    (add-to-list 'evil-emacs-state-modes 'gnus-group-mode)
    (setq send-mail-function (quote smtpmail-send-it))
    (setq gnus-select-method
	  '(nnimap "gmail"
		   (nnimap-address "imap.gmail.com")
		   (nnimap-server-port "993")
		   (nnimap-stream ssl)))
    (setq smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587
	  gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
	  gnus-message-archive-group nil)
	  ;; mml2015-encrypt-to-self 
    ;; Attempt to encrypt all outgoin emails
    ;; (add-hook 'message-setup-hook 'mml-secure-message-encrypt)
    (eval-after-load 'gnus-group
    '(progn
       (defhydra hydra-gnus-group (:color blue)
	 "Do?"
	 ("l" gnus-group-list-all-groups "List subscribed groups")
	 ("a" gnus-group-list-active "List groups on servers")
	 ("c" gnus-group-catchup-all "Read all")
	 ("G" gnus-group-make-nnir-group "Search server G G")
	 ("g" gnus-group-get-new-news "Refresh g")
	 ("s" gnus-group-enter-server-mode "Servers")
	 ("m" gnus-group-new-mail "Compose m OR C-x m")
	 ("#" gnus-topic-mark-topic "mark #")
	 ("q" nil "cancel"))
       (define-key gnus-group-mode-map "," 'hydra-gnus-group/body)))

  ;; gnus-summary-mode
  (eval-after-load 'gnus-sum
    '(progn
       (defhydra hydra-gnus-summary (:color blue)
	 "Do?"
	 ("s" gnus-summary-show-thread "Show thread")
	 ("h" gnus-summary-hide-thread "Hide thread")
	 ("n" gnus-summary-insert-new-articles "Refresh / N")
	 ("f" gnus-summary-mail-forward "Forward C-c C-f")
	 ("!" gnus-summary-tick-article-forward "Mail -> disk !")
	 ("p" gnus-summary-put-mark-as-read "Mail <- disk")
	 ("c" gnus-summary-catchup-and-exit "Read all c")
	 ("e" gnus-summary-resend-message-edit "Resend S D e")
	 ("R" gnus-summary-reply-with-original "Reply with original R")
	 ("r" gnus-summary-reply "Reply r")
	 ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
	 ("w" gnus-summary-wide-reply "Reply all S w")
	 ("#" gnus-topic-mark-topic "mark #")
	 ("q" nil "cancel"))
       (define-key gnus-summary-mode-map "," 'hydra-gnus-summary/body)))

  ;; gnus-article-mode
  (eval-after-load 'gnus-art
    '(progn
       (defhydra hydra-gnus-article (:color blue)
	 "Do?"
	 ("f" gnus-summary-mail-forward "Forward")
	 ("R" gnus-article-reply-with-original "Reply with original R")
	 ("r" gnus-article-reply "Reply r")
	 ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
	 ("o" gnus-mime-save-part "Save attachment at point o")
	 ("w" gnus-article-wide-reply "Reply all S w")
	 ("q" nil "cancel"))
       (define-key gnus-article-mode-map "," 'hydra-gnus-article/body)))

  (eval-after-load 'message
    '(progn (add-hook 'message-mode-hook 'flyspell-mode)
       (defhydra hydra-message (:color blue)
	 "Do?"
	 ("ca" mml-attach-file "Attach C-c C-a")
	 ("cc" message-send-and-exit "Send C-c C-c")
	 ("q" nil "cancel"))
       (global-set-key (kbd "C-c C-y") 'hydra-message/body))))
