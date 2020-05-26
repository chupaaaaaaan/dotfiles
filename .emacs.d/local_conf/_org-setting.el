(setq my:org-directory "~/org/")
(setq my:org-tag-alist '((:startgroup . nil)
                         ("read" . ?r)
                         ("study" . ?s)
                         ("develop" . ?d)
                         (:endgroup . nil)
                         ("chore" . ?c)))
(setq sche "  SCHEDULED: <%(org-read-date)>\n")
(setq scht "  SCHEDULED: <%(org-read-date t)>\n")
(setq dead "  DEADLINE: <%(org-read-date)>\n")
(setq pbgn "  :PROPERTIES:\n")
(setq pend "  :END:\n")
(setq peff "  :Effort: %(org-read-property-value \"Effort\")\n")

(setq todo-entry     (concat "* TODO %?\n" sche pbgn peff pend "  %U\n"))
(setq schedule-entry (concat "* %?\n" scht "  %U\n"))

(setq my:org-capture-templates
      (list (list
             "tweet" "セルフツイート" 'item
             '(file+headline ladicle/get-today-diary "Log")
             "%(ladicle/org-get-time) %?\n"
             :prepend nil)

            (list
             "memo" "雑多なメモ" 'entry
             '(file+headline ladicle/get-today-diary "Memo")
             "* %? :MEMO:\n"
             :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)

            (list
             "inbox" "タスクの作成" 'entry
             '(file+headline inbox-file "Inbox")
             todo-entry
             :empty-lines 1 :jump-to-captured nil)

            (list
             "interrupt" "割り込みタスク" 'entry
             '(file+headline inbox-file "Inbox")
             "* TODO %?\n  %U\n"
             :empty-lines 1 :clock-in 1 :clock-resume 1)

            (list
             "schedule" "カレンダー" 'entry
             '(file+headline schedule-file "Schedule")
             schedule-entry
             :empty-lines 1)

            (list
             "hack-emacs" "Emacsをハック" 'checkitem
             '(file+headline org-default-notes-file "Hacking Emacs")
             "[ ] %?"
             :prepend 1)

            (list
             "link" "リンクを追加" 'item
             '(clock)
             "%A\n"
             :immediate-finish 1 :prepend nil)
            ))
