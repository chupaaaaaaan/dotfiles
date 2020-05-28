(setq my:org-directory "~/org/")
(setq my:org-tag-alist '((:startgroup . nil)
                         ("read" . ?r)
                         ("study" . ?s)
                         ("develop" . ?d)
                         (:endgroup . nil)
                         ("chore" . ?c)))

(setq tag-search-habit   "+HABIT")
(setq tag-search-inbox   "+INBOX")
(setq tag-search-wip     "+WORK/+WIP|+WAIT")
(setq tag-search-todo    "+WORK/TODO")
(setq tag-search-pending "+WORK/+HOLDING|+PENDING")
(setq tag-search-project "+PROJECT/-DONE-CANCELED")
(setq tag-search-someday "+SOMEDAY/-DONE-CANCELED")
(setq tag-report-daily   "+CLOSED<\"<tomorrow>\"+CLOSED>=\"<today>\"")
(setq tag-report-weekly  "+CLOSED<\"<today>\"+CLOSED>=\"<-1w>\"")

(setq my:org-agenda-custom-commands
      '(("h" "Habits: 習慣タスク"
         tags-todo tag-search-habit ((org-agenda-overriding-header "Habit")
                                     (org-agenda-sorting-strategy
                                      '(todo-state-down effort-up category-keep))))
        ("i" "Agenda: 予定表"
         ((agenda    "" nil)
          (tags-todo tag-search-inbox
                     ((org-agenda-overriding-header "Inbox")
                      (org-tags-match-list-sublevels nil)))
          (tags-todo tag-search-wip
                     ((org-agenda-overriding-header "Work in progress")
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-sorting-strategy '(effort-up scheduled-up))))
          (tags-todo tag-search-todo
                     ((org-agenda-overriding-header "Next actions")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(scheduled-up))))
          (tags-todo tag-search-pending
                     ((org-agenda-overriding-header "Waiting")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(scheduled-up))))
          (tags tag-report-daily
                ((org-agenda-overriding-header "Daily Closed")
                 (org-agenda-sorting-strategy '(timestamp-down))))
          (tags tag-report-weekly
                ((org-agenda-overriding-header "Weekly Closed")
                 (org-agenda-sorting-strategy '(timestamp-down))))
          (tags-todo tag-search-project
                     ((org-agenda-overriding-header "Project")
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo tag-search-someday
                     ((org-agenda-overriding-header "Someday")
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-sorting-strategy '(category-keep))))
          nil))))




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
