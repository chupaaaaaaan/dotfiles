(setq my:org-directory "~/org/")
(setq my:org-tag-alist '((:startgroup . nil)
                         ("read" . ?r)
                         ("study" . ?s)
                         ("develop" . ?d)
                         (:endgroup . nil)
                         ("chore" . ?c)))

(setq tag-search-habit   "+HABIT")
(setq tag-search-inbox   "+INBOX")
(setq tag-search-todo    "+WORK/TODO")
(setq tag-search-wip     "-INBOX-HABIT/+WIP")
(setq tag-search-note    "-INBOX-HABIT/+NOTE")
(setq tag-search-pending "-INBOX-HABIT/+HOLDING|+PENDING")
(setq tag-search-project "+PROJECT/-DONE-CANCELED")
(setq tag-search-someday "+SOMEDAY/-DONE-CANCELED")
(setq tag-report-daily   "+CLOSED<\"<tomorrow>\"+CLOSED>=\"<today>\"")
(setq tag-report-weekly  "+CLOSED<\"<today>\"+CLOSED>=\"<-1w>\"")

(setq my:org-agenda-custom-commands
      (list (list "h" "Habits: 習慣タスク"
                  'tags-todo tag-search-habit '((org-agenda-overriding-header "Habit")
                                                (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
            (list "i" "Agenda: 予定表"
                  (list (list 'agenda "" nil)
                        (list 'tags-todo tag-search-inbox
                              '((org-agenda-overriding-header "Inbox")
                                (org-tags-match-list-sublevels nil)))
                        (list 'tags-todo tag-search-wip
                              '((org-agenda-overriding-header "Work in progress")
                                (org-tags-match-list-sublevels nil)
                                (org-agenda-sorting-strategy '(priority-down scheduled-up effort-up))))
                        (list 'tags-todo tag-search-todo
                              '((org-agenda-overriding-header "Next actions")
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy '(priority-down scheduled-up))))
                        (list 'tags-todo tag-search-project
                              '((org-agenda-overriding-header "Project")
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy '(category-keep))))
                        (list 'tags-todo tag-search-note
                              '((org-agenda-overriding-header "Documents/Notes")
                                (org-tags-match-list-sublevels nil)))
                        (list 'tags-todo tag-search-pending
                              '((org-agenda-overriding-header "Waiting")
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy '(scheduled-up))))
                        (list 'tags-todo tag-search-someday
                              '((org-agenda-overriding-header "Someday")
                                (org-tags-match-list-sublevels nil)
                                (org-agenda-sorting-strategy '(category-keep))))
                        ;; (list 'tags tag-report-daily
                        ;;       '((org-agenda-overriding-header "Daily Closed")
                        ;;         (org-agenda-sorting-strategy '(timestamp-down))))
                        ;; (list 'tags tag-report-weekly
                        ;;       '((org-agenda-overriding-header "Weekly Closed")
                        ;;         (org-agenda-sorting-strategy '(timestamp-down))))
                        nil))))

(setq sche "  SCHEDULED: <%(org-read-date)>\n")
(setq scht "  SCHEDULED: <%(org-read-date t)>\n")
(setq schd "  DEADLINE: <%(org-read-date)>\n")
(setq pbgn "  :PROPERTIES:\n")
(setq pend "  :END:\n")
(setq peff "  :Effort: %(org-read-property-value \"Effort\")\n")

(setq todo-entry     (concat "* TODO [#C] %?\n" pbgn peff pend "  %U\n"))
(setq schedule-entry (concat "* %?\n" scht "  %U\n"))

(setq my:org-capture-templates
      (list (list
             "tweet" "一言メモ" 'item
             '(file+headline ladicle/get-today-diary "Log")
             "%(ladicle/org-get-time) %?\n"
             :prepend nil)

            (list
             "diary" "日記" 'entry
             '(file+headline ladicle/get-today-diary "Diary")
             "* %?\n"
             :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)

            (list
             "memo" "メモ・記録" 'entry
             '(file chpn/today-memo-string)
             "* %?\n"
             :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)

            (list
             "inbox" "タスクの作成" 'entry
             '(file inbox-file)
             todo-entry
             :empty-lines 1 :jump-to-captured nil)

            (list
             "interrupt" "割り込み作業" 'entry
             '(file inbox-file)
             "* TODO [#B] %?\n  %U\n"
             :empty-lines 1 :clock-in 1 :clock-resume 1)

            (list
             "schedule" "カレンダー" 'entry
             '(file inbox-file)
             schedule-entry
             :empty-lines 1)

            (list
             "chore" "雑務・休憩など" 'entry
             '(file inbox-file)
             "* DONE :chore:%?\n  %U\n"
             :empty-lines 1 :clock-in 1 :clock-resume 1)

            (list
             "link" "リンクを追加" 'item
             '(clock)
             "%A\n"
             :immediate-finish 1 :prepend nil)
            ))
