(setq my:org-directory "~/org/")
(setq my:org-tag-alist '((:startgroup . nil)
                         ("design" . ?s)
                         ("develop" . ?d)
                         ("meeting" . ?m)
                         (:endgroup . nil)
                         (:startgroup . nil)
                         ("work" . ?w)
                         ("qanda" . ?q)
                         ("break" . ?b)
                         (:endgroup . nil)))

(setq my:org-agenda-custom-commands
      (list (list "h" "Habits: 習慣タスク"
                  'tags-todo "+HABIT"
                  '((org-agenda-overriding-header "Habit")
                    (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))

            (list "i" "Agenda: 予定表"
                  (list (list 'agenda "" nil)

                        (list 'tags-todo "-INBOX-HABIT/+NEXT"
                              '((org-agenda-overriding-header "Next Actions")
                                (org-tags-match-list-sublevels nil)
                                (org-agenda-sorting-strategy '(priority-down scheduled-up effort-up))))
                        nil))

            (list "p" "Tasks: タスク"
                  (list (list 'tags-todo "+INBOX"
                              '((org-agenda-overriding-header "Inbox")
                                (org-tags-match-list-sublevels nil)))

                        (list 'tags-todo "-INBOX-HABIT/-REFR-SOME-DONE-CANCELED"
                              '((org-agenda-overriding-header "Tasks")
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy '(category-keep))))
                        nil))

            (list "w" "Waiting for: 待ち状態"
                  (list (list 'tags-todo "-INBOX-HABIT/+WAIT"
                              '((org-agenda-overriding-header "Waiting for")
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy '(category-keep))))
                        nil))

            (list "r" "Reference: 参考資料など"
                  (list (list 'tags-todo "-INBOX-HABIT/+REFR"
                              '((org-agenda-overriding-header "Reference")
                                (org-tags-match-list-sublevels nil)))
                        nil))

            (list "y" "Someday: いつかやる/多分やる"
                  (list (list 'tags-todo "-INBOX-HABIT/+SOME"
                              '((org-agenda-overriding-header "Someday")
                                (org-agenda-sorting-strategy '(category-keep))
                                (org-tags-match-list-sublevels nil)))
                        nil))

            (list "d" "Done: 完了"
                  (list (list 'tags "+CLOSED<\"<tomorrow>\"+CLOSED>=\"<today>\""
                              '((org-agenda-overriding-header "Today's Done")
                                (org-tags-match-list-sublevels nil)))

                        (list 'tags "+CLOSED<\"<today>\"+CLOSED>=\"<yesterday>\""
                              '((org-agenda-overriding-header "Yesterday's Done")
                                (org-tags-match-list-sublevels nil)))

                        (list 'tags "+CLOSED<\"<yesterday>\"+CLOSED>=\"<-1w>\""
                              '((org-agenda-overriding-header "Weekly Done")
                                (org-tags-match-list-sublevels nil)))
                        nil))

            ))
(setq capture-templates-dir (concat my:org-directory "capture_templates/"))
(setq my:org-capture-templates
      (list ;; (list
            ;;  "tweet" "一言メモ" 'item
            ;;  '(file+headline ladicle/get-today-diary "Log")
            ;;  "%(ladicle/org-get-time) %?\n"
            ;;  :prepend nil)

            (list
             "diary" "日記" 'entry
             '(file+headline ladicle/get-today-diary "Diary")
             "* %?\n"
             :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)

            (list
             "inbox" "新規タスク" 'entry
             '(file inbox-file)
             (concat "%[" capture-templates-dir "inbox.org" "]")
             :empty-lines 1 :jump-to-captured nil)

            ;; (list
            ;;  "subproject" "サブプロジェクトを追加" 'entry
            ;;  '(file (chpn/org-get-current-file-name) (chpn/org-get-heading-title))
            ;;  todo-entry
            ;;  :empty-lines 1 :jump-to-captured nil)

            (list
             "interrupt" "突発作業" 'entry
             '(file inbox-file)
             (concat "%[" capture-templates-dir "interrupt.org" "]")
             :empty-lines 1 :clock-in 1 :clock-resume 1)

            (list
             "schedule" "予定作業" 'entry
             '(file schedule-file)
             (concat "%[" capture-templates-dir "schedule.org" "]")
             :empty-lines 1)

            (list
             "memo" "メモ・記録" 'plain
             '(file chpn/today-memo-string)
             (concat "%[" capture-templates-dir "memo.org" "]")
             :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)

            (list
             "break" "休憩" 'entry
             '(file inbox-file)
             "* DONE 休憩（%?）  :break:\n  %U\n"
             :empty-lines 1 :clock-in 1 :clock-resume 1)

            (list
             "link" "リンクを追加" 'item
             '(clock)
             "%A\n"
             :immediate-finish 1 :prepend nil)
            ))
