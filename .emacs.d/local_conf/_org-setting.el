(setq my:org-directory "~/org/")
(setq my:org-tag-alist '((:startgroup . nil)
                         ("read" . ?r)
                         ("study" . ?s)
                         ("develop" . ?d)
                         (:endgroup . nil)
                         ("chore" . ?c)))
(setq my:org-capture-templates
      '(("tweet"      "セルフツイート" item  (file+headline ladicle/get-today-diary "Log")    "%(ladicle/org-get-time) %?\n" :prepend nil)
        ("memo"       "雑多なメモ"     entry (file+headline ladicle/get-today-diary "Memo")   "* %? :MEMO:\n"                :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
        ("inbox"      "タスクの作成"   entry (file+headline inbox-file "Inbox")               "* TODO %?\n  %U\n"            :empty-lines 1 :jump-to-captured nil)
        ("interrupt"  "割り込みタスク" entry (file+headline inbox-file "Inbox")               "* DONE %?\n  %U\n"            :empty-lines 1 :clock-in 1 :clock-resume 1)
        ("schedule"   "カレンダー"     entry (file+headline schedule-file "Schedule")         "* %?\n  SCHEDULED: <%(org-read-date)>\n  %U\n" :empty-lines 1)
        ("hack"       "ハックしたい事" item  (file+headline org-default-notes-file "Hacking") "[ ] %?"                       :prepend 1)
        ("link"       "リンクを追加"   item  (clock)                                          "%A\n"                         :immediate-finish 1 :prepend nil)))

;; ("daily-report"   "その日のまとめ"   entry (file+headline ladicle/get-today-diary "Memo") "* Report%?\n%(org-clock-report)\n"             :empty-lines 1)
;; ("books-memo"     "読書メモ"         entry (file+headline ladicle/get-today-diary "Memo") "* %? :BOOK:\n"                                 :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
;; ("wish-memo"      "欲しいものリスト" entry (file+headline inbox-file "My Wishes")         "* TODO %?"                                     :prepend 1)

;;
