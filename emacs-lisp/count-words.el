(defun count-words-result (count)
  (cond ((zerop count)
         (message "The region does NOT have any words."))
        ((= 1 count)
         (message "The region has 1 word."))
        (t
         (message "The region has %d words." count))))

(defun count-words-example (beginning end)
  (interactive "r")
  (message "Counting words in region ...")
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      (count-words-result count))))

(defun count-words-recursive (beginning end)
  (interactive "r")
  (message "Counting words in region ...")
  (save-excursion
    (goto-char beginning)
    (count-words-result (count-words-recursive-internal end))))

(defun count-words-recursive-internal (end)
  (if (and (< (point) end)
           (re-search-forward "\\w+\\W*" end t))
      (1+ (count-words-recursive-internal end))
    0))
