;;; ps-utils.el --- Org utility functions -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'xml)
(require 'url)

(defcustom ps/org-link-title-max-length 200
  "Maximum title length before truncation with ellipsis."
  :type 'integer)

(defun ps/org--shorten (s)
  "Truncate string S to `ps/org-link-title-max-length' chars, appending \"...\"."
  (if (and s (> (length s) ps/org-link-title-max-length))
      (concat (substring s 0 ps/org-link-title-max-length) "...")
    s))

(defun ps/org--clean-title (s)
  "Decode ALL HTML entities (named, numeric, hex) and clean string S.
Returns nil if S is nil or empty after decoding."
  (when (and s (not (string-empty-p s)))
    (let ((decoded s)
          (limit 0))
      ;; Use a while loop with a safety limit to prevent infinite loops.
      ;; We process the string until no more &...; entities remain.
      (while (and (string-match "&\\([^;]+\\);" decoded) (< limit 10))
        (let* ((entity (match-string 1 decoded))
               (replacement
                (cond
                 ;; 1. Check Named Entities (e.g., &amp;)
                 ((assoc entity xml-entity-alist)
                  (let ((val (cdr (assoc entity xml-entity-alist))))
                    (if (characterp val) (string val) val)))

                 ;; 2. HTML specific fallbacks
                 ((string= entity "nbsp") " ")
                 ((string= entity "mdash") "—")

                 ;; 3. Hex entities: &#x...; (Safe length check)
                 ((and (string-prefix-p "#x" entity) (> (length entity) 2))
                  (condition-case nil
                      (string (string-to-number (substring entity 2) 16))
                    (error nil)))

                 ;; 4. Numeric entities: &#...; (Safe length check)
                 ((and (string-prefix-p "#" entity) (> (length entity) 1))
                  (condition-case nil
                      (string (string-to-number (substring entity 1)))
                    (error nil))))))

          ;; If a decoded replacement is itself a bare '&' (from &amp;,
          ;; &#38;, or &#x26;), defer it to the placeholder too. Otherwise
          ;; the fresh '&' could be re-consumed by a following entity match
          ;; (e.g. "&amp; &lt;" would mis-parse "& &lt;" as one entity).
          (setq decoded (if replacement
                            (replace-match (if (equal replacement "&")
                                               "AMP_INTERNAL"
                                             replacement)
                                           t t decoded)
                          ;; If we can't decode it, replace '&' with a temp char
                          ;; so we don't match this specific broken entity again
                          (replace-match "AMP_INTERNAL" t t decoded)))
          (setq limit (1+ limit))))

      ;; Final cleanup: Restore any un-decodable amps and strip non-printables
      (let* ((final-s (replace-regexp-in-string "AMP_INTERNAL" "&" decoded))
             (cleaned (replace-regexp-in-string
                       "[^\u0009\u000A\u000D\u0020-\u007E\u00A0-\uD7FF\uE000-\uFFFD]"
                       "" final-s)))
        (string-trim cleaned)))))

(provide 'ps-utils)
;;; ps-utils.el ends here
