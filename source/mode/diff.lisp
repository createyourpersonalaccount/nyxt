;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/diff-mode
    (:documentation "Mode for viewing diffs between two buffers."))
(in-package :nyxt/diff-mode)

(export-always 'diff-mode)
(define-mode diff-mode ()
  "Diff mode is used to view the diffs between two buffers."
  ((rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            (".nyxt-diff-insert"
             :background-color "#bbeabb"
             :text-decoration "none")
            ("ins.nyxt-diff-replace"
             :background-color "#bbeabb"
             :text-decoration "none")
            (".nyxt-diff-delete"
             :background-color "#efcbcf"
             :text-decoration "none")
            ("del.nyxt-diff-replace"
             :background-color "#efcbcf"
             :text-decoration "none"))
          ;; FIXME Add these colors to the theme.  Check if they work well with
          ;; dark themes.
          :documentation "Diff colours for its visual representation.
They're based on the modus-operandi theme by Protesilaos Stavrou, which follows
the highest standard on accessibility."))
  (:toggler-command-p nil))

(define-command-global diff (&optional (url "diff://"))
  "Show difference between two buffers."
  ;; the new buffer doesn't get focus, why?
  (let ((buffer (make-buffer :url url
                             :title "Diff"
                             :load-url-p nil
                             :extra-modes '(diff-mode))))
    (set-current-buffer (buffer-load url :buffer buffer))))

(define-internal-scheme "diff"
    (lambda (url buffer)
      (declare (ignore url))
      (let ((old-html (ffi-buffer-get-document (prompt1
                                                :prompt "Old buffer"
                                                :sources (make-instance
                                                          'buffer-source
                                                          :constructor (nyxt::sort-by-time
                                                                        (remove-if
                                                                         (match-scheme "diff")
                                                                         (buffer-list)))
                                                          :multi-selection-p nil
                                                          :return-actions nil))))
            (new-html (ffi-buffer-get-document (prompt1
                                                :prompt "New buffer"
                                                :sources (make-instance
                                                          'buffer-source
                                                          :constructor (alex:rotate
                                                                        (nyxt::sort-by-time
                                                                         (remove-if
                                                                          (match-scheme "diff")
                                                                          (buffer-list)))
                                                                        -1)
                                                          :multi-selection-p nil
                                                          :return-actions nil)))))
        (enable-modes '(diff-mode) buffer)
        (spinneret:with-html-string
          (:style (style (find-submode 'nyxt/diff-mode:diff-mode buffer)))
          (:raw (html-diff:html-diff old-html
                                     new-html
                                     :insert-class "nyxt-diff-insert"
                                     :delete-class "nyxt-diff-delete"
                                     :replace-class "nyxt-diff-replace")))))
  :secure-p t)
