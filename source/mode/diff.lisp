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

(define-class open-urls-source (prompter:source)
  ((prompter:name "Currently open URLs")
   (prompter:constructor (mapcar #'url (buffer-list))))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((uri quri:uri) (source prompter:source))
  `("URL" ,(render-url uri)))

(define-command-global diff (&key (diffable-url-sources
                                   (list (make-instance 'open-urls-source)
                                         (make-instance 'global-history-source
                                                        :return-actions (list (lambda-mapped-command url))
                                                        :multi-selection-p nil)))
                             (old-url (prompt1 :prompt "Old URL"
                                               :sources diffable-url-sources))
                             (new-url (prompt1 :prompt "New URL"
                                               :sources diffable-url-sources)))
  "Show the difference between the OLD-URL and NEW-URL.

DIFFABLE-URL-SOURCES allow you to configure the sources URLs are selected from."
  (when (and old-url new-url)
    (make-buffer-focus
     :url (format nil "diff:?old=~a&new=~a"
                  (quri:url-encode (quri:render-uri old-url))
                  (quri:url-encode (quri:render-uri new-url))))))

(defun fetch-url-source (url)
  (alex:if-let ((existing-buffer
                 (find url (remove-if (lambda (buffer)
                                        (or (url-empty-p (url buffer))
                                            (not (valid-url-p (url buffer)))))
                                      (buffer-list))
                       :test #'quri:uri= :key #'url)))
    (ffi-buffer-get-document existing-buffer)
    (let ((channel (nyxt::make-channel 1)))
      (run-thread "diff-mode URL fetching"
        (let ((buffer (make-background-buffer :url url)))
          (hooks:add-hook
           (buffer-loaded-hook buffer)
           (make-instance 'hooks:handler
                          :fn (lambda (buffer)
                                (calispel:! channel (ffi-buffer-get-document buffer))
                                (hooks:remove-hook (buffer-loaded-hook buffer)
                                                   'buffer-source-fetching)
                                (ffi-buffer-delete buffer))
                          :name 'buffer-source-fetching))))
      (calispel:? channel))))

(define-internal-scheme "diff"
    (lambda (url buffer)
      (let* ((params (quri:uri-query-params (quri:uri url)))
             (old-url (quri:uri (quri:url-decode (str:s-assoc-value params "old"))))
             (new-url (quri:uri (quri:url-decode (str:s-assoc-value params "new"))))
             (old-html (fetch-url-source old-url))
             (new-html (fetch-url-source new-url))
             (diff (html-diff:html-diff old-html new-html
                                        :insert-class "nyxt-diff-insert"
                                        :delete-class "nyxt-diff-delete"
                                        :replace-class "nyxt-diff-replace"))
             (diff-dom (nyxt/dom:named-html-parse diff)))
        (loop for element across (clss:select "[href], [src]" diff-dom)
              when (plump:attribute element "href")
                do (plump:set-attribute
                    element "href"
                    (quri:render-uri
                     (quri:merge-uris (quri:uri (plump:attribute element "href")) new-url)))
              when (plump:attribute element "src")
                do (plump:set-attribute
                    element "src"
                    (quri:render-uri
                     (quri:merge-uris (quri:uri (plump:attribute element "src")) new-url))))
        (enable-modes '(diff-mode) buffer)
        (spinneret:with-html-string
          (:style (style (find-submode 'nyxt/diff-mode:diff-mode buffer)))
          (:raw (plump:serialize diff-dom nil))))))
