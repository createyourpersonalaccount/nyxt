;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/search-buffer-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/search-buffer-mode)

(define-mode search-buffer-mode ()
  "Mode for searching text within the buffer."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (test-function
    #'search-string
    :type function
    :documentation "The function that determines whether a search match is found.

You can redefine it to enable regex-based search, for example:
\(define-configuration nyxt/search-buffer-mode:search-buffer-mode
  ((nyxt/search-buffer-mode:test-function #'cl-ppcre:scan)))")
   (style
    (theme:themed-css (theme *browser*)
      (".nyxt-search-hint"
       :background-color (str:concat theme:secondary " !important")
       :color (str:concat theme:on-secondary " !important")
       :font-family "monospace,monospace"
       :padding "0px 0.3em"
       :border-radius "0.3em"
       :z-index #.(1- (expt 2 31)))
      (".nyxt-select-search-hint"
       :background-color (str:concat theme:accent " !important")
       :color (str:concat theme:on-accent " !important")))
    :documentation "The style of the search overlays.")
   (keyscheme-map
    (define-keyscheme-map "search-buffer-mode" ()
      keyscheme:cua
      (list
       "C-f" 'search-buffer
       "f3" 'search-buffer
       "M-f" 'remove-search-hints)
      keyscheme:emacs
      (list
       "C-s s" 'search-buffer
       "C-s k" 'remove-search-hints)
      keyscheme:vi-normal
      (list
       "/" 'search-buffer
       "?" 'remove-search-hints)))))

(defun search-string (substring s &key case-sensitive-p)
  (let ((test (if case-sensitive-p #'string= #'string-equal)))
    (search substring s :test test)))

(define-class search-match ()
  ((identifier)
   (element)
   (pos)
   (body)
   (buffer))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nyxt/hint-mode:identifier ((match search-match))
  (identifier match))

(defmethod prompter:object-attributes ((match search-match) (source prompter:source))
  `(("Text" ,(let ((long-match (> (pos match) 40)))
               (uiop:strcat
                (when long-match
                  "...")
                (subseq (body match)
                        (if long-match
                            (alex:if-let ((pos (position #\Space (body match) :end (- (pos match) 20) :from-end t)))
                              (1+ pos)
                              0)
                            0)))))
    ("Buffer ID" ,(princ-to-string (id (buffer match))))
    ("Buffer title" ,(title (buffer match)))))

(define-parenscript-async hint-elements (selectors)
  (dolist (selector (ps:lisp selectors))
    (ps:let* ((element (nyxt/ps:qs document selector)))
      (ps:try
       (ps:chain element class-list (add "nyxt-search-hint"))
       (:catch (error))))))

(define-parenscript highlight-selected-hint (&key element scroll)
  (ps:let* ((element (ps:@ (nyxt/ps:qs document (ps:lisp (nyxt/dom:get-unique-selector element))))))
    (when element
      (unless (ps:chain element class-list (contains "nyxt-select-search-hint"))
        (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-select-search-hint")))
          (ps:dolist (e old-elements)
            (ps:chain e class-list (remove "nyxt-select-search-hint")))))
      (ps:chain element class-list (add "nyxt-select-search-hint"))
      (when (ps:lisp scroll)
        (ps:chain element (scroll-into-view (ps:create block "center")))))))

(define-parenscript remove-focus ()
  (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-search-highlight-hint")))
    (ps:dolist (e old-elements)
      (ps:chain e class-list (remove "nyxt-highlight-search-hint"))
      (ps:chain e class-list (add "nyxt-search-hint")))))

(define-command remove-search-hints ()
  "Remove all search hints."
  (ps-eval (ps:dolist (element (nyxt/ps:qsa document ".nyxt-search-hint"))
             (ps:chain element class-list (remove "nyxt-search-hint")))))

(defun add-search-hints (input buffer)
  (let* ((input (str:replace-all " " " " input))
         (test (test-function (find-submode 'search-buffer-mode buffer)))
         (elements (nyxt/dom:find-text
                    input (elt (clss:select "body" (document-model buffer)) 0)
                    :test test))
         (search-matches (mapcar (lambda (element)
                                   (make-instance 'search-match
                                                  :identifier (nyxt/dom:get-unique-selector element)
                                                  :element element
                                                  :pos (funcall test input (plump:text element))
                                                  :body (plump:text element)
                                                  :buffer buffer))
                                 elements)))
    (remove-search-hints)
    (with-current-buffer buffer
      (add-stylesheet (style (find-submode 'search-buffer-mode)))
      (hint-elements (map 'vector #'identifier search-matches))
      search-matches)))

(define-class search-buffer-source (prompter:source)
  ;; FIXME case-sensitive-p
  ((case-sensitive-p nil)
   (buffer (current-buffer))
   (minimum-search-length 3)
   (prompter:name "Search buffer")
   (prompter:selection-actions-enabled-p t)
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      (if (>= (length input) (minimum-search-length source))
          (add-search-hints input (buffer source))
          (progn
            (remove-search-hints)
            '()))))
   (prompter:selection-actions
    (lambda (suggestion)
      (set-current-buffer (buffer suggestion) :focus nil)
      (with-current-buffer (buffer suggestion)
        (highlight-selected-hint :element (element suggestion)
                                 :scroll t))))
   (prompter:destructor
    (lambda (prompter source)
      (declare (ignore prompter source))
      (unless (keep-search-hints-p (current-buffer))
        (remove-search-hints)))))
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(defmethod initialize-instance :after ((source search-buffer-source) &key)
  (setf (prompter:name source)
        (format nil "~a (~a+ characters)"
                (prompter:name source)
                (minimum-search-length source))))

(define-command search-buffer (&key case-sensitive-p)
  "Search on the current buffer.
If you want to remove the search hints when you close the search
prompt, Set BUFFER's `keep-search-hints-p' slot to nil.

Example:

  (define-configuration buffer
    ((keep-search-hints-p nil)))"
  (prompt :prompt "Search text"
          :sources (make-instance 'search-buffer-source
                                  :case-sensitive-p case-sensitive-p
                                  :return-actions
                                  (list (lambda (search-match)
                                          (unless (keep-search-hints-p (current-buffer))
                                            (remove-search-hints))
                                          search-match)))))

(define-command search-buffers (&key case-sensitive-p)
  "Search multiple buffers."
  (let ((buffers (prompt :prompt "Search buffer(s)"
                         :sources (make-instance 'buffer-source ; TODO: Define class?
                                                 :return-actions '()
                                                 :multi-selection-p t))))
    (prompt
     :prompt "Search text"
     :sources (mapcar (lambda (buffer)
                        (make-instance 'search-buffer-source
                                       :name (format nil "Search ~a" (if (url-empty-p (url buffer))
                                                                         (title buffer)
                                                                         (url buffer)))
                                       :case-sensitive-p case-sensitive-p
                                       :buffer buffer))
                      buffers))))
