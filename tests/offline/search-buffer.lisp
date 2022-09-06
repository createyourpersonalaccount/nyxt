(in-package :nyxt/tests)
(use-nyxt-package-nicknames)

(define-test search-buffer ()
  (let ((html-doc "
<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"utf-8\">
    <title>Website</title>
  </head>
  <body>
    <header>
      <h1>Title1</h1>
    </header>
    <h2>Title2</h2>
    <p>Test.</p>
    <ul>
      <li>Test item.</li>
      <li>Test item.</li>
      <li>Test item.</li>
      <li>Test item.</li>
      <li>Test item.</li>
    </ul>
    <h3>Title3</h3>
    <p>Test.</p>
    <p>Test.</p>
    <h2>Title2</h2>
    <p>Test.</p>
    <h3>Title3</h3>
    <p>Test.</p>
    <h2>Title2</h2>
    <p>Test.</p>
    <script>whatever-javascript;</script>
  </body>
</html>"))
  (assert-eq
   11
   (nyxt/dom:find-text "test"
                       (elt (clss:select "body" (plump:parse html-doc)) 0)
                       :test #'search))))
