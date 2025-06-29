;;; Handy functions for automatically creating drafts and publishing
;;; blog posts using my Hakyll setup.

(defun draft-post (name &optional publish)
  "Create a new draft blog post with the given name. If the post
already exists, open it up instead.

This creates the necessary directory structure and prepopulates a
generated index.md with a title and author field. All the actual
file munging is done by a simple Haskell script."
  (interactive "sName: ")
  (let ((flag (if publish "-p" "")))
    (message (format "Running `blog-post '%s' '%s' %s" name (sanitize-title name) flag))
    (shell-command (format "~/Programming/website/blog-post '%s' '%s' %s" name (sanitize-title name) flag)))
  (find-file (format "~/Programming/website/drafts/%s/index.md" (sanitize-title name)))
  (end-of-buffer))

(defun publish-post (name)
  "Publishes the post with the given name."
  (interactive "sName: ")
  (draft-post name t))

(defun sanitize-title (title)
  "Sanitize the title into a form that can be used in URLs and directory
names.

There is presumably some “correct” way to do it, but for now this just
explicitly handles the special cases I've run into in actual titles."
  (replace-regexp-in-string
   "," ""
   (replace-regexp-in-string " " "-" title)))
