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
    (message (format "Running `blog-post '%s' %s" name flag))
    (shell-command (format "blog-post '%s' %s" name flag)))
  (find-file (format "~/Public/drafts/%s/index.md" (replace-regexp-in-string " " "-" name)))
  (end-of-buffer))

(defun publish-post (name)
  "Publishes the post with the given name."
  (interactive "sName: ")
  (draft-post name t))
