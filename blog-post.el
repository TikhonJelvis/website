;;; Handy functions for automatically creating drafts and publishing
;;; blog posts using my Hakyll setup.

(defun draft-post (name)
  "Create a new draft blog post with the given name. If the post
already exists, open it up instead.

This creates the necessary directory structure and prepopulates a
generated index.md with a title and author field. All the actual
file munging is done by a simple Haskell script."
  (interactive "sName: ")
  (message (format "Running `blog-post '%s'" name))
  (shell-command (format "blog-post.hs '%s'" name))
  (find-file (format "~/Public/website/drafts/%s/index.md" name))
  (end-of-buffer))
