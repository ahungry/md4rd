;;; redditor-mode.el --- Browse reddit in Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/color-theme-ahungry
;; Version: 0.0.1
;; Keywords: ahungry palette color theme emacs color-theme deftheme
;; Package-Requires: ((emacs "25") (hierarchy "0") (request "0") (cl-lib "0") (dash "0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See a ref for hierarchy-mode here:
;; https://github.com/DamienCassou/hierarchy

;;; Code:

(require 'hierarchy)
(require 'cl-lib)
(require 'dash)
(require 'request)

(defvar rm:version "0.0.1"
  "The current version of the mode.")

(defvar rm:cache-comments nil
  "Store the most recent comment cache/fetch.")

(defvar rm:cache-subreddit nil
  "Store the most recent comment cache/fetch.")

(defvar rm:comments-composite nil)
(defvar rm:subreddit-composite nil)

(defvar rm:fetch-comments-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((data (cl-getf data :data)))
       (setq rm:cache-comments data)
       (print rm:cache-comments)
       (message "rm: Comment fetch complete.")
       (rm:comments-show)
       )))
  )

(defvar rm:fetch-subreddit-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((data (cl-getf data :data)))
       (setq rm:cache-subreddit data)
       (print rm:cache-subreddit)
       (message "rm: Subreddit fetch complete.")
       (rm:subreddit-show)
       )))
  )

(defvar rm:subreddit-url
  "https://www.reddit.com/r/emacs.json")

(defvar rm:comment-url
  "https://www.reddit.com/r/emacs/comments/7rbzqx/rms_please_help_proofreading_the_emacs_manual.json")

(defun rm:fetch-comments ()
  "Get a list of the comments on a thread."
  (request-response-data
   (request rm:comment-url
            :complete rm:fetch-comments-callback
            :sync nil
            :parser 'json-read
            :headers `(("User-Agent" . "fun")))))

(defun rm:fetch-subreddit ()
  "Get a list of the subreddit on a thread."
  (request-response-data
   (request rm:subreddit-url
            :complete rm:fetch-subreddit-callback
            :sync nil
            :parser 'json-read
            :headers `(("User-Agent" . "fun")))))

(defun rm:parse-comments-helper (comments)
  "Parse the comments that were fetched.

COMMENTS block is the nested list structure with them."
  (let* ((data (assoc 'data comments))
         (name (assoc 'name data))
         (parent_id (assoc 'parent_id data))
         (body (assoc 'body data))
         (author (assoc 'author data))
         (score (assoc 'score data))
         (replies (assoc 'replies data))
         (children (assoc 'children data)))
    (when (and name body parent_id)
      (let ((composite
             `(
               (name . ,(intern (cdr name)))
               ,body
               ,author
               ,score
               (parent_id . ,(intern (cdr parent_id))))))
        (push composite rm:comments-composite)))
    (when children (rm:parse-comments (cdr children)))
    (when (and replies
               (cdr replies)
               (listp (cdr replies)))
      (rm:parse-comments-helper (cdr replies))
      )
    )
  )

(defun rm:parse-subreddit-helper (subreddit)
  "Parse the subreddit that were fetched.

SUBREDDIT block is the nested list structure with them."
  (let* ((data (assoc 'data subreddit))
         (name (assoc 'permalink data))
         (permalink (assoc 'permalink data))
         (num_comments (assoc 'num_comments data))
         (author (assoc 'author data))
         (title (assoc 'title data))
         (selftext (assoc 'selftext data))
         (score (assoc 'score data))
         (replies (assoc 'replies data))
         (children (assoc 'children data)))
    (when (and name permalink)
      (let ((composite
             `(
               (name . ,(intern (cdr name)))
               ,permalink
               ,num_comments
               ,author
               ,title
               ,selftext
               ,score)))
        (push composite rm:subreddit-composite)))
    (when children (rm:parse-subreddit (cdr children)))
    (when (and replies
               (cdr replies)
               (listp (cdr replies)))
      (rm:parse-subreddit-helper (cdr replies))
      )
    )
  )

(defun rm:parse-comments (comments-vector)
  "Parse the cached comments and move to a hierarchy.

COMMENTS-VECTOR is a vector of comments."
  (mapcar #'rm:parse-comments-helper comments-vector))

(defun rm:parse-subreddit (subreddit-vector)
  "Parse the cached subreddit and move to a hierarchy.

SUBREDDIT-VECTOR is a vector of subreddit."
  (mapcar #'rm:parse-subreddit-helper subreddit-vector))

(defun rm:parse-comments-from-cache ()
  "Parse comment structures from cache data."
  (setq rm:comments-composite nil)
  (rm:parse-comments rm:cache-comments)
  rm:comments-composite
  )

(defun rm:parse-subreddit-from-cache ()
  "Parse comment structures from cache data."
  (setq rm:subreddit-composite nil)
  (rm:parse-subreddit (list rm:cache-subreddit))
  rm:subreddit-composite
  )

(defun rm:find-comment-by-name (name)
  "Given NAME, find the corresponding comment."
  (cl-find-if
   (lambda (comment)
     (equal name (cdr (assoc 'name comment))))
   rm:comments-composite))

(defun rm:find-subreddit-post-by-name (name)
  "Given NAME, find the corresponding subreddit-post."
  (cl-find-if
   (lambda (subreddit-post)
     (equal name (cdr (assoc 'name subreddit-post))))
   rm:subreddit-composite))

(defvar rm:parentfn
  (lambda (name)
    (unless (equal 'thread name)
      (let ((parent-id
             (cdr (assoc
                   'parent_id
                   (cl-find-if
                    (lambda (comment)
                      (equal name (cdr (assoc 'name comment))))
                    rm:comments-composite)))))
        (if parent-id parent-id 'thread)))
    ))

(defvar rm:subreddit-parentfn
      (lambda (name)
        (unless (equal name 'thread) 'thread)))

(defvar rm:hierarchy (hierarchy-new))

(defvar rm:subreddit-hierarchy (hierarchy-new))

(defun rm:comments-unique-ids (comments)
  "Get the unique IDs from both parent and name slots.

COMMENTS should be the rm:comments-composite.

If we want to date sort or something, this would probably be
the spot to do it as well."
  (-uniq
   (append
    (cl-loop
     for c in comments
     collect (cdr (assoc 'name c)))
    (cl-loop
     for c in comments
     collect (cdr (assoc 'parent_id c)))))
  )

(defun rm:hierarchy-build ()
  "Generate the comment structure."
  (setq rm:hierarchy (hierarchy-new))
  (hierarchy-add-tree rm:hierarchy 'thread rm:parentfn)
  (let ((comments (rm:parse-comments-from-cache)))
    (cl-loop
     for comment in (rm:comments-unique-ids comments)
     do (progn
          (hierarchy-add-tree
           rm:hierarchy
           comment
           rm:parentfn))))
  )

(defun rm:subreddit-hierarchy-build ()
  "Generate the subreddit-post structure."
  (setq rm:subreddit-hierarchy (hierarchy-new))
  (hierarchy-add-tree rm:subreddit-hierarchy 'thread rm:subreddit-parentfn)
  (let ((subreddit-posts (rm:parse-subreddit-from-cache)))
    (cl-loop
     for subreddit-post in subreddit-posts
     do (progn
          (print (cdr (assoc 'name subreddit-post)))
          (hierarchy-add-tree
           rm:subreddit-hierarchy
           (cdr (assoc 'name subreddit-post))
           rm:subreddit-parentfn)
          )))
  )

;; (defun rm:hierarchy-build ()
;;   "Generate the comment structure."
;;   (let ((parentfn
;;          (lambda (item)
;;            (cl-case item
;;              (dove 'bird)
;;              (pigeon 'bird)
;;              (bird 'animal)
;;              (dolphin 'animal)
;;              (cow 'animal)))))
;;     (hierarchy-add-tree rm:hierarchy 'dove parentfn)
;;     (hierarchy-add-tree rm:hierarchy 'pigeon parentfn)
;;     (hierarchy-add-tree rm:hierarchy 'dolphin parentfn)
;;     (hierarchy-add-tree rm:hierarchy 'cow parentfn)
;;     ))

;; (hierarchy-sort animals)
;; (hierarchy-roots animals)
;; (hierarchy-leafs animals)

;; (switch-to-buffer
;;  (hierarchy-tabulated-display
;;   animals
;;   (hierarchy-labelfn-indent
;;    (hierarchy-labelfn-button
;;     (lambda (item _) (insert (symbol-name item)))
;;     (lambda (item _) (message "You clicked on: %s" item))))))

;; (switch-to-buffer
;;  (hierarchy-tree-display
;;   animals
;;   (lambda (item _) (insert (symbol-name item)))))

(defvar rm:hierarchy-labelfn-hooks nil)
(defvar rm:subreddit-hierarchy-labelfn-hooks nil)

(defun rm:hierarchy-labelfn-button (labelfn actionfn)
  "Return a function rendering LABELFN in a button.

Clicking the button triggers ACTIONFN.  ACTIONFN is a function
taking an item of HIERARCHY and an indentation value (a number)
as input.  This function is called when an item is clicked.  The
return value of ACTIONFN is ignored."
  (lambda (item indent)
    (let ((start (point)))
      (funcall labelfn item indent)
      (make-text-button start (point)
                        'action (lambda (_) (funcall actionfn item indent)))
      (cl-loop
       for fn in rm:hierarchy-labelfn-hooks
       do (funcall fn item indent)))))

(defun rm:comments-show ()
  "Show the comments that were built in the structure."
  (interactive)
  (setq rm:hierarchy-labelfn-hooks
        '(
          (lambda (item indent)
            (let ((comment (rm:find-comment-by-name item)))
              (when comment
                (insert
                 (format
                  " (%s) → %s\n"
                  (cdr (assoc 'score comment))
                  (cdr (assoc 'body comment)))))))))
  (rm:hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    rm:hierarchy
    (hierarchy-labelfn-indent
     (rm:hierarchy-labelfn-button
      (lambda (item _)
        (let ((comment (rm:find-comment-by-name item)))
          (if comment
              (insert
               (format "%s"
                       (cdr (assoc 'author comment))))
            (insert (symbol-name item))
            )))
      (lambda (item _) (message "You clicked on: %s" item)))))))

(defun rm:subreddit-show ()
  "Show the subreddit-posts that were built in the structure."
  (interactive)
  (setq rm:hierarchy-labelfn-hooks
        '(
          (lambda (item indent)
            (let ((subreddit-post (rm:find-subreddit-post-by-name item)))
              (when subreddit-post
                (insert
                 (format
                  " (↑ %s / ☠ %s) by: %s"
                  (cdr (assoc 'score subreddit-post))
                  (cdr (assoc 'num_comments subreddit-post))
                  (cdr (assoc 'author subreddit-post)))))))))
  (rm:subreddit-hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    rm:subreddit-hierarchy
    (hierarchy-labelfn-indent
     (rm:hierarchy-labelfn-button
      (lambda (item _)
        (let ((subreddit-post (rm:find-subreddit-post-by-name item)))
          (if subreddit-post
              (insert
               (format "%s"
                       (cdr (assoc 'title subreddit-post))))
            (insert (symbol-name item))
            )))
      (lambda (item _)
        (setq rm:comment-url (format "http://reddit.com/%s.json" (symbol-name item)))
        (rm:fetch-comments)
        (message "Fetching: %s" rm:comment-url)))))))

;;;###autoload
(defun redditor-mode ()
  "Invoke the main mode."
  (interactive)
  ;; (rm:fetch-comments)
  (rm:fetch-subreddit))

(provide 'redditor-mode)
;;; redditor-mode.el ends here
