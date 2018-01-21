;;; redditor-mode.el --- Browse reddit in Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/color-theme-ahungry
;; Version: 0.0.1
;; Keywords: ahungry palette color theme emacs color-theme deftheme
;; Package-Requires: ((emacs "25.1") (hierarchy "0.7.0") (request "0.3.0") (cl-lib "0") (dash "2.13.0"))

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
(require 'json)

(defvar redditor-mode--version "0.0.1"
  "The current version of the mode.")

(defvar redditor-mode--cache-comments nil
  "Store the most recent comment cache/fetch.")

(defvar redditor-mode--cache-subreddit
  (make-hash-table :test #'equal)
  "Store the most recent comment cache/fetch.")

(defvar redditor-mode--comments-composite nil)

(defvar redditor-mode--subreddit-composite
  (make-hash-table :test #'equal))

(defvar redditor-mode--fetch-comments-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((data (cl-getf data :data)))
       (setq redditor-mode--cache-comments data)
       (redditor-mode--comments-show)))))

(defvar redditor-mode--fetch-subreddit-callback
  (cl-function
   (lambda (subreddit &rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((my-data (cl-getf data :data)))
       (setf (gethash subreddit redditor-mode--cache-subreddit) my-data)
       (redditor-mode--subreddit-show)))))

(defvar redditor-mode--subreddit-url
  "https://www.reddit.com/r/%s.json")

(defvar redditor-mode--comment-url nil)

(defun redditor-mode--fetch-comments ()
  "Get a list of the comments on a thread."
  (request-response-data
   (request redditor-mode--comment-url
            :complete redditor-mode--fetch-comments-callback
            :sync nil
            :parser #'json-read
            :headers `(("User-Agent" . "fun")))))

(defun redditor-mode--fetch-subreddit (subreddit)
  "Get a list of the SUBREDDIT on a thread."
  (request-response-data
   (request (format redditor-mode--subreddit-url subreddit)
            :complete
            (cl-function
             (lambda (&rest data &allow-other-keys)
               (apply redditor-mode--fetch-subreddit-callback subreddit data)))
            :sync nil
            :parser #'json-read
            :headers `(("User-Agent" . "fun")))))

(defun redditor-mode--parse-comments-helper (comments)
  "Parse the comments that were fetched.

COMMENTS block is the nested list structure with them."
  (let-alist (alist-get 'data comments)
    (when (and .name .body .parent_id)
      (let ((composite (list (cons 'name (intern .name))
                             (cons 'body   .body)
                             (cons 'author .author)
                             (cons 'score  .score)
                             (cons 'parent_id (intern .parent_id)))))
        (push composite redditor-mode--comments-composite)))
    (when .children (redditor-mode--parse-comments .children))
    (when (and .replies
               (listp .replies))
      (redditor-mode--parse-comments-helper .replies))))

(defun redditor-mode--parse-subreddit-helper (subreddit-post subreddit)
  "Parse the subreddit that were fetched.

SUBREDDIT-POST is the actual post data submitted.
SUBREDDIT block is the nested list structure with them."
  (let-alist (alist-get 'data subreddit-post)
    (when (and .name .permalink)
      (let ((composite (list (cons 'name (intern .name))
                             (cons 'permalink    .permalink)
                             (cons 'num_comments .num_comments)
                             (cons 'author       .author)
                             (cons 'title        .title)
                             (cons 'selftext     .selftext)
                             (cons 'score        .score))))
        (push composite (gethash subreddit redditor-mode--subreddit-composite))))
    (when .children (redditor-mode--parse-subreddit .children subreddit))
    (when (and .replies
               (listp .replies))
      (redditor-mode--parse-subreddit-helper .replies subreddit))))

(defun redditor-mode--parse-comments (comments-vector)
  "Parse the cached comments and move to a hierarchy.

COMMENTS-VECTOR is a vector of comments."
  (mapcar #'redditor-mode--parse-comments-helper comments-vector))

(defun redditor-mode--parse-subreddit (subreddit-vector subreddit)
  "Parse the cached subreddit and move to a hierarchy.

SUBREDDIT-VECTOR is a vector of subreddit.
SUBREDDIT is the name of the subreddit."
  (mapcar (lambda (sub)
            (redditor-mode--parse-subreddit-helper sub subreddit))
          subreddit-vector))

(defun redditor-mode--parse-comments-from-cache ()
  "Parse comment structures from cache data."
  (setq redditor-mode--comments-composite nil)
  (redditor-mode--parse-comments redditor-mode--cache-comments)
  redditor-mode--comments-composite)

(defun redditor-mode--parse-subreddit-from-cache (subreddit)
  "Parse comment structures from cache data.

SUBREDDIT should be a valid subreddit."
  (setf (gethash subreddit redditor-mode--subreddit-composite) nil)
  (redditor-mode--parse-subreddit (list (gethash subreddit redditor-mode--cache-subreddit)) subreddit)
  (gethash subreddit redditor-mode--subreddit-composite))

(defun redditor-mode--find-comment-by-name (name)
  "Given NAME, find the corresponding comment."
  (cl-find-if
   (lambda (comment)
     (equal name (alist-get 'name comment)))
   redditor-mode--comments-composite))

(defun redditor-mode--find-subreddit-post-by-name (name)
  "Given NAME, find the corresponding subreddit-post."
  (let ((found nil))
    (maphash
     (lambda (_ hash-value)
       (let ((post-find
              (cl-find-if
               (lambda (subreddit-post)
                 (equal name (alist-get 'name subreddit-post)))
               hash-value)))
         (when post-find (setq found post-find))))
     redditor-mode--subreddit-composite)
    found))

(defvar redditor-mode--parentfn
  (lambda (name)
    (unless (equal 'thread name)
      (let ((parent-id
             (alist-get
              'parent_id
              (cl-find-if
               (lambda (comment)
                 (equal name (alist-get 'name comment)))
               redditor-mode--comments-composite))))
        (if parent-id parent-id 'thread)))))

(defgroup redditor-mode nil
  "Redditor Mode customization group."
  :group 'applications)

(defcustom redditor-mode-subreddits-active
  '(emacs lisp+Common_Lisp prolog)
  "List of subreddits you would like to subscribe to."
  :group 'redditor-mode
  :type (list 'symbol))

(defvar redditor-mode--hierarchy (hierarchy-new))

(defvar redditor-mode--subreddit-hierarchy (hierarchy-new))

(defun redditor-mode--comments-unique-ids (comments)
  "Get the unique IDs from both parent and name slots.

COMMENTS should be the ‘redditor-mode--comments-composite’.

If we want to date sort or something, this would probably be
the spot to do it as well."
  (-uniq
   (append
    (cl-loop
     for c in comments
     collect (alist-get 'name c))
    (cl-loop
     for c in comments
     collect (alist-get 'parent_id c)))))

(defun redditor-mode--hierarchy-build ()
  "Generate the comment structure."
  (setq redditor-mode--hierarchy (hierarchy-new))
  (hierarchy-add-tree redditor-mode--hierarchy 'thread redditor-mode--parentfn)
  (let ((comments (redditor-mode--parse-comments-from-cache)))
    (cl-loop
     for comment in (redditor-mode--comments-unique-ids comments)
     do (progn
          (hierarchy-add-tree
           redditor-mode--hierarchy
           comment
           redditor-mode--parentfn)))))

(defun redditor-mode--subreddit-hierarchy-build ()
  "Generate the subreddit-post structure."
  (setq redditor-mode--subreddit-hierarchy (hierarchy-new))
  (hierarchy-add-tree redditor-mode--subreddit-hierarchy 'subs (lambda (_) nil))
  (mapcar
   (lambda (subreddit)
     (hierarchy-add-tree redditor-mode--subreddit-hierarchy subreddit (lambda (_) 'subs))
     (let ((subreddit-posts (redditor-mode--parse-subreddit-from-cache subreddit)))
       (cl-loop
        for subreddit-post in subreddit-posts
        do (progn
             (hierarchy-add-tree
              redditor-mode--subreddit-hierarchy
              (alist-get 'name subreddit-post)
              (lambda (_) subreddit))))))
   redditor-mode-subreddits-active))

(defvar redditor-mode--hierarchy-labelfn-hooks nil)
(defvar redditor-mode--subreddit-hierarchy-labelfn-hooks nil)

(defun redditor-mode--hierarchy-labelfn-button (labelfn actionfn)
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
       for fn in redditor-mode--hierarchy-labelfn-hooks
       do (funcall fn item indent)))))

(defun redditor-mode--comments-show ()
  "Show the comments that were built in the structure."
  (interactive)
  (setq redditor-mode--hierarchy-labelfn-hooks
        '((lambda (item indent)
            (let ((comment (redditor-mode--find-comment-by-name item)))
              (when comment
                (let-alist comment
                  (insert
                   (format " (%s) → %s\n"
                           .score .body))))))))
  (redditor-mode--hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    redditor-mode--hierarchy
    (hierarchy-labelfn-indent
     (redditor-mode--hierarchy-labelfn-button
      (lambda (item _)
        (let ((comment (redditor-mode--find-comment-by-name item)))
          (if comment
              (insert
               (format "%s"
                       (alist-get 'author comment)))
            (insert (symbol-name item)))))
      (lambda (item _) (message "You clicked on: %s" item)))))))

(defun redditor-mode--subreddit-show ()
  "Show the subreddit-posts that were built in the structure."
  (interactive)
  (setq redditor-mode--hierarchy-labelfn-hooks
        '((lambda (item indent)
            (let ((subreddit-post (redditor-mode--find-subreddit-post-by-name item)))
              (when subreddit-post
                (let-alist subreddit-post
                  (insert
                   (format " (↑ %s / ☠ %s) by: %s"
                           .score .num_comments .author))))))))
  (redditor-mode--subreddit-hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    redditor-mode--subreddit-hierarchy
    (hierarchy-labelfn-indent
     (redditor-mode--hierarchy-labelfn-button
      (lambda (item _)
        (let ((subreddit-post (redditor-mode--find-subreddit-post-by-name item)))
          (if subreddit-post
              (insert
               (format "%s"
                       (alist-get 'title subreddit-post)))
            (insert (symbol-name item)))))
      (lambda (item _)
        (let* ((subreddit-post (redditor-mode--find-subreddit-post-by-name item))
               (permalink (alist-get 'permalink subreddit-post)))
          (setq redditor-mode--comment-url (format "http://reddit.com/%s.json" permalink)))
        (redditor-mode--fetch-comments)
        (message "Fetching: %s" redditor-mode--comment-url)))))))

;;;###autoload
(defun redditor-mode ()
  "Invoke the main mode."
  (interactive)
  ;; (redditor-mode--fetch-comments)
  (mapcar #'redditor-mode--fetch-subreddit redditor-mode-subreddits-active))

(provide 'redditor-mode)
;;; redditor-mode.el ends here
