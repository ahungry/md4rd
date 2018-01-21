;;; redditor.el --- Browse reddit. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/color-theme-ahungry
;; Version: 0.0.1
;; Keywords: ahungry reddit browse news
;; Package-Requires: ((emacs "25.1") (hierarchy "0.7.0") (request "0.3.0") (cl-lib "0.6.1") (dash "2.12.0"))

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

(defvar redditor--version "0.0.1"
  "The current version of the mode.")

(defvar redditor--cache-comments nil
  "Store the most recent comment cache/fetch.")

(defvar redditor--cache-subreddit
  (make-hash-table :test #'equal)
  "Store the most recent comment cache/fetch.")

(defvar redditor--comments-composite nil)

(defvar redditor--subreddit-composite
  (make-hash-table :test #'equal))

(defvar redditor--fetch-comments-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((data (plist-get data :data)))
       (setq redditor--cache-comments data)
       (redditor--comments-show)))))

(defvar redditor--fetch-subreddit-callback
  (cl-function
   (lambda (subreddit &rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((my-data (plist-get data :data)))
       (setf (gethash subreddit redditor--cache-subreddit) my-data)
       (redditor--subreddit-show)))))

(defvar redditor--subreddit-url
  "https://www.reddit.com/r/%s.json")

(defvar redditor--comment-url nil)

(defun redditor--fetch-comments ()
  "Get a list of the comments on a thread."
  (request-response-data
   (request redditor--comment-url
            :complete redditor--fetch-comments-callback
            :sync nil
            :parser #'json-read
            :headers `(("User-Agent" . "fun")))))

(defun redditor--fetch-subreddit (subreddit)
  "Get a list of the SUBREDDIT on a thread."
  (request-response-data
   (request (format redditor--subreddit-url subreddit)
            :complete
            (cl-function
             (lambda (&rest data &allow-other-keys)
               (apply redditor--fetch-subreddit-callback subreddit data)))
            :sync nil
            :parser #'json-read
            :headers `(("User-Agent" . "fun")))))

(defun redditor--parse-comments-helper (comments)
  "Parse the comments that were fetched.

COMMENTS block is the nested list structure with them."
  (let-alist (alist-get 'data comments)
    (when (and .name .body .parent_id)
      (let ((composite (list (cons 'name (intern .name))
                             (cons 'body   .body)
                             (cons 'author .author)
                             (cons 'score  .score)
                             (cons 'parent_id (intern .parent_id)))))
        (push composite redditor--comments-composite)))
    (when .children (redditor--parse-comments .children))
    (when (and .replies
               (listp .replies))
      (redditor--parse-comments-helper .replies))))

(defun redditor--parse-subreddit-helper (subreddit-post subreddit)
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
        (push composite (gethash subreddit redditor--subreddit-composite))))
    (when .children (redditor--parse-subreddit .children subreddit))
    (when (and .replies
               (listp .replies))
      (redditor--parse-subreddit-helper .replies subreddit))))

(defun redditor--parse-comments (comments-vector)
  "Parse the cached comments and move to a hierarchy.

COMMENTS-VECTOR is a vector of comments."
  (mapcar #'redditor--parse-comments-helper comments-vector))

(defun redditor--parse-subreddit (subreddit-vector subreddit)
  "Parse the cached subreddit and move to a hierarchy.

SUBREDDIT-VECTOR is a vector of subreddit.
SUBREDDIT is the name of the subreddit."
  (mapcar (lambda (sub)
            (redditor--parse-subreddit-helper sub subreddit))
          subreddit-vector))

(defun redditor--parse-comments-from-cache ()
  "Parse comment structures from cache data."
  (setq redditor--comments-composite nil)
  (redditor--parse-comments redditor--cache-comments)
  redditor--comments-composite)

(defun redditor--parse-subreddit-from-cache (subreddit)
  "Parse comment structures from cache data.

SUBREDDIT should be a valid subreddit."
  (setf (gethash subreddit redditor--subreddit-composite) nil)
  (redditor--parse-subreddit (list (gethash subreddit redditor--cache-subreddit)) subreddit)
  (gethash subreddit redditor--subreddit-composite))

(defun redditor--find-comment-by-name (name)
  "Given NAME, find the corresponding comment."
  (cl-find-if
   (lambda (comment)
     (equal name (alist-get 'name comment)))
   redditor--comments-composite))

(defun redditor--find-subreddit-post-by-name (name)
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
     redditor--subreddit-composite)
    found))

(defvar redditor--parentfn
  (lambda (name)
    (unless (equal 'thread name)
      (let ((parent-id
             (alist-get
              'parent_id
              (cl-find-if
               (lambda (comment)
                 (equal name (alist-get 'name comment)))
               redditor--comments-composite))))
        (if parent-id parent-id 'thread)))))

(defgroup redditor nil
  "Redditor Mode customization group."
  :group 'applications)

(defcustom redditor-subreddits-active
  '(emacs lisp+Common_Lisp prolog)
  "List of subreddits you would like to subscribe to."
  :group 'redditor
  :type (list 'symbol))

(defvar redditor--hierarchy (hierarchy-new))

(defvar redditor--subreddit-hierarchy (hierarchy-new))

(defun redditor--comments-unique-ids (comments)
  "Get the unique IDs from both parent and name slots.

COMMENTS should be the ‘redditor--comments-composite’.

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

(defun redditor--hierarchy-build ()
  "Generate the comment structure."
  (setq redditor--hierarchy (hierarchy-new))
  (hierarchy-add-tree redditor--hierarchy 'thread redditor--parentfn)
  (let ((comments (redditor--parse-comments-from-cache)))
    (cl-loop
     for comment in (redditor--comments-unique-ids comments)
     do (progn
          (hierarchy-add-tree
           redditor--hierarchy
           comment
           redditor--parentfn)))))

(defun redditor--subreddit-hierarchy-build ()
  "Generate the subreddit-post structure."
  (setq redditor--subreddit-hierarchy (hierarchy-new))
  (hierarchy-add-tree redditor--subreddit-hierarchy 'subs (lambda (_) nil))
  (mapcar
   (lambda (subreddit)
     (hierarchy-add-tree redditor--subreddit-hierarchy subreddit (lambda (_) 'subs))
     (let ((subreddit-posts (redditor--parse-subreddit-from-cache subreddit)))
       (cl-loop
        for subreddit-post in subreddit-posts
        do (progn
             (hierarchy-add-tree
              redditor--subreddit-hierarchy
              (alist-get 'name subreddit-post)
              (lambda (_) subreddit))))))
   redditor-subreddits-active))

(defvar redditor--hierarchy-labelfn-hooks nil)
(defvar redditor--subreddit-hierarchy-labelfn-hooks nil)

(defun redditor--hierarchy-labelfn-button (labelfn actionfn)
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
       for fn in redditor--hierarchy-labelfn-hooks
       do (funcall fn item indent)))))

(defun redditor--comments-show ()
  "Show the comments that were built in the structure."
  (interactive)
  (setq redditor--hierarchy-labelfn-hooks
        '((lambda (item indent)
            (let ((comment (redditor--find-comment-by-name item)))
              (when comment
                (let-alist comment
                  (insert
                   (format " (%s) → %s\n"
                           .score .body))))))))
  (redditor--hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    redditor--hierarchy
    (hierarchy-labelfn-indent
     (redditor--hierarchy-labelfn-button
      (lambda (item _)
        (let ((comment (redditor--find-comment-by-name item)))
          (if comment
              (insert
               (format "%s"
                       (alist-get 'author comment)))
            (insert (symbol-name item)))))
      (lambda (item _) (message "You clicked on: %s" item)))))))

(defun redditor--subreddit-show ()
  "Show the subreddit-posts that were built in the structure."
  (interactive)
  (setq redditor--hierarchy-labelfn-hooks
        '((lambda (item indent)
            (let ((subreddit-post (redditor--find-subreddit-post-by-name item)))
              (when subreddit-post
                (let-alist subreddit-post
                  (insert
                   (format " (↑ %s / ☠ %s) by: %s"
                           .score .num_comments .author))))))))
  (redditor--subreddit-hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    redditor--subreddit-hierarchy
    (hierarchy-labelfn-indent
     (redditor--hierarchy-labelfn-button
      (lambda (item _)
        (let ((subreddit-post (redditor--find-subreddit-post-by-name item)))
          (if subreddit-post
              (insert
               (format "%s"
                       (alist-get 'title subreddit-post)))
            (insert (symbol-name item)))))
      (lambda (item _)
        (let* ((subreddit-post (redditor--find-subreddit-post-by-name item))
               (permalink (alist-get 'permalink subreddit-post)))
          (setq redditor--comment-url (format "http://reddit.com/%s.json" permalink)))
        (redditor--fetch-comments)
        (message "Fetching: %s" redditor--comment-url)))))))

;;;###autoload
(defun redditor ()
  "Invoke the main mode."
  (interactive)
  ;; (redditor--fetch-comments)
  (mapcar #'redditor--fetch-subreddit redditor-subreddits-active))

;;;###autoload
(defun redditor-mode ()
  "Invoke the main mode."
  (interactive)
  (redditor))

(provide 'redditor)
;;; redditor.el ends here
