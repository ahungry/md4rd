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

(defvar rm:comments-composite nil)

(defvar rm:fetch-comments-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((data (cl-getf data :data)))
       (setq rm:cache-comments data)
       (message "Fetch done.")
       (rm:comments-show)
       ;; (print data)
       )))
  )

(defun rm:fetch-comments ()
  "Get a list of the comments on a thread."
  (request-response-data
   (request "https://www.reddit.com/r/emacs/comments/7rbzqx/rms_please_help_proofreading_the_emacs_manual.json"
            :complete rm:fetch-comments-callback
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

(defun rm:parse-comments (comments-vector)
  "Parse the cached comments and move to a hierarchy.

COMMENTS-VECTOR is a vector of comments."
  (mapcar #'rm:parse-comments-helper comments-vector)
  )

(defun rm:parse-comments-from-cache ()
  "Parse comment structures from cache data."
  (setq rm:comments-composite nil)
  (rm:parse-comments rm:cache-comments)
  rm:comments-composite
  )

(defun rm:find-comment-by-name (name)
  "Given NAME, find the corresponding comment."
  (cl-find-if
   (lambda (comment)
     (equal name (cdr (assoc 'name comment))))
   rm:comments-composite))

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

(defvar rm:hierarchy (hierarchy-new))

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
          (print comment)
          (hierarchy-add-tree
           rm:hierarchy
           comment
           rm:parentfn))))
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

(defun rm:comments-show ()
  "Show the comments that were built in the structure."
  (interactive)
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

;;;###autoload
(defun redditor-mode ()
  "Invoke the main mode."
  (interactive)
  (rm:fetch-comments))

(provide 'redditor-mode)
;;; redditor-mode.el ends here
