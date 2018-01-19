;;; redditor-mode.el --- TBD. -*- lexical-binding: t; -*-

;;; Commentary:

;; See a ref for hierarchy here:
;; https://github.com/DamienCassou/hierarchy

;;; Code:

(require 'hierarchy)

(defvar rm:reddit-cache-comments nil
  "Store the most recent comment cache/fetch.")

(defvar rm:reddit-comments-composite nil)

(defvar rm:reddit-fetch-comments-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from request."
     (let ((data (cl-getf data :data)))
       (setq rm:reddit-cache-comments data)
       (message "Fetch done.")
       ;; (print data)
       )))
  )

(defun rm:reddit-fetch-comments ()
  "Get a list of the comments on a thread."
  (request-response-data
   (request "https://www.reddit.com/r/emacs/comments/7rbzqx/rms_please_help_proofreading_the_emacs_manual.json"
            :complete rm:reddit-fetch-comments-callback
            :sync nil
            :parser 'json-read
            :headers `(("User-Agent" . "fun")))))

(defun rm:reddit-parse-comments-helper (comments)
  "Parse the comments that were fetched.

COMMENTS block is the nested list structure with them."
  (let* ((data (assoc 'data comments))
         (name (assoc 'name data))
         (parent_id (assoc 'parent_id data))
         (body (assoc 'body data))
         (replies (assoc 'replies data))
         (children (assoc 'children data)))
    (when (and name body parent_id)
      (let ((composite
             `(
               (name . ,(make-symbol (cdr name)))
               ,body
               (parent_id . ,(make-symbol (cdr parent_id))))))
        (push composite rm:reddit-comments-composite)))
    (when children (rm:reddit-parse-comments (cdr children)))
    (when (and replies
               (cdr replies)
               (listp (cdr replies)))
      (rm:reddit-parse-comments-helper (cdr replies))
      )
    )
  )

(defun rm:reddit-parse-comments (comments-vector)
  "Parse the cached comments and move to a hierarchy.

COMMENTS-VECTOR is a vector of comments."
  (mapcar #'rm:reddit-parse-comments-helper comments-vector)
  )

(defun rm:reddit-parse-comments-from-cache ()
  "Parse comment structures from cache data."
  (setq rm:reddit-comments-composite nil)
  (rm:reddit-parse-comments rm:reddit-cache-comments)
  rm:reddit-comments-composite
  )

(defvar rm:parentfn
  (lambda (name)
    (let ((parent-id
           (cdr (assoc
                 'parent_id
                 (cl-find-if
                  (lambda (comment)
                    (string= name (cdr (assoc 'name comment))))
                  rm:reddit-comments-composite)))))
      (when parent-id (make-symbol parent-id)))
    ))

(defvar rm:hierarchy (hierarchy-new))

(defun rm:hierarchy-build ()
  "Generate the comment structure."
  (setq rm:hierarchy (hierarchy-new))
  (let ((comments (rm:reddit-parse-comments-from-cache)))
    (cl-loop
     for comment in comments
     do (hierarchy-add-tree
         rm:hierarchy
         (make-symbol (cdr (assoc 'name comment)))
         rm:parentfn)))
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
         (insert (format "\n%s\n" (symbol-name item))))))

(defun rm::comments-show ()
  "Show the comments that were built in the structure."
  (interactive)
  (rm:hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    rm:hierarchy
    (hierarchy-labelfn-indent
     (rm:hierarchy-labelfn-button
      (lambda (item _) (insert (symbol-name item)))
      (lambda (item _) (message "You clicked on: %s" item)))))))

(provide 'redditor-mode)
;;; redditor-mode.el ends here
