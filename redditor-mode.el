;;; redditor-mode.el --- TBD. -*- lexical-binding: t; -*-

;;; Commentary:

;; See a ref for hierarchy here:
;; https://github.com/DamienCassou/hierarchy

;;; Code:

(require 'hierarchy)

(defvar rm:hierarchy (hierarchy-new))

(defun rm:hierarchy-build ()
  "Generate the comment structure."
  (let ((parentfn
         (lambda (item)
           (cl-case item
             (dove 'bird)
             (pigeon 'bird)
             (bird 'animal)
             (dolphin 'animal)
             (cow 'animal)))))
    (hierarchy-add-tree rm:hierarchy 'dove parentfn)
    (hierarchy-add-tree rm:hierarchy 'pigeon parentfn)
    (hierarchy-add-tree rm:hierarchy 'dolphin parentfn)
    (hierarchy-add-tree rm:hierarchy 'cow parentfn)
    ))

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
