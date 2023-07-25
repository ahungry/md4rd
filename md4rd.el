;;; md4rd.el --- Mode for reddit (browse it). -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/md4rd
;; Version: 0.3.1
;; Keywords: ahungry reddit browse news
;; Package-Requires: ((emacs "25.1") (request "0.3.0") (cl-lib "0.6.1") (dash "2.12.0") (s "1.12.0") (tree-mode "1.0.0"))

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

;; A major mode / command set for browsing Reddit.
;; After installation, just run: M-x md4rd to get started!

;;; News:

;;;; Changes in 0.3.1:
;; Fix for md4rd-open call.

;;; Code:

(require 'hierarchy)
(require 'cl-lib)
(require 'cl-extra)
(require 'dash)
(require 'request)
(require 'json)
(require 's)
(require 'tree-mode)

(defvar md4rd--version "0.3.1"
  "The current version of the mode.")

;;   ___   _       _   _
;;  / _ \ /_\ _  _| |_| |_
;; | (_) / _ \ || |  _| ' \
;;  \___/_/ \_\_,_|\__|_||_|

;; OAuth related code (figlets rock!):

;; https://github.com/reddit/reddit/wiki/OAuth2
(defvar md4rd--oauth-client-id ""
  "The client ID that links this up to the reddit.com OAuth endpoint, as generated from Reddit when you create/apply for an API app.")

(defvar md4rd--oauth-redirect-uri
  ""
  "The Oauth2 redirect_uri that links this up to the reddit.com OAuth endpoint.")

(defvar md4rd--oauth-url
  "https://www.reddit.com/api/v1/authorize?client_id=%s&response_type=code&state=nil&redirect_uri=%s&duration=permanent&scope=vote,submit"
  "The OAuth URL/endpoint.")

(defvar md4rd--oauth-access-token-uri
  "https://www.reddit.com/api/v1/access_token"
  "The OAuth access token URI (step 2, after user fills out code).")

(defvar md4rd--oauth-code ""
  "The code, as given from the reddit redirect URI after user accepts permissions.")

(defvar md4rd--oauth-access-token ""
  "The access token, as given from the reddit OAuth endpoint after user inputs code.")

(defvar md4rd--oauth-refresh-token ""
  "The refresh token, as given from the reddit OAuth endpoint after user inputs code.")

(defvar md4rd--action-button-ctx 'visit
  "The next action to attempt on a button press.

Should be one of visit, upvote, downvote, open.")

(defun md4rd-logged-in-p ()
  "See if the user is signed in or not."
  (> (length md4rd--oauth-access-token) 0))

(defun md4rd--oauth-build-url ()
  "Generate the URL based on our parameters."
  (format md4rd--oauth-url
          md4rd--oauth-client-id
          md4rd--oauth-redirect-uri))

;; User should end up here:
;; http://ahungry.com/md4rd?state=nil&code=secret_code_shown
(defun md4rd--oauth-browser-fetch ()
  "Open the user's browser to the endpoint to get the OAuth token."
  (message (format "For OAuth (md4rd) opening browser to: %s" (md4rd--oauth-build-url)))
  (browse-url
   (md4rd--oauth-build-url)))

(defun md4rd-oauth-set-code (code)
  "Set the authorization CODE for OAuth (necessary to request the bearer token)."
  (interactive "sPlease enter the code you received from the browser: ")
  (setq md4rd--oauth-code (s-trim code)))

(cl-defun md4rd--oauth-fetch-callback (&rest data &allow-other-keys)
  "Callback to run when the oauth code fetch is complete."
  (let-alist (plist-get data :data)
    (unless (and .access_token .refresh_token .expires_in)
      (message "md4rd: Failed to fetch OAuth access_token and refresh_token values!")
      (error "md4rd: Failed to fetch OAuth access_token and refresh_token values!"))
    (setq md4rd--oauth-access-token .access_token)
    (setq md4rd--oauth-refresh-token .refresh_token)
    ;; @todo Handle expires_in value (should be ~1 hour, so refresh before then)
    (message "Tokens set - consider adding md4rd--oauth-access-token and md4rd--oauth-refresh-token values to your init file to avoid signing in again in the future sessions.")))

(defun md4rd--oauth-fetch-authorization-token ()
  "Make the initial code request for OAuth."
  (request-response-data
   (request md4rd--oauth-access-token-uri
            :complete #'md4rd--oauth-fetch-callback
            :data (format "grant_type=authorization_code&code=%s&redirect_uri=%s"
                          md4rd--oauth-code
                          md4rd--oauth-redirect-uri)
            :sync nil
            :type "POST"
            :parser #'json-read
            :headers `(("User-Agent" . "md4rd")
                       ;; This is just the 'client_id:' base64'ed
                       ("Authorization" . ,(format "Basic %s" (base64-encode-string (format "%s:" md4rd--oauth-client-id))))))))

(cl-defun md4rd--oauth-fetch-callback-refresh-token (&rest data &allow-other-keys)
  "Callback to run when the oauth refresh fetch is complete."
  (let-alist (plist-get data :data)
    (unless (and .access_token .expires_in)
      (message "md4rd: Failed to refresh the OAuth access token!")
      (error "md4rd: Failed to refresh the OAuth access token!"))
    (setq md4rd--oauth-access-token .access_token)
    ;; @todo Handle expires_in value (should be ~1 hour, so refresh before then)
    (message "md4rd: Access token refreshed.")))

(defun md4rd--oauth-fetch-refresh-token ()
  "Make a request for a new OAuth access token using the permanent refresh token."
  (request-response-data
   (request md4rd--oauth-access-token-uri
            :complete #'md4rd--oauth-fetch-callback-refresh-token
            :data (format "grant_type=refresh_token&refresh_token=%s"
                          md4rd--oauth-refresh-token)
            :sync nil
            :type "POST"
            :parser #'json-read
            :headers `(("User-Agent" . "md4rd")
                       ;; This is just the 'client_id:' base64'ed
                       ("Authorization" . ,(format "Basic %s" (base64-encode-string (format "%s:" md4rd--oauth-client-id))))))))

;;;###autoload
(defun md4rd-login ()
  "Sign into the reddit system via OAuth, to allow use of authenticated endpoints."
  (interactive)
  (md4rd--oauth-browser-fetch)
  (call-interactively #'md4rd-oauth-set-code)
  (md4rd--oauth-fetch-authorization-token))

;;;###autoload
(defun md4rd-refresh-login ()
  "Refresh the OAuth authentication token (lifetime 3600 seconds)."
  (interactive)
  (md4rd--oauth-fetch-refresh-token))

;; For comment votes, the usable id is just the 'name' property.
(defun md4rd--post-vote (id dir)
  "Cast a vote on a thing.  ID is the t3_xxx type id, DIR is up or down."
  (message (format  "Voting on %s with a value of: %s" id dir))
  (request-response-data
   (request "https://oauth.reddit.com/api/vote"
            :complete nil
            :data (format "id=%s&dir=%s" id dir)
            :sync nil
            :type "POST"
            :parser #'json-read
            :headers `(("User-Agent" . "md4rd")
                       ("Authorization" . ,(format  "bearer %s" md4rd--oauth-access-token))))))

(defun md4rd--post-reply (id message)
  "Cast a vote on a thing.  ID is the t3_xxx type id, MESSAGE is the message."
  (message (format  "Replying to %s with a value of: %s" id message))
  (request-response-data
   (request "https://oauth.reddit.com/api/comment"
            :complete nil
            :data (format "thing_id=%s&text=%s" id message)
            :sync nil
            :type "POST"
            :parser #'json-read
            :headers `(("User-Agent" . "md4rd")
                       ("Authorization" . ,(format  "bearer %s" md4rd--oauth-access-token))))))

(defvar md4rd--cache-comments nil
  "Store the most recent comment cache/fetch.")

(defvar md4rd--cache-sub
  (make-hash-table :test #'equal)
  "Store the most recent comment cache/fetch.")

(defvar md4rd--comments-composite nil)

(defvar md4rd--sub-composite
  (make-hash-table :test #'equal))

(cl-defun md4rd--fetch-comments-callback (&rest data &allow-other-keys)
  "Callback for async, DATA is the response from request."
  (let ((data (plist-get data :data)))
    (setq md4rd--cache-comments data)
    (md4rd--comments-show)))

(cl-defun md4rd--fetch-sub-callback (sub &rest data &allow-other-keys)
  "Callback for async, DATA is the response from request."
  (let ((my-data (plist-get data :data)))
    (setf (gethash sub md4rd--cache-sub) my-data)
    (md4rd--sub-show)))

(defvar md4rd--sub-url
  "https://www.reddit.com/r/%s.json")

(defun md4rd--fetch-comments (comment-url)
  "Get a list of the comments on a thread that belong to COMMENT-URL."
  (request-response-data
   (request comment-url
            :complete #'md4rd--fetch-comments-callback
            :sync nil
            :parser #'json-read
            :headers `(("User-Agent" . "fun")))))

(defun md4rd--fetch-sub (sub)
  "Get a list of the SUB on a thread."
  (request-response-data
   (request (format md4rd--sub-url sub)
            :complete
            (cl-function
             (lambda (&rest data &allow-other-keys)
               (apply #'md4rd--fetch-sub-callback sub data)))
            :sync nil
            :parser #'json-read
            :headers `(("User-Agent" . "fun")))))

(defun md4rd--parse-comments-helper (comments)
  "Parse the comments that were fetched.

COMMENTS block is the nested list structure with them."
  (when (listp comments)
    (let-alist (alist-get 'data comments)
      (when (and .name (or .body .selftext))
        (let ((composite (list (cons 'name (intern .name))
                               (cons 'body   (or .body .selftext))
                               (cons 'author .author)
                               (cons 'score  .score)
                               (cons 'parent_id (if .parent_id (intern .parent_id) 'thread)))))
          (push composite md4rd--comments-composite)))
      (when .children (md4rd--parse-comments .children))
      (when (and .replies
                 (listp .replies))
        (md4rd--parse-comments-helper .replies)))))

(defun md4rd--parse-sub-helper (sub-post sub)
  "Parse the sub that were fetched.

SUB-POST is the actual post data submitted.
SUB block is the nested list structure with them."
  (let-alist (alist-get 'data sub-post)
    (when (and .name .permalink)
      (let ((composite (list (cons 'name (intern .name))
                             (cons 'permalink    .permalink)
                             (cons 'url          .url)
                             (cons 'num_comments .num_comments)
                             (cons 'author       .author)
                             (cons 'title        .title)
                             (cons 'selftext     .selftext)
                             (cons 'score        .score))))
        (push composite (gethash sub md4rd--sub-composite))))
    (when .children (md4rd--parse-sub .children sub))
    (when (and .replies
               (listp .replies))
      (md4rd--parse-sub-helper .replies sub))))

(defun md4rd--parse-comments (comments-vector)
  "Parse the cached comments and move to a hierarchy.

COMMENTS-VECTOR is a vector of comments."
  (mapcar #'md4rd--parse-comments-helper comments-vector))

(defun md4rd--parse-sub (sub-vector sub)
  "Parse the cached sub and move to a hierarchy.

SUB-VECTOR is a vector of sub.
SUB is the name of the sub."
  (mapcar (lambda (sub-post)
            (md4rd--parse-sub-helper sub-post sub))
          sub-vector))

(defun md4rd--parse-comments-from-cache ()
  "Parse comment structures from cache data."
  (setq md4rd--comments-composite nil)
  (md4rd--parse-comments md4rd--cache-comments)
  md4rd--comments-composite)

(defun md4rd--parse-sub-from-cache (sub)
  "Parse comment structures from cache data.

SUB should be a valid sub."
  (setf (gethash sub md4rd--sub-composite) nil)
  (md4rd--parse-sub (list (gethash sub md4rd--cache-sub)) sub)
  (gethash sub md4rd--sub-composite))

(defun md4rd--find-comment-by-name (name)
  "Given NAME, find the corresponding comment."
  (cl-find-if
   (lambda (comment)
     (equal name (alist-get 'name comment)))
   md4rd--comments-composite))

(defun md4rd--find-sub-post-by-name (name)
  "Given NAME, find the corresponding sub-post."
  (let ((found nil))
    (maphash
     (lambda (_ hash-value)
       (let ((post-find
              (cl-find-if
               (lambda (sub-post)
                 (equal name (alist-get 'name sub-post)))
               hash-value)))
         (when post-find (setq found post-find))))
     md4rd--sub-composite)
    found))

(defvar md4rd--parentfn
  (lambda (name)
    (unless (equal 'thread name)
      (let ((parent-id
             (alist-get
              'parent_id
              (cl-find-if
               (lambda (comment)
                 (equal name (alist-get 'name comment)))
               md4rd--comments-composite))))
        (if parent-id parent-id 'thread)))))

(defgroup md4rd nil
  "Md4rd Mode customization group."
  :group 'applications)

(defcustom md4rd-subs-active
  '(emacs lisp+Common_Lisp prolog)
  "List of subs you would like to subscribe to."
  :group 'md4rd
  :type (list 'symbol))


;;  ___  _         _
;; |   \(_)____ __| |__ _ _  _
;; | |) | (_-< '_ \ / _` | || |
;; |___/|_/__/ .__/_\__,_|\_, |
;;           |_|          |__/

;; Display related code (thanks hierarchy):

;; Derived from twittering-mode.el
(defun md4rd-pop-to-buffer-in-current-window (buf &optional win)
  "Select the buffer BUF in the window WIN by splitting it.
If WIN is nil, the selected window is splitted."
  (let* ((win (or win (selected-window)))
	 (size
	  (let ((rest (- (window-height win) 15)))
	    (if (<= rest 3)
		;; To avoid an error due to a too small window.
		nil
	      rest)))
	 (new-win (split-window win size)))
    (select-window new-win)
    (switch-to-buffer buf)))

;; Lifted from window.el kill-buffer-and-window
(defun md4rd--reply-kill ()
  "Kill the reply buffer entirely."
  (interactive)
  (let ((window-to-delete (selected-window))
        (buffer-to-kill (current-buffer))
        (delete-window-hook (lambda () (ignore-errors (delete-window)))))
    (unwind-protect
        (progn
          (add-hook 'kill-buffer-hook delete-window-hook t t)
          (if (kill-buffer (current-buffer))
              ;; If `delete-window' failed before, we rerun it to regenerate
              ;; the error so it can be seen in the echo area.
              (when (eq (selected-window) window-to-delete)
                (delete-window))))
      ;; If the buffer is not dead for some reason (probably because
      ;; of a `quit' signal), remove the hook again.
      (ignore-errors
        (with-current-buffer buffer-to-kill
          (remove-hook 'kill-buffer-hook delete-window-hook t))))))

(defvar md4rd--reply-to-id nil)

(defun md4rd--reply-send ()
  "Send the reply buffer message."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 4)
    (let ((content (buffer-substring (point) (point-max))))
      (md4rd--post-reply md4rd--reply-to-id content)
      (md4rd--reply-kill))))

(defun md4rd--reply-pop (name)
  "Pop up a reply buffer to send a response to NAME comment."
  (interactive)
  (setq md4rd--reply-to-id name)
  (let ((reply-buffer (generate-new-buffer "*md4rd-reply*")))
    (md4rd-pop-to-buffer-in-current-window reply-buffer)
    (md4rd-reply-mode)
    (insert (format "Replying to comment id: %s\n" name))
    (insert ";; C-c C-c to submit, C-c C-k to cancel.\n")
    (insert ";; Text above this line will be ignored.\n")
    (insert ";; -------------------------------------\n")
    (add-text-properties (point-min) (point-max)
                         `(font-lock-face font-lock-comment-face rear-nonsticky t))
    (font-lock-flush)))

(defvar md4rd--hierarchy (hierarchy-new))

(defvar md4rd--sub-hierarchy (hierarchy-new))

(defun md4rd--comments-unique-ids (comments)
  "Get the unique IDs from both parent and name slots.

COMMENTS should be the ‘md4rd--comments-composite’.

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

(defun md4rd--hierarchy-build ()
  "Generate the comment structure."
  (setq md4rd--hierarchy (hierarchy-new))
  (hierarchy-add-tree md4rd--hierarchy 'thread md4rd--parentfn)
  (let ((comments (md4rd--parse-comments-from-cache)))
    (cl-loop
     for comment in (md4rd--comments-unique-ids comments)
     do (progn
          (hierarchy-add-tree
           md4rd--hierarchy
           comment
           md4rd--parentfn)))))

(defun md4rd--sub-hierarchy-build ()
  "Generate the sub-post structure."
  (setq md4rd--sub-hierarchy (hierarchy-new))
  (hierarchy-add-tree md4rd--sub-hierarchy 'subs (lambda (_) nil))
  (mapcar
   (lambda (sub)
     (hierarchy-add-tree md4rd--sub-hierarchy sub (lambda (_) 'subs))
     (let ((sub-posts (md4rd--parse-sub-from-cache sub)))
       (cl-loop
        for sub-post in sub-posts
        do (progn
             (hierarchy-add-tree
              md4rd--sub-hierarchy
              (alist-get 'name sub-post)
              (lambda (_) sub))))))
   md4rd-subs-active))

(defvar md4rd--hierarchy-labelfn-hooks nil)
(defvar md4rd--sub-hierarchy-labelfn-hooks nil)

(defun md4rd--hierarchy-labelfn-fixed-indent (labelfn &optional indent-string)
  "Return a function rendering LABELFN indented with INDENT-STRING.

INDENT-STRING defaults to a 2-space string.  Indentation is
multiplied by the depth of the displayed item."
  (let ((indent-string (or indent-string "  ")))
    (lambda (item indent)
      (insert indent-string)
      (funcall labelfn item indent))))

(defface md4rd--greentext-face
  '((((type graphic) (background dark))
     :background nil :foreground "#90a959")
    (((type graphic) (background light))
     :background nil :foreground "#90a959")
    (t :background nil :foreground "#90a959"))
  "Face for rendering greentexts."
  :group 'md4rd)

(defun md4rd--greentext (text)
  "Take in the multiple lines of a post and color the quotes (lines
starting with >)."
  (let* ((entities-text (replace-regexp-in-string
                         "\\\\?&gt;" ">" text))
         (lines (split-string entities-text "[\n]+"))
         (green-lines (cl-map 'list 'md4rd--greentext-line lines)))
    (mapconcat (lambda (l) (format "%s\n" l))
               green-lines " ")))

(defun md4rd--greentext-line (line)
  "Take in a single line and color it if it is a quote."
  (let ((isquote (cl-search ">" line :start2 0)))
    (if isquote
        (add-text-properties
         0 (length line)
         `(font-lock-face md4rd--greentext-face) line)))
  line)

(defun md4rd--hierarchy-labelfn-button (labelfn actionfn)
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
       for fn in md4rd--hierarchy-labelfn-hooks
       do (funcall fn item indent)))))

(defvar md4rd--fool-text "[deleted]"
  "Comments by fools will be replaced by this.")

(defvar md4rd--fool-list '()
  "Do not show comments from this list of fools.")

(defun md4rd--comments-show ()
  "Show the comments that were built in the structure."
  (interactive)
  (setq md4rd--hierarchy-labelfn-hooks
        '((lambda (item indent)
            (let ((comment (md4rd--find-comment-by-name item)))
              (when comment
                (let-alist comment
                  (insert
                   (format " (%s) → %s\n"
                           .score
                           (if (member .author md4rd--fool-list)
                               md4rd--fool-text
                             (md4rd--greentext .body))))))))))
  (md4rd--hierarchy-build)
  (switch-to-buffer
   (hierarchy-tree-display
    md4rd--hierarchy
    (md4rd--hierarchy-labelfn-fixed-indent
     (md4rd--hierarchy-labelfn-button
      (lambda (item _)
        (let ((comment (md4rd--find-comment-by-name item)))
          (if comment
              (insert
               (format "%s"
                       (alist-get 'author comment)))
            (insert (symbol-name item)))))
      ;; Controls the button events we dispatch.
      (lambda (item _)
        (let ((comment (md4rd--find-comment-by-name item)))
          (let-alist comment
            (cond
             ((equal 'reply md4rd--action-button-ctx)
              (md4rd--reply-pop .name))

             ((equal 'upvote md4rd--action-button-ctx)
              (md4rd--post-vote .name 1))

             ((equal 'downvote md4rd--action-button-ctx)
              (md4rd--post-vote .name -1))

             ((equal 'open md4rd--action-button-ctx)
              (browse-url .url))

             ((equal 'visit md4rd--action-button-ctx)
              (when .permalink
                (message "Fetching: %s" .permalink)
                (md4rd--fetch-comments
                 (format "http://reddit.com%s.json" .permalink))))

             (t (error "Unknown link action!"))))))))))
  ;; (md4rd-widget-collapse-all 2)
  (md4rd-widget-expand-all)
  (md4rd-mode)
  ;; TODO: Maybe I'll make this auto-run, but it is problematic atm,
  ;; when the trees are toggled, the nice indent/fill is lost.
  ;; (md4rd-indent-all-the-lines)
  )

(defun md4rd-indent-all-the-lines ()
  "Indent them!"
  (interactive)
  (setq inhibit-read-only t)
  ;; (fill-region (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (setq inhibit-read-only nil))

(defun md4rd--sub-show ()
  "Show the sub-posts that were built in the structure."
  (interactive)
  ;; Unique buffer for subreddit displays.
  (let ((buffer (or (get-buffer "*subreddits*")
                    (get-buffer-create "*subreddits*"))))
    (setq md4rd--hierarchy-labelfn-hooks
          '((lambda (item indent)
              (let ((sub-post (md4rd--find-sub-post-by-name item)))
                (when sub-post
                  (let-alist sub-post
                    (if (not (member .author md4rd--fool-list))
                        (insert
                         (format " (↑ %s / ☠ %s) by: %s"
                                 .score .num_comments .author)))))))))
    (md4rd--sub-hierarchy-build)
    (switch-to-buffer
     (hierarchy-tree-display
      md4rd--sub-hierarchy
      (md4rd--hierarchy-labelfn-fixed-indent
       (md4rd--hierarchy-labelfn-button
        ;; Controls the label we show for the article post.
        (lambda (item _)
          (let ((sub-post (md4rd--find-sub-post-by-name item)))
            (if sub-post
                (insert
                 (format "%s"
                         (if (member (alist-get 'author sub-post) md4rd--fool-list)
                             md4rd--fool-text
                           (alist-get 'title sub-post))))
              (insert (symbol-name item)))))
        ;; Controls the button events we dispatch.
        (lambda (item _)
          (let ((sub-post (md4rd--find-sub-post-by-name item)))
            (let-alist sub-post
              (cond
               ((equal 'upvote md4rd--action-button-ctx)
                (md4rd--post-vote .name +1))

               ((equal 'downvote md4rd--action-button-ctx)
                (md4rd--post-vote .name -1))

               ((equal 'open md4rd--action-button-ctx)
                (browse-url .url))

               ((equal 'visit md4rd--action-button-ctx)
                (when .permalink
                  (message "Fetching: %s" .permalink)
                  (md4rd--fetch-comments
                   (format "http://reddit.com%s.json" .permalink))))

               (t (error "Unknown link action!"))))))))
      buffer))
    (md4rd-mode)
    (md4rd-widget-collapse-all)))

;;;###autoload
(defun md4rd ()
  "Invoke the main mode."
  (interactive)
  (mapcar #'md4rd--fetch-sub md4rd-subs-active))

;;;###autoload
(defun mode-for-reddit ()
  "Invoke the main mode."
  (interactive)
  (md4rd))

;;    _      _   _
;;   /_\  __| |_(_)___ _ _  ___
;;  / _ \/ _|  _| / _ \ ' \(_-<
;; /_/ \_\__|\__|_\___/_||_/__/

;;  Actions related code:

(defun md4rd-reply ()
  "Reply something the user is on."
  (interactive)
  (unless (md4rd-logged-in-p)
    (md4rd-login))
  ;; Ensure we're actually on a plain button, not a tree widget.
  (when (equal 'button (button-type (button-at (point))))
    (setq md4rd--action-button-ctx 'reply)
    (message "Reply!")
    (push-button)
    (setq md4rd--action-button-ctx 'visit)))

(defun md4rd-upvote ()
  "Upvote something the user is on."
  (interactive)
  (unless (md4rd-logged-in-p)
    (md4rd-login))
  ;; Ensure we're actually on a plain button, not a tree widget.
  (when (equal 'button (button-type (button-at (point))))
    (setq md4rd--action-button-ctx 'upvote)
    (message "Upvoted!")
    (push-button)
    (setq md4rd--action-button-ctx 'visit)))

(defun md4rd-downvote ()
  "Downvote something the user is on."
  (interactive)
  (unless (md4rd-logged-in-p)
    (md4rd-login))
  (when (equal 'button (button-type (button-at (point))))
    (setq md4rd--action-button-ctx 'downvote)
    (message "Downvoting!")
    (push-button)
    (setq md4rd--action-button-ctx 'visit)))

(defun md4rd-open ()
  "Open the thread in browser, or toggle the subreddit."
  (interactive)
  (save-excursion
    ;; This doesn't seem to work at all on Emacs 27.0.50
    ;; Not sure why Wei added it, but going to take out for now.
    ;; (forward-button 1)
    (condition-case-unless-debug nil
        (let ((md4rd--action-button-ctx 'open))
          (push-button))
      (error
       (condition-case-unless-debug nil
           (progn
             (widget-backward 1)
             (tree-mode-toggle-expand))
         (error
          (message "Cannot open next button.")))))))

(defun md4rd-visit ()
  "Visit the thread in Emacs, or toggle the subreddit."
  (interactive)
  (save-excursion
    (forward-button 1)
    (condition-case-unless-debug nil
        (let ((md4rd--action-button-ctx 'visit))
          (push-button))
      (error
       (condition-case-unless-debug nil
           (progn
             (widget-backward 1)
             (tree-mode-toggle-expand))
         (error
          (message "Cannot visit next button.")))))))

;;  __  __         _
;; |  \/  |___  __| |___
;; | |\/| / _ \/ _` / -_)
;; |_|  |_\___/\__,_\___|

;; Mode related code:

(defun md4rd-widget-toggle-line ()
  "Hop to the widget and open it up."
  (interactive)
  ;; If we're at the article text, we get 'button
  ;; On the widget, we receive nil for 2nd check.
  (when (and (button-at (point))
             (equal 'button (button-type (button-at (point)))))
    (widget-backward 1))

  (unless (button-at (point))
    (widget-forward 1))
  (widget-button-press (point))
  (widget-forward 1))

(defun md4rd-widget-collapse-all (&optional level)
  "Collapse to LEVEL, or to the subreddit list by default."
  (interactive "p")
  (tree-mode-goto-root)
  (tree-mode-expand-level (or level 1)))

(defun md4rd-widget-expand-all (&optional level)
  "Expand to LEVEL, or expand all nodes by default."
  (interactive "p")
  (tree-mode-goto-root)
  (tree-mode-expand-level (or level 0)))

(defun md4rd-jump-to-subs ()
  "Jump back to subs hierarchy after visiting a thread"
  (interactive)
  (switch-to-buffer "*subreddits*"))

(defvar md4rd-reply-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'md4rd--reply-send)
    (define-key map (kbd "C-c C-k") 'md4rd--reply-kill)
    map)
  "Keymap for md4rd reply major mode.")

(defvar md4rd-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "u") 'tree-mode-goto-parent)
    (define-key map (kbd "o") 'md4rd-open)
    (define-key map (kbd "v") 'md4rd-visit)
    (define-key map (kbd "e") 'tree-mode-toggle-expand)
    (define-key map (kbd "E") 'md4rd-widget-expand-all)
    (define-key map (kbd "C") 'md4rd-widget-collapse-all)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "j") 'widget-forward)
    (define-key map (kbd "h") 'backward-button)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "k") 'widget-backward)
    (define-key map (kbd "l") 'forward-button)
    (define-key map (kbd "q") 'kill-current-buffer)
    (define-key map (kbd "r") 'md4rd-reply)
    (define-key map (kbd "u") 'md4rd-upvote)
    (define-key map (kbd "d") 'md4rd-downvote)
    (define-key map (kbd "t") 'md4rd-widget-toggle-line)
    (define-key map (kbd "M-q") 'md4rd-indent-all-the-lines)
    map)
  "Keymap for md4rd major mode.")

(defun md4rd-evil-binds ()
  "Bind commands for evil users as well (when its on)."
  (interactive)
  (when (fboundp 'evil-define-key*)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "r") 'md4rd-reply)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "u") 'md4rd-upvote)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "d") 'md4rd-downvote)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "o") 'md4rd-open)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "t") 'md4rd-widget-toggle-line)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "e") 'md4rd-widget-expand-all)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "c") 'md4rd-widget-collapse-all)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "M-q") 'md4rd-indent-all-the-lines)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "TAB") 'widget-forward)
    (evil-define-key* '(normal motion) md4rd-mode-map (kbd "<backtab>") 'widget-backward)))

(defun md4rd-reply-mode ()
  "Invoke the main reply mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map md4rd-reply-mode-map)
  (setq major-mode 'md4rd-reply-mode)
  (setq mode-name "md4rd-reply")
  (run-hooks 'md4rd-reply-mode-hook))

(defun md4rd-get-current-indent ()
  (save-excursion
    (forward-line -1)
    (while (looking-at "^[ ]*$")
      (forward-line -1))
    (let* ((l (thing-at-point 'line t))
           (lpos (string-match "[A-Za-z0-9]" l)))
      (if (and lpos (> lpos 0))
          (max lpos (current-indentation))
        (current-indentation)))))

(defun md4rd-fill-line ()
 (let (spos epos)
          (save-excursion
            (beginning-of-line)
            (setq spos (point))
            (end-of-line)
            (setq epos (point))
            (fill-region spos epos))))

(defun md4rd-indent-line ()
  "See what happens."
  (interactive)
  (beginning-of-line)
  (md4rd-fill-line)
  (if (or (looking-at "^.*[[]-")
              (looking-at "^.*`-]")
              (looking-at "^.*|-]")
              (looking-at "^.**→"))
      (md4rd-fill-line)
    (let ((cur-indent (md4rd-get-current-indent)))
      (unless (bobp)
        (fill-paragraph))
      (indent-line-to cur-indent))))

;;;###autoload
(defun md4rd-mode ()
  "Invoke the main mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map md4rd-mode-map)
  (md4rd-evil-binds)
  (set (make-local-variable 'indent-line-function) 'md4rd-indent-line)
  (setq major-mode 'md4rd-mode)
  (setq mode-name "md4rd")
  (run-hooks 'md4rd-mode-hook))

;; (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines)

(provide 'md4rd)
;;; md4rd.el ends here
