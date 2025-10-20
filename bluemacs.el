;;; bluemacs.el --- Bluesky client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Anna Pawlicka <hi@annapawlicka.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: comm, bluesky, social
;; URL: https://github.com/annapawlicka/bluemacs.el

;;; Commentary:

;; This package provides a simple Bluesky client for Emacs.
;; It allows you to log in to Bluesky and view posts from your timeline.
;; More to come.
;;
;; Usage:
;;   M-x bluemacs-login
;;   M-x bluemacs-timeline

;;; Code:

(require 'json)
(require 'url)
(require 'auth-source)

;;; Customization

(defgroup bluemacs nil
  "Bluesky client for Emacs."
  :group 'applications
  :prefix "bluemacs-")

(defcustom bluemacs-instance "https://bsky.social"
  "Bluesky instance URL."
  :type 'string
  :group 'bluemacs)

(defcustom bluemacs-timeline-limit 10
  "Number of posts to fetch from timeline."
  :type 'integer
  :group 'bluemacs)

;;; Variables

(defvar bluemacs-access-token nil
  "Access token for Bluesky API authentication.")

(defvar bluemacs-did nil
  "User's DID (Decentralized Identifier).")

(defvar bluemacs-handle nil
  "User's Bluesky handle.")

(defvar bluemacs-timeline-buffer "*Bluesky Timeline*"
  "Buffer name for displaying timeline.")

;;; Authentication

(defun bluemacs--get-credentials ()
  "Retrieve Bluesky credentials from auth-source or prompt user."
  (let* ((auth-info (car (auth-source-search :host "bsky.social"
                                               :require '(:user :secret)
                                               :create t)))
         (handle (if auth-info
                     (plist-get auth-info :user)
                   (read-string "Bluesky handle (e.g., user.bsky.social): ")))
         (password (if auth-info
                       (let ((secret (plist-get auth-info :secret)))
                         (if (functionp secret)
                             (funcall secret)
                           secret))
                     (read-passwd "App password: "))))
    (cons handle password)))

(defun bluemacs--make-request (endpoint method &optional data callback)
  "Make HTTP request to Bluesky API.
ENDPOINT is the API endpoint (without base URL).
METHOD is the HTTP method (GET, POST, etc.).
DATA is optional JSON data to send.
CALLBACK is called with parsed JSON response."
  (let* ((url (concat bluemacs-instance endpoint))
         (url-request-method method)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when bluemacs-access-token
                `(("Authorization" . ,(concat "Bearer " bluemacs-access-token))))))
         (url-request-data (when data (encode-coding-string (json-encode data) 'utf-8))))
    (url-retrieve
     url
     (lambda (status)
       (condition-case err
           (progn
             (goto-char (point-min))
             (when (re-search-forward "\n\n" nil t)
               (let* ((response-body (buffer-substring-no-properties (point) (point-max)))
                      (json-object-type 'plist)
                      (json-array-type 'list)
                      (json-key-type 'keyword)
                      (response (condition-case parse-err
                                    (json-read-from-string response-body)
                                  (error
                                   (message "JSON parse error: %s" (error-message-string parse-err))
                                   nil))))
                 (when callback
                   (funcall callback response status)))))
         (error
          (message "Request error: %s" (error-message-string err))
          (when callback
            (funcall callback nil status)))))
     nil t)))

;;;###autoload
(defun bluemacs-login ()
  "Log in to Bluesky."
  (interactive)
  (let ((credentials (bluemacs--get-credentials)))
    (bluemacs--make-request
     "/xrpc/com.atproto.server.createSession"
     "POST"
     `((identifier . ,(car credentials))
       (password . ,(cdr credentials)))
     (lambda (response status)
       (if (plist-get response :accessJwt)
           (progn
             (setq bluemacs-access-token (plist-get response :accessJwt)
                   bluemacs-did (plist-get response :did)
                   bluemacs-handle (plist-get response :handle))
             (message "Logged in as %s" bluemacs-handle))
         (message "Login failed: %s" (or (plist-get response :message) "Unknown error")))))))

;;;###autoload
(defun bluemacs-logout ()
  "Log out from Bluesky."
  (interactive)
  (setq bluemacs-access-token nil
        bluemacs-did nil
        bluemacs-handle nil)
  (message "Logged out from Bluesky"))

;;; Timeline

(defun bluemacs--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (format-time-string "%Y-%m-%d %H:%M" (date-to-time timestamp))
    "Unknown time"))

(defun bluemacs--format-post (post)
  "Format a single POST for display."
  (condition-case err
      (let* ((post-data (plist-get post :post))
             (record (plist-get (or post-data post) :record))
             (author (plist-get (or post-data post) :author))
             (author-handle (or (plist-get author :handle) "unknown"))
             (author-display (or (plist-get author :displayName) author-handle))
             (text (or (plist-get record :text) "[No text]"))
             (created-at (plist-get record :createdAt))
             (like-count (or (plist-get (or post-data post) :likeCount) 0))
             (repost-count (or (plist-get (or post-data post) :repostCount) 0))
             (reply-count (or (plist-get (or post-data post) :replyCount) 0)))
        (format "%s (@%s) - %s\n%s\n[replies: %d  reposts: %d  likes: %d]\n%s\n"
                (propertize author-display 'face 'bold)
                author-handle
                (bluemacs--format-timestamp created-at)
                text
                reply-count
                repost-count
                like-count
                (make-string 80 ?-)))
    (error
     (message "Error formatting post: %s" (error-message-string err))
     (format "[Error displaying post: %s]\n%s\n"
             (error-message-string err)
             (make-string 80 ?-)))))

(defun bluemacs--display-timeline (posts)
  "Display POSTS in timeline buffer."
  (with-current-buffer (get-buffer-create bluemacs-timeline-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "Bluesky Timeline (@%s)\n" bluemacs-handle)
                          'face '(:height 1.5 :weight bold))
              (make-string 80 ?=)
              "\n\n")
      (dolist (post posts)
        (insert (bluemacs--format-post post)))
      (goto-char (point-min))
      (bluemacs-mode))
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun bluemacs-timeline ()
  "Fetch and display Bluesky timeline."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in. Run M-x bluemacs-login first"))
  (message "Fetching timeline...")
  (bluemacs--make-request
   (format "/xrpc/app.bsky.feed.getTimeline?limit=%d" bluemacs-timeline-limit)
   "GET"
   nil
   (lambda (response status)
     (let ((feed (plist-get response :feed)))
       (if feed
           (bluemacs--display-timeline feed)
         (message "Failed to fetch timeline: %s"
                  (or (plist-get response :message) "Unknown error")))))))

;;;###autoload
(defun bluemacs-refresh-timeline ()
  "Refresh the current timeline."
  (interactive)
  (bluemacs-timeline))

;;; Major Mode

(defvar bluemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'bluemacs-refresh-timeline)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for `bluemacs-mode'.")

(define-derived-mode bluemacs-mode special-mode "Bluemacs"
  "Major mode for displaying Bluesky timeline.

\\{bluemacs-mode-map}"
  (setq truncate-lines nil
        buffer-read-only t))

;;; Footer

(provide 'bluemacs)

;;; bluemacs.el ends here
