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
;;
;; Auto-refresh:
;;   Set `bluemacs-auto-refresh-interval' to enable automatic timeline refresh.
;;   Use M-x bluemacs-set-refresh-interval to change the interval.
;;   Use M-x bluemacs-toggle-auto-refresh to toggle auto-refresh on/off.
;;   In the timeline buffer, press 'a' to toggle or 'i' to set interval.
;;
;; Images:
;;   Embedded images are displayed by default in graphical Emacs.
;;   Use M-x bluemacs-toggle-images or press 'I' in the timeline to toggle.
;;   Customize `bluemacs-image-max-width' and `bluemacs-image-max-height'.
;;
;; Posting:
;;   Use M-x bluemacs-compose or press 'c' in the timeline to compose a post.
;;   In the compose buffer, press C-c C-c to send, C-c C-k to cancel.
;;   Or use M-x bluemacs-post to post text directly from the minibuffer.
;;
;; Viewing Threads:
;;   Press 't' on any post to view its thread with all replies.
;;   Replies are indented to show conversation hierarchy.  Press 'b' to go
;;   back to Timeline.

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

(defcustom bluemacs-timeline-limit 50
  "Number of posts to fetch from timeline."
  :type 'integer
  :group 'bluemacs)

(defcustom bluemacs-auto-refresh-interval nil
  "Auto-refresh interval for timeline in seconds.
If nil, auto-refresh is disabled.
If set to a number, the timeline will automatically refresh at that interval."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'bluemacs)

(defcustom bluemacs-display-images t
  "Whether to display embedded images in posts."
  :type 'boolean
  :group 'bluemacs)

(defcustom bluemacs-image-max-width 400
  "Maximum width for displayed images in pixels."
  :type 'integer
  :group 'bluemacs)

(defcustom bluemacs-image-max-height 300
  "Maximum height for displayed images in pixels."
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

(defvar bluemacs-refresh-timer nil
  "Timer object for auto-refreshing timeline.")

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
             (set-buffer-multibyte t)
             (goto-char (point-min))
             (when (re-search-forward "\n\n" nil t)
               (let* ((response-body (decode-coding-string
                                      (buffer-substring (point) (point-max))
                                      'utf-8))
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
     (lambda (response _status)
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

;;; Images

(defun bluemacs--download-image (url)
  "Download image from URL and return it as an Emacs image object."
  (when (and bluemacs-display-images (display-graphic-p))
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url t)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (when (re-search-forward "\n\n" nil t)
                (let* ((image-data (buffer-substring-no-properties (point) (point-max)))
                       (image (create-image image-data nil t)))
                  (kill-buffer)
                  image)))))
      (error
       (message "Error downloading image: %s" (error-message-string err))
       nil))))

(defun bluemacs--resize-image (image)
  "Resize IMAGE to fit within configured dimensions."
  (when image
    (let* ((size (image-size image t))
           (width (car size))
           (height (cdr size))
           (max-width bluemacs-image-max-width)
           (max-height bluemacs-image-max-height)
           (scale (min (/ (float max-width) width)
                      (/ (float max-height) height)
                      1.0)))
      (when (< scale 1.0)
        (create-image (plist-get (cdr image) :data)
                     (plist-get (cdr image) :type)
                     t
                     :width (round (* width scale))
                     :height (round (* height scale)))))))

(defun bluemacs--format-embed-images (embed)
  "Format EMBED images for display."
  (when bluemacs-display-images
    (let ((embed-type (plist-get embed :$type)))
      (cond
       ;; Handle image embeds
       ((or (string= embed-type "app.bsky.embed.images")
            (string= embed-type "app.bsky.embed.images#view"))
        (let ((images (plist-get embed :images)))
          (when images
            (message "Found %d images" (length images))
            (if (display-graphic-p)
                (mapconcat
                 (lambda (img)
                   (let* ((thumb (plist-get img :thumb))
                          (fullsize (plist-get img :fullsize))
                          (url (or thumb fullsize))
                          (alt (or (plist-get img :alt) ""))
                          (image (when url
                                   (message "Downloading image from: %s" url)
                                   (bluemacs--download-image url))))
                     (if image
                         (progn
                           (message "Image downloaded successfully")
                           (let ((resized (or (bluemacs--resize-image image) image)))
                             (concat
                              (propertize " " 'display resized)
                              (when (not (string-empty-p alt))
                                (format "\n[Image: %s]" alt))
                              "\n")))
                       (progn
                         (message "Failed to download image")
                         (when (not (string-empty-p alt))
                           (format "[Image: %s]\n" alt))))))
                 images
                 "")
              ;; Terminal mode - just show alt text
              (mapconcat
               (lambda (img)
                 (let ((alt (or (plist-get img :alt) "")))
                   (if (not (string-empty-p alt))
                       (format "[Image: %s]\n" alt)
                     "[Image]\n")))
               images
               "")))))))))

;;; Timeline

(defun bluemacs--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (format-time-string "%Y-%m-%d %H:%M" (date-to-time timestamp))
    "Unknown time"))

(defun bluemacs--format-post (post &optional indent-level)
  "Format a single POST for display.
INDENT-LEVEL determines indentation for nested replies (default 0)."
  (condition-case err
      (let* ((indent-level (or indent-level 0))
             (indent-str (make-string (* indent-level 2) ?\s))
             (post-data (plist-get post :post))
             (uri (plist-get (or post-data post) :uri))
             (record (plist-get (or post-data post) :record))
             (author (plist-get (or post-data post) :author))
             (embed (plist-get (or post-data post) :embed))
             (author-handle (or (plist-get author :handle) "unknown"))
             (author-display (or (plist-get author :displayName) author-handle))
             (text (or (plist-get record :text) "[No text]"))
             (created-at (plist-get record :createdAt))
             (like-count (or (plist-get (or post-data post) :likeCount) 0))
             (repost-count (or (plist-get (or post-data post) :repostCount) 0))
             (reply-count (or (plist-get (or post-data post) :replyCount) 0))
             (images (bluemacs--format-embed-images embed)))
        (propertize
         (concat
          indent-str
          (format "%s (@%s) - %s\n%s%s\n"
                  (propertize author-display 'face 'bold)
                  author-handle
                  (bluemacs--format-timestamp created-at)
                  indent-str
                  text)
          (when images
            (concat indent-str images "\n"))
          (format "%s[replies: %d  reposts: %d  likes: %d]%s\n%s%s\n"
                  indent-str
                  reply-count
                  repost-count
                  like-count
                  (if (> reply-count 0) " - press 't' to view thread" "")
                  indent-str
                  (make-string 80 ?-)))
         'bluemacs-post-uri uri))
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
      (set-buffer-multibyte t)
      (setq buffer-file-coding-system 'utf-8)
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
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (message "Fetching timeline...")
  (bluemacs--make-request
   (format "/xrpc/app.bsky.feed.getTimeline?limit=%d" bluemacs-timeline-limit)
   "GET"
   nil
   (lambda (response _status)
     (let ((feed (plist-get response :feed)))
       (if feed
           (progn
             (bluemacs--display-timeline feed)
             (message "Timeline fetched: %d posts" (length feed)))
         (message "Failed to fetch timeline: %s"
                  (or (plist-get response :message) "Unknown error")))))))

;;;###autoload
(defun bluemacs-refresh-timeline ()
  "Refresh the current timeline."
  (interactive)
  (bluemacs-timeline))

;;;###autoload
(defun bluemacs-toggle-images ()
  "Toggle display of embedded images."
  (interactive)
  (setq bluemacs-display-images (not bluemacs-display-images))
  (if (display-graphic-p)
      (message "Image display %s" (if bluemacs-display-images "enabled" "disabled"))
    (message "Images only work in graphical Emacs (not terminal mode)"))
  (when (get-buffer bluemacs-timeline-buffer)
    (bluemacs-refresh-timeline)))

;;; Thread/Replies

(defun bluemacs--get-post-uri-at-point ()
  "Get the post URI at point."
  (get-text-property (point) 'bluemacs-post-uri))

;;;###autoload
(defun bluemacs-view-thread ()
  "View the thread/replies for the post at point."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (let ((uri (bluemacs--get-post-uri-at-point)))
    (unless uri
      (user-error "No post at point"))
    (message "Fetching thread...")
    (bluemacs--make-request
     (format "/xrpc/app.bsky.feed.getPostThread?uri=%s&depth=10"
             (url-hexify-string uri))
     "GET"
     nil
     (lambda (response _status)
       (let ((thread (plist-get response :thread)))
         (if thread
             (bluemacs--display-thread thread)
           (message "Failed to fetch thread: %s"
                    (or (plist-get response :message) "Unknown error"))))))))

(defun bluemacs--display-thread (thread)
  "Display THREAD in a buffer."
  (let ((buffer (get-buffer-create "*Bluesky Thread*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-buffer-multibyte t)
        (setq buffer-file-coding-system 'utf-8)
        (insert (propertize "Thread View" 'face '(:height 1.5 :weight bold))
                (propertize " (press 'b' to go back to timeline)\n" 'face 'italic)
                (make-string 80 ?=)
                "\n\n")
        (bluemacs--insert-thread thread 0)
        (goto-char (point-min))
        (bluemacs-mode))
      (switch-to-buffer (current-buffer)))
    (message "Thread loaded")))

(defun bluemacs--insert-thread (thread indent-level)
  "Insert THREAD with INDENT-LEVEL into current buffer."
  (when thread
    (let* ((post (plist-get thread :post))
           (replies (plist-get thread :replies)))
      ;; Insert the main post
      (when post
        (insert (bluemacs--format-post (list :post post) indent-level)))
      ;; Insert replies recursively
      (when replies
        (dolist (reply replies)
          (bluemacs--insert-thread reply (1+ indent-level)))))))

;;;###autoload
(defun bluemacs-back-to-timeline ()
  "Close current view and return to timeline."
  (interactive)
  (let ((timeline-buffer (get-buffer bluemacs-timeline-buffer)))
    (if timeline-buffer
        (progn
          (kill-buffer)
          (switch-to-buffer timeline-buffer))
      (progn
        (kill-buffer)
        (bluemacs-timeline)))))

;;; Posting

(defun bluemacs--get-current-timestamp ()
  "Get current timestamp in ISO 8601 format for AT Protocol."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time) t))

;;;###autoload
(defun bluemacs-post (text)
  "Post TEXT as a new skeet to Bluesky."
  (interactive "sPost text: ")
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (when (string-empty-p (string-trim text))
    (user-error "Post text cannot be empty"))
  (when (> (length text) 300)
    (user-error "Post text too long (max 300 characters, got %d)" (length text)))
  (let ((record `((text . ,text)
                  (createdAt . ,(bluemacs--get-current-timestamp))
                  ($type . "app.bsky.feed.post"))))
    (bluemacs--make-request
     "/xrpc/com.atproto.repo.createRecord"
     "POST"
     `((repo . ,bluemacs-did)
       (collection . "app.bsky.feed.post")
       (record . ,record))
     (lambda (response _status)
       (if (plist-get response :uri)
           (message "Posted successfully!")
         (message "Failed to post: %s"
                  (or (plist-get response :message) "Unknown error")))))))

;;;###autoload
(defun bluemacs-compose ()
  "Compose a new post in a dedicated buffer."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (let ((buffer (get-buffer-create "*Bluesky Compose*")))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (insert ";; Write your post below (max 300 characters)\n")
      (insert ";; Press C-c C-c to post, C-c C-k to cancel\n\n")
      (local-set-key (kbd "C-c C-c") #'bluemacs-compose-send)
      (local-set-key (kbd "C-c C-k") #'bluemacs-compose-cancel))
    (switch-to-buffer-other-window buffer)
    (goto-char (point-max))))

(defun bluemacs-compose-send ()
  "Send the post from the compose buffer."
  (interactive)
  (let ((text (buffer-substring-no-properties
               (save-excursion
                 (goto-char (point-min))
                 (forward-line 3)
                 (point))
               (point-max))))
    (when (string-empty-p (string-trim text))
      (user-error "Post text cannot be empty"))
    (when (> (length text) 300)
      (user-error "Post text too long (max 300 characters, got %d)" (length text)))
    (bluemacs-post text)
    (kill-buffer)
    (delete-window)))

(defun bluemacs-compose-cancel ()
  "Cancel composing and close the buffer."
  (interactive)
  (when (yes-or-no-p "Discard post? ")
    (kill-buffer)
    (delete-window)))

;;; Auto-refresh

(declare-function bluemacs-timeline "bluemacs")

(defun bluemacs--start-refresh-timer ()
  "Start the auto-refresh timer if configured."
  (bluemacs--stop-refresh-timer)
  (when (and bluemacs-auto-refresh-interval
             (> bluemacs-auto-refresh-interval 0))
    (setq bluemacs-refresh-timer
          (run-at-time bluemacs-auto-refresh-interval
                       bluemacs-auto-refresh-interval
                       #'bluemacs--auto-refresh-timeline))))

(defun bluemacs--stop-refresh-timer ()
  "Stop the auto-refresh timer."
  (when bluemacs-refresh-timer
    (cancel-timer bluemacs-refresh-timer)
    (setq bluemacs-refresh-timer nil)))

(defun bluemacs--auto-refresh-timeline ()
  "Auto-refresh timeline if the timeline buffer is visible."
  (when (and (buffer-live-p (get-buffer bluemacs-timeline-buffer))
             (get-buffer-window bluemacs-timeline-buffer t))
    (bluemacs-timeline)))

;;;###autoload
(defun bluemacs-toggle-auto-refresh ()
  "Toggle auto-refresh for the timeline."
  (interactive)
  (if bluemacs-refresh-timer
      (progn
        (bluemacs--stop-refresh-timer)
        (message "Auto-refresh disabled"))
    (if bluemacs-auto-refresh-interval
        (progn
          (bluemacs--start-refresh-timer)
          (message "Auto-refresh enabled (every %d seconds)" bluemacs-auto-refresh-interval))
      (let ((interval (read-number "Refresh interval (seconds): " 60)))
        (setq bluemacs-auto-refresh-interval interval)
        (bluemacs--start-refresh-timer)
        (message "Auto-refresh enabled (every %d seconds)" interval)))))

;;;###autoload
(defun bluemacs-set-refresh-interval (seconds)
  "Set the auto-refresh interval to SECONDS."
  (interactive "nRefresh interval (seconds, 0 to disable): ")
  (setq bluemacs-auto-refresh-interval (if (> seconds 0) seconds nil))
  (if bluemacs-auto-refresh-interval
      (progn
        (bluemacs--start-refresh-timer)
        (message "Auto-refresh set to %d seconds" seconds))
    (bluemacs--stop-refresh-timer)
    (message "Auto-refresh disabled")))

;;; Major Mode

(defvar bluemacs-mode-map nil
  "Keymap for `bluemacs-mode'.")

(unless bluemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "g" #'bluemacs-refresh-timeline)
    (define-key map "c" #'bluemacs-compose)
    (define-key map "t" #'bluemacs-view-thread)
    (define-key map "b" #'bluemacs-back-to-timeline)
    (define-key map "a" #'bluemacs-toggle-auto-refresh)
    (define-key map "i" #'bluemacs-set-refresh-interval)
    (define-key map "I" #'bluemacs-toggle-images)
    (define-key map "q" #'quit-window)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "?" #'describe-mode)
    (define-key map " " #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map (kbd "S-SPC") #'scroll-down-command)
    (define-key map "<" #'beginning-of-buffer)
    (define-key map ">" #'end-of-buffer)
    (setq bluemacs-mode-map map)))

(define-derived-mode bluemacs-mode nil "Bluemacs"
  "Major mode for displaying Bluesky timeline.

Key bindings:
\\{bluemacs-mode-map}"
  (kill-all-local-variables)
  (use-local-map bluemacs-mode-map)
  (setq major-mode 'bluemacs-mode
        mode-name "Bluemacs"
        truncate-lines nil
        buffer-read-only t)
  (add-hook 'kill-buffer-hook #'bluemacs--stop-refresh-timer nil t)
  (bluemacs--start-refresh-timer))

;;; Footer

(provide 'bluemacs)

;;; bluemacs.el ends here
