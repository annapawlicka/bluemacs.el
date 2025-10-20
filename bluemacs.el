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

(defun bluemacs--format-post (post)
  "Format a single POST for display."
  (condition-case err
      (let* ((post-data (plist-get post :post))
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
        (concat
         (format "%s (@%s) - %s\n%s\n"
                 (propertize author-display 'face 'bold)
                 author-handle
                 (bluemacs--format-timestamp created-at)
                 text)
         (when images
           (concat images "\n"))
         (format "[replies: %d  reposts: %d  likes: %d]\n%s\n"
                 reply-count
                 repost-count
                 like-count
                 (make-string 80 ?-))))
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
    (user-error "Not logged in. Run M-x bluemacs-login first"))
  (message "Fetching timeline...")
  (bluemacs--make-request
   (format "/xrpc/app.bsky.feed.getTimeline?limit=%d" bluemacs-timeline-limit)
   "GET"
   nil
   (lambda (response status)
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
