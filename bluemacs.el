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
;;
;; Replying to a post:
;;   Use 'r' on any post to open compose buffer.  In the compose buffer, press
;;   C-c C-c to send, C-c C-k to cancel.

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

(defvar-local bluemacs-reply-data nil
  "Buffer-local variable storing reply data (parent post info).")

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

(defun bluemacs--format-quoted-post (quoted-post indent-str)
  "Format a QUOTED-POST for display with INDENT-STR."
  ;; The quoted-post can have different structures:
  ;; - Direct record with :author, :value (post data)
  ;; - Or wrapped in :$type "app.bsky.embed.record#viewRecord"
  (let* (;; For viewRecord type, data is directly on quoted-post with :author and :value
         (author (plist-get quoted-post :author))
         (uri (plist-get quoted-post :uri))  ;; URI of the quoted post
         (cid (plist-get quoted-post :cid))  ;; CID of the quoted post
         (value (plist-get quoted-post :value))  ;; The actual post record
         (author-handle (or (plist-get author :handle) "unknown"))
         (author-display (or (plist-get author :displayName) author-handle))
         (text (or (plist-get value :text) "[No text]"))
         (facets (plist-get value :facets))
         (embeds-data (plist-get value :embed))  ;; Embeds within the quoted post
         (formatted-text (bluemacs--buttonize-with-facets text facets))
         (quoted-embeds (when embeds-data
                          (bluemacs--format-embeds embeds-data (concat indent-str "│ ")))))
    (propertize
     (concat
      indent-str
      "┌─ Quoted Post ─────────────────────────────────────────────────\n"
      indent-str
      "│ "
      (format "%s (@%s)" author-display author-handle)
      (when uri " - [press RET to view thread]")
      "\n"
      indent-str
      "│ "
      formatted-text
      "\n"
      (when quoted-embeds
        (concat quoted-embeds "\n"))
      indent-str
      "└───────────────────────────────────────────────────────────────\n")
     'face '(:foreground "gray")
     'bluemacs-quoted-post-uri uri
     'bluemacs-quoted-post-cid cid
     'keymap (let ((map (make-sparse-keymap)))
               (define-key map (kbd "RET") #'bluemacs-view-quoted-post)
               map)
     'mouse-face 'highlight
     'help-echo "Press RET to view this quoted post's thread")))

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

(defun bluemacs--format-embeds (embed indent-str)
  "Format EMBED for display with INDENT-STR.
Handles both image embeds and quote post (record) embeds."
  (when embed
    (let ((embed-type (plist-get embed :$type)))
      (cond
       ;; Quote post (record embed)
       ((or (string= embed-type "app.bsky.embed.record")
            (string= embed-type "app.bsky.embed.record#view"))
        (let ((record (plist-get embed :record)))
          (when record
            (bluemacs--format-quoted-post record indent-str))))

       ;; Record with media (quote post with images)
       ((or (string= embed-type "app.bsky.embed.recordWithMedia")
            (string= embed-type "app.bsky.embed.recordWithMedia#view"))
        (let ((record (plist-get embed :record))
              (media (plist-get embed :media)))
          (concat
           ;; First show the quoted post
           (when record
             (let ((record-embed (plist-get record :record)))
               (when record-embed
                 (bluemacs--format-quoted-post record-embed indent-str))))
           ;; Then show any images
           (when media
             (concat indent-str (bluemacs--format-embed-images media))))))

       ;; Regular images
       ((or (string= embed-type "app.bsky.embed.images")
            (string= embed-type "app.bsky.embed.images#view"))
        (bluemacs--format-embed-images embed))

       ;; External link embeds - just show a simple indicator
       ((or (string= embed-type "app.bsky.embed.external")
            (string= embed-type "app.bsky.embed.external#view"))
        (let ((external (plist-get embed :external)))
          (when external
            (let ((title (plist-get external :title))
                  (uri (plist-get external :uri)))
              (when (or title uri)
                (concat indent-str
                        (format "[Link: %s]\n" (or title uri))))))))))))

;;; Timeline

(defun bluemacs--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (format-time-string "%Y-%m-%d %H:%M" (date-to-time timestamp))
    "Unknown time"))

(defun bluemacs--byte-to-char-pos (text byte-pos)
  "Convert BYTE-POS (UTF-8 byte offset) in TEXT to character position."
  (let ((byte-count 0)
        (char-pos 0))
    (while (and (< char-pos (length text))
                (< byte-count byte-pos))
      (let* ((char (aref text char-pos))
             (char-bytes (length (encode-coding-string (string char) 'utf-8))))
        (setq byte-count (+ byte-count char-bytes))
        (setq char-pos (1+ char-pos))))
    char-pos))

(defun bluemacs--buttonize-with-facets (text facets)
  "Make URLs in TEXT clickable using FACETS metadata.
FACETS is a list of facet objects from Bluesky API that describe
links, mentions, and other rich text features."
  (if (not facets)
      ;; Fallback: try to detect URLs with regex if no facets provided
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward
                "\\bhttps?://[^ \t\n\r\"'<>]*[^ \t\n\r\"'<>.,;:?!]" nil t)
          (let ((url (match-string 0)))
            (make-text-button (match-beginning 0) (match-end 0)
                              'url url
                              'face 'link
                              'follow-link t
                              'action (lambda (button)
                                        (browse-url (button-get button 'url)))
                              'help-echo url)))
        (buffer-substring (point-min) (point-max)))
    ;; Use facets to create buttons
    (with-temp-buffer
      (insert text)
      (dolist (facet facets)
        (let* ((index (plist-get facet :index))
               (features (plist-get facet :features))
               (byte-start (plist-get index :byteStart))
               (byte-end (plist-get index :byteEnd)))
          (when (and byte-start byte-end features)
            (dolist (feature features)
              (let ((type (plist-get feature :$type)))
                (when (string= type "app.bsky.richtext.facet#link")
                  (let* ((uri (plist-get feature :uri))
                         ;; Convert byte positions to character positions
                         (char-start (1+ (bluemacs--byte-to-char-pos text byte-start)))
                         (char-end (1+ (bluemacs--byte-to-char-pos text byte-end))))
                    (when (and uri (> char-end char-start) (<= char-end (point-max)))
                      (make-text-button char-start char-end
                                        'url uri
                                        'face 'link
                                        'follow-link t
                                        'action (lambda (button)
                                                  (browse-url (button-get button 'url)))
                                        'help-echo uri)))))))))
      (buffer-substring (point-min) (point-max)))))

(defun bluemacs--format-post (post &optional indent-level root-uri root-cid)
  "Format a single POST for display.
INDENT-LEVEL determines indentation for nested replies (default 0).
ROOT-URI and ROOT-CID identify the root post of the thread for reply tracking."
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
             (facets (plist-get record :facets))
             (created-at (plist-get record :createdAt))
             (like-count (or (plist-get (or post-data post) :likeCount) 0))
             (repost-count (or (plist-get (or post-data post) :repostCount) 0))
             (reply-count (or (plist-get (or post-data post) :replyCount) 0))
             (viewer (plist-get (or post-data post) :viewer))
             (like-uri (when viewer (plist-get viewer :like)))
             (liked (not (null like-uri)))
             (repost-uri (when viewer (plist-get viewer :repost)))
             (reposted (not (null repost-uri)))
             (embeds (bluemacs--format-embeds embed indent-str))
             (formatted-text (bluemacs--buttonize-with-facets text facets))
             (post-header (format "%s (@%s) - %s\n%s"
                                  (propertize author-display 'face 'bold)
                                  author-handle
                                  (bluemacs--format-timestamp created-at)
                                  indent-str))
             (post-footer (format "\n%s[replies: %d  reposts: %d%s  likes: %d%s]%s\n%s%s\n"
                                  indent-str
                                  reply-count
                                  repost-count
                                  (if reposted " ♻" "")
                                  like-count
                                  (if liked " ♥" "")
                                  (if (> reply-count 0) " - press 't' to view thread" "")
                                  indent-str
                                  (make-string 80 ?-)))
             (cid (plist-get (or post-data post) :cid))
             (author-did (plist-get author :did))
             ;; For top-level posts, this post is the root. For replies, use provided root.
             (actual-root-uri (or root-uri uri))
             (actual-root-cid (or root-cid cid)))
        (concat
         (propertize indent-str
                     'bluemacs-post-uri uri
                     'bluemacs-post-cid cid
                     'bluemacs-post-author-did author-did
                     'bluemacs-root-uri actual-root-uri
                     'bluemacs-root-cid actual-root-cid
                     'bluemacs-like-uri like-uri
                     'bluemacs-repost-uri repost-uri)
         (propertize post-header
                     'bluemacs-post-uri uri
                     'bluemacs-post-cid cid
                     'bluemacs-post-author-did author-did
                     'bluemacs-root-uri actual-root-uri
                     'bluemacs-root-cid actual-root-cid
                     'bluemacs-like-uri like-uri
                     'bluemacs-repost-uri repost-uri)
         (propertize formatted-text
                     'bluemacs-post-uri uri
                     'bluemacs-post-cid cid
                     'bluemacs-post-author-did author-did
                     'bluemacs-root-uri actual-root-uri
                     'bluemacs-root-cid actual-root-cid
                     'bluemacs-like-uri like-uri
                     'bluemacs-repost-uri repost-uri)
         (when embeds
           (propertize (concat "\n" embeds)
                       'bluemacs-post-uri uri
                       'bluemacs-post-cid cid
                       'bluemacs-post-author-did author-did
                       'bluemacs-root-uri actual-root-uri
                       'bluemacs-root-cid actual-root-cid
                       'bluemacs-like-uri like-uri
                       'bluemacs-repost-uri repost-uri))
         (propertize post-footer
                     'bluemacs-post-uri uri
                     'bluemacs-post-cid cid
                     'bluemacs-post-author-did author-did
                     'bluemacs-root-uri actual-root-uri
                     'bluemacs-root-cid actual-root-cid
                     'bluemacs-like-uri like-uri
                     'bluemacs-repost-uri repost-uri)))
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

(defun bluemacs--get-post-data-at-point ()
  "Get the post data (URI, CID, author DID, root info) at point for replying."
  (let ((uri (get-text-property (point) 'bluemacs-post-uri))
        (cid (get-text-property (point) 'bluemacs-post-cid))
        (author-did (get-text-property (point) 'bluemacs-post-author-did))
        (root-uri (get-text-property (point) 'bluemacs-root-uri))
        (root-cid (get-text-property (point) 'bluemacs-root-cid)))
    (when (and uri cid author-did)
      (list :uri uri
            :cid cid
            :author-did author-did
            :root-uri (or root-uri uri)  ;; If no root, this post is the root
            :root-cid (or root-cid cid)))))

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

;;;###autoload
(defun bluemacs-view-quoted-post ()
  "View the thread for the quoted post at point."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (let ((uri (get-text-property (point) 'bluemacs-quoted-post-uri)))
    (unless uri
      (user-error "No quoted post at point"))
    (message "Fetching quoted post thread...")
    (bluemacs--make-request
     (format "/xrpc/app.bsky.feed.getPostThread?uri=%s&depth=10"
             (url-hexify-string uri))
     "GET"
     nil
     (lambda (response _status)
       (let ((thread (plist-get response :thread)))
         (if thread
             (bluemacs--display-thread thread)
           (message "Failed to fetch quoted post thread: %s"
                    (or (plist-get response :message) "Unknown error"))))))))

(defun bluemacs--insert-thread (thread indent-level &optional root-uri root-cid)
  "Insert THREAD with INDENT-LEVEL into current buffer.
ROOT-URI and ROOT-CID identify the root post of the thread."
  (when thread
    (let* ((post (plist-get thread :post))
           (replies (plist-get thread :replies))
           (uri (plist-get post :uri))
           (cid (plist-get post :cid))
           ;; First post in thread is the root
           (actual-root-uri (or root-uri uri))
           (actual-root-cid (or root-cid cid)))
      ;; Insert the main post
      (when post
        (insert (bluemacs--format-post (list :post post) indent-level actual-root-uri actual-root-cid)))
      ;; Insert replies recursively, passing down the root
      (when replies
        (dolist (reply replies)
          (bluemacs--insert-thread reply (1+ indent-level) actual-root-uri actual-root-cid))))))

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
(defun bluemacs-post (text &optional reply-to)
  "Post TEXT as a new skeet to Bluesky.
If REPLY-TO is provided, post as a reply to that post.
REPLY-TO should be a plist with :uri, :cid, and :author-did."
  (interactive "sPost text: ")
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (when (string-empty-p (string-trim text))
    (user-error "Post text cannot be empty"))
  (when (> (length text) 300)
    (user-error "Post text too long (max 300 characters, got %d)" (length text)))
  (let* ((reply-ref (when reply-to
                      `((root . ((uri . ,(plist-get reply-to :root-uri))
                                 (cid . ,(plist-get reply-to :root-cid))))
                        (parent . ((uri . ,(plist-get reply-to :uri))
                                   (cid . ,(plist-get reply-to :cid)))))))
         (record `((text . ,text)
                   (createdAt . ,(bluemacs--get-current-timestamp))
                   ($type . "app.bsky.feed.post")
                   ,@(when reply-ref `((reply . ,reply-ref))))))
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

;;;###autoload
(defun bluemacs-reply ()
  "Compose a reply to the post at point."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (let ((post-data (bluemacs--get-post-data-at-point)))
    (unless post-data
      (user-error "No post at point to reply to"))
    (let ((buffer (get-buffer-create "*Bluesky Reply*")))
      (with-current-buffer buffer
        (erase-buffer)
        (text-mode)
        (insert ";; Write your reply below (max 300 characters)\n")
        (insert (format ";; Replying to post: %s\n" (plist-get post-data :uri)))
        (insert ";; Press C-c C-c to post, C-c C-k to cancel\n\n")
        ;; Store reply data as buffer-local variable
        (setq-local bluemacs-reply-data post-data)
        (local-set-key (kbd "C-c C-c") #'bluemacs-reply-send)
        (local-set-key (kbd "C-c C-k") #'bluemacs-compose-cancel))
      (switch-to-buffer-other-window buffer)
      (goto-char (point-max)))))

(defun bluemacs-reply-send ()
  "Send the reply from the compose buffer."
  (interactive)
  (unless (boundp 'bluemacs-reply-data)
    (user-error "No reply data found.  Use bluemacs-reply to compose a reply"))
  (let ((text (buffer-substring-no-properties
               (save-excursion
                 (goto-char (point-min))
                 (forward-line 4)
                 (point))
               (point-max)))
        (reply-data bluemacs-reply-data))
    (when (string-empty-p (string-trim text))
      (user-error "Reply text cannot be empty"))
    (when (> (length text) 300)
      (user-error "Reply text too long (max 300 characters, got %d)" (length text)))
    (bluemacs-post text reply-data)
    (kill-buffer)
    (delete-window)))

;;; Liking

;;;###autoload
(defun bluemacs-toggle-like ()
  "Like or unlike the post at point."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (let ((uri (get-text-property (point) 'bluemacs-post-uri))
        (cid (get-text-property (point) 'bluemacs-post-cid))
        (like-uri (get-text-property (point) 'bluemacs-like-uri)))
    (unless (and uri cid)
      (user-error "No post at point"))
    (if like-uri
        ;; Unlike: delete the like record
        (bluemacs--unlike-post like-uri)
      ;; Like: create a like record
      (bluemacs--like-post uri cid))))

(defun bluemacs--like-post (post-uri post-cid)
  "Create a like for the post identified by POST-URI and POST-CID."
  (let ((record `((subject . ((uri . ,post-uri)
                              (cid . ,post-cid)))
                  (createdAt . ,(bluemacs--get-current-timestamp))
                  ($type . "app.bsky.feed.like"))))
    (bluemacs--make-request
     "/xrpc/com.atproto.repo.createRecord"
     "POST"
     `((repo . ,bluemacs-did)
       (collection . "app.bsky.feed.like")
       (record . ,record))
     (lambda (response _status)
       (if (plist-get response :uri)
           (progn
             (message "Liked!")
             (bluemacs-refresh-timeline))
         (message "Failed to like: %s"
                  (or (plist-get response :message) "Unknown error")))))))

(defun bluemacs--unlike-post (like-uri)
  "Delete the like record identified by LIKE-URI."
  ;; Parse the like URI to extract repo and rkey
  ;; URI format: at://did:plc:xxx/app.bsky.feed.like/rkey
  (if (string-match "at://\\([^/]+\\)/app\\.bsky\\.feed\\.like/\\(.+\\)" like-uri)
      (let ((repo (match-string 1 like-uri))
            (rkey (match-string 2 like-uri)))
        (bluemacs--make-request
         "/xrpc/com.atproto.repo.deleteRecord"
         "POST"
         `((repo . ,repo)
           (collection . "app.bsky.feed.like")
           (rkey . ,rkey))
         (lambda (_response _status)
           ;; deleteRecord returns empty response on success
           (message "Unliked!")
           (bluemacs-refresh-timeline))))
    (message "Invalid like URI format: %s" like-uri)))

;;; Reposting

;;;###autoload
(defun bluemacs-toggle-repost ()
  "Repost or unrepost the post at point."
  (interactive)
  (unless bluemacs-access-token
    (user-error "Not logged in.  Run M-x bluemacs-login first"))
  (let ((uri (get-text-property (point) 'bluemacs-post-uri))
        (cid (get-text-property (point) 'bluemacs-post-cid))
        (repost-uri (get-text-property (point) 'bluemacs-repost-uri)))
    (unless (and uri cid)
      (user-error "No post at point"))
    (if repost-uri
        ;; Unrepost: delete the repost record
        (bluemacs--unrepost-post repost-uri)
      ;; Repost: create a repost record
      (bluemacs--repost-post uri cid))))

(defun bluemacs--repost-post (post-uri post-cid)
  "Create a repost for the post identified by POST-URI and POST-CID."
  (let ((record `((subject . ((uri . ,post-uri)
                              (cid . ,post-cid)))
                  (createdAt . ,(bluemacs--get-current-timestamp))
                  ($type . "app.bsky.feed.repost"))))
    (bluemacs--make-request
     "/xrpc/com.atproto.repo.createRecord"
     "POST"
     `((repo . ,bluemacs-did)
       (collection . "app.bsky.feed.repost")
       (record . ,record))
     (lambda (response _status)
       (if (plist-get response :uri)
           (progn
             (message "Reposted!")
             (bluemacs-refresh-timeline))
         (message "Failed to repost: %s"
                  (or (plist-get response :message) "Unknown error")))))))

(defun bluemacs--unrepost-post (repost-uri)
  "Delete the repost record identified by REPOST-URI."
  ;; Parse the repost URI to extract repo and rkey
  ;; URI format: at://did:plc:xxx/app.bsky.feed.repost/rkey
  (if (string-match "at://\\([^/]+\\)/app\\.bsky\\.feed\\.repost/\\(.+\\)" repost-uri)
      (let ((repo (match-string 1 repost-uri))
            (rkey (match-string 2 repost-uri)))
        (bluemacs--make-request
         "/xrpc/com.atproto.repo.deleteRecord"
         "POST"
         `((repo . ,repo)
           (collection . "app.bsky.feed.repost")
           (rkey . ,rkey))
         (lambda (_response _status)
           ;; deleteRecord returns empty response on success
           (message "Unreposted!")
           (bluemacs-refresh-timeline))))
    (message "Invalid repost URI format: %s" repost-uri)))

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
    (define-key map "r" #'bluemacs-reply)
    (define-key map "l" #'bluemacs-toggle-like)
    (define-key map "R" #'bluemacs-toggle-repost)
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
