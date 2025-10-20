# bluemacs.el

A (WIP) Bluesky client for Emacs.

## Features

- Secure authentication with Bluesky
- View your timeline with UTF-8 support (including emojis)
- Display embedded images inline (in graphical Emacs)
- Compose and post text updates
- View conversation threads with replies
- Auto-refresh timeline at configurable intervals
- Simple keybindings

## Installation

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/annapawlicka/bluemacs.el.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/bluemacs.el")
   (require 'bluemacs)
   ```

### Using `use-package`

```elisp
(use-package bluemacs
  :load-path "/path/to/bluemacs.el"
  :config
  (setq bluemacs-auto-refresh-interval 60)) ; Optional: auto-refresh every 60 seconds
```

## Usage

### Authentication

Before using bluemacs, you need to create an **App Password** (not your main Bluesky password):

1. Go to https://bsky.app/settings/app-passwords
2. Create a new app password
3. Save it securely

### First Login

```elisp
M-x bluemacs-login
```

You'll be prompted for:
- Your Bluesky handle (e.g., `username.bsky.social`), you can also use your email
- Your app password

**Note:** Credentials are only stored in memory by default. See [Saving Credentials](#saving-credentials) to persist them.

### Viewing Timeline

```elisp
M-x bluemacs-timeline
```

This opens a buffer displaying your Bluesky timeline.

## Keybindings

In the timeline buffer:

| Key | Action                        |
|-----|-------------------------------|
| `g` | Refresh timeline              |
| `c` | Compose new post              |
| `t` | View thread/replies           |
| `b` | Back to timeline              |
| `a` | Toggle auto-refresh           |
| `i` | Set refresh interval          |
| `I` | Toggle image display          |
| `q` | Quit window                   |
| `n` | Next line                     |
| `p` | Previous line                 |
| `?` | Describe mode (show help)     |
| `SPC` | Scroll up                   |
| `DEL` | Scroll down                 |
| `<` | Beginning of buffer           |
| `>` | End of buffer                 |

## Auto-Refresh

bluemacs supports automatic timeline refreshing:

### Configure via Emacs settings

```elisp
;; Set auto-refresh interval (in seconds)
(setq bluemacs-auto-refresh-interval 60)  ; Refresh every 60 seconds
```

### Configure interactively

From the timeline buffer:
- Press `i` to set the refresh interval
- Press `a` to toggle auto-refresh on/off

Or use commands:
```elisp
M-x bluemacs-set-refresh-interval
M-x bluemacs-toggle-auto-refresh
```

**Note:** Auto-refresh only works when the timeline buffer is visible, so it won't waste resources in the background.

## Posting

bluemacs allows you to compose and post text updates to Bluesky.

### Compose in Buffer (Recommended)

1. Press `c` in the timeline (or run `M-x bluemacs-compose`)
2. A compose buffer opens with instructions
3. Write your post (max 300 characters)
4. Press `C-c C-c` to send or `C-c C-k` to cancel

### Quick Post from Minibuffer

```elisp
M-x bluemacs-post RET
Post text: Your message here RET
```

**Note:** Posts are limited to 300 characters. The package will validate length before posting.

## Viewing Threads

View conversation threads with all replies:

1. Navigate to any post in the timeline
2. Press `t` to view the full thread
3. Replies are indented to show conversation hierarchy
4. Press `b` to go back to the timeline

**Features:**
- Fetches up to 10 levels of nested replies
- 2-space indentation per reply level
- Shows reply counts and hints when posts have replies

## Images

bluemacs can display embedded images inline when running in graphical Emacs.

### Features

- Automatic display of embedded images in posts
- Image resizing to fit configured dimensions
- Alt text support
- Works only in graphical Emacs (shows alt text in terminal mode)

### Usage

**Toggle images on/off:**
- Press `I` (capital i) in the timeline buffer
- Or run `M-x bluemacs-toggle-images`

**Customize image size:**
```elisp
(setq bluemacs-image-max-width 600)   ;; Default: 400
(setq bluemacs-image-max-height 400)  ;; Default: 300
```

**Disable images by default:**
```elisp
(setq bluemacs-display-images nil)
```

**Note:** Images only display in graphical Emacs. In terminal mode, alt text is shown instead.

## Saving Credentials

By default, your password is not saved and you'll need to log in each time you restart Emacs. To save credentials securely:

### Option 1: Plain text (not recommended)

Create `~/.authinfo`:
```bash
echo "machine bsky.social login your.handle.bsky.social password your-app-password" > ~/.authinfo
chmod 600 ~/.authinfo
```

### Option 2: Encrypted (recommended)

Create `~/.authinfo.gpg` (requires GPG):
```bash
echo "machine bsky.social login your.handle.bsky.social password your-app-password" | gpg -c -o ~/.authinfo.gpg
```

With credentials saved, `bluemacs-login` will automatically use them without prompting.

## Customization

### Available Options

```elisp
;; Bluesky instance URL (default: "https://bsky.social")
(setq bluemacs-instance "https://bsky.social")

;; Number of posts to fetch (default: 50)
(setq bluemacs-timeline-limit 20)

;; Auto-refresh interval in seconds (default: nil, disabled)
(setq bluemacs-auto-refresh-interval 120)

;; Display embedded images (default: t)
(setq bluemacs-display-images t)

;; Maximum image dimensions in pixels
(setq bluemacs-image-max-width 400)
(setq bluemacs-image-max-height 300)
```

### Example Configuration

```elisp
(use-package bluemacs
  :load-path "/path/to/bluemacs.el"
  :custom
  (bluemacs-timeline-limit 15)
  (bluemacs-auto-refresh-interval 90)
  :config
  (message "bluemacs loaded!"))
```

## Commands

| Command                           | Description                              |
|-----------------------------------|------------------------------------------|
| `bluemacs-login`                  | Log in to Bluesky                        |
| `bluemacs-logout`                 | Log out from current session             |
| `bluemacs-timeline`               | Fetch and display timeline               |
| `bluemacs-refresh-timeline`       | Refresh the current timeline             |
| `bluemacs-compose`                | Compose a new post in buffer             |
| `bluemacs-post`                   | Post text from minibuffer                |
| `bluemacs-view-thread`            | View thread/replies for post at point    |
| `bluemacs-back-to-timeline`       | Return to timeline from thread view      |
| `bluemacs-toggle-auto-refresh`    | Toggle auto-refresh on/off               |
| `bluemacs-set-refresh-interval`   | Set auto-refresh interval                |
| `bluemacs-toggle-images`          | Toggle image display on/off              |

## Requirements

- Emacs 27.1 or higher
- `json` (built-in)
- `url` (built-in)
- `auth-source` (built-in)

## Security Notes

1. **Always use App Passwords**, never your main Bluesky password
2. **Encrypt your authinfo file** using `.authinfo.gpg` instead of plain `.authinfo`
3. **Set proper file permissions**: `chmod 600 ~/.authinfo` or `~/.authinfo.gpg`
4. **Access tokens are stored in memory** and are cleared when you restart Emacs or run `bluemacs-logout`

## Roadmap

Completed features:
- [x] Compose and post new text skeets
- [x] Thread view

Future features planned:
- [ ] Reply to posts
- [ ] Like and repost functionality
- [ ] View user profiles
- [ ] Notifications

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

See [LICENSE](LICENSE) file for details.

## Links

- Repository: https://github.com/annapawlicka/bluemacs.el
- Bluesky: https://bsky.app
- AT Protocol Documentation: https://atproto.com
