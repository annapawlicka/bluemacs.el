# bluemacs.el

A (WIP) Bluesky client for Emacs.

## Features

- Secure authentication with Bluesky
- View your timeline with UTF-8 support (including emojis)
- Display embedded images inline (in graphical Emacs)
- Display quote posts (posts that reference other posts)
- Create quote posts
- Compose and post text updates
- Reply to posts with proper threading
- Like and unlike posts
- Repost and unrepost posts
- View who reposted posts in your timeline
- View conversation threads with replies
- View notifications (likes, reposts, follows, mentions, replies, quotes)
- View user profiles with avatar, bio, and stats
- Clickable links in posts
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
| `r` | Reply to post at point        |
| `Q` | Quote post at point           |
| `l` | Like/unlike post at point     |
| `R` | Repost/unrepost post at point |
| `t` | View thread/replies           |
| `N` | View notifications            |
| `P` | View user profile             |
| `b` | Back to previous view         |
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

## Navigation

bluemacs maintains a navigation history stack, allowing you to browse through different views and return to where you came from:

- **Press `b`** to go back to the previous view (timeline, thread, profile, or notifications)
- The navigation stack remembers your scroll position in each buffer
- If the navigation stack is empty, `b` returns to the timeline
- You can navigate through multiple levels: timeline → thread → profile → back to thread → back to timeline

**Example navigation flow:**
1. View timeline
2. Press `t` on a post to view its thread (timeline is saved to history)
3. Press `P` on a reply to view that user's profile (thread is saved to history)
4. Press `b` to return to the thread view
5. Press `b` again to return to the timeline

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

## Replying to Posts

Reply to any post in your timeline or in thread view:

1. Navigate to the post you want to reply to
2. Press `r` (or run `M-x bluemacs-reply`)
3. A compose buffer opens showing which post you're replying to
4. Write your reply (max 300 characters)
5. Press `C-c C-c` to send or `C-c C-k` to cancel

**Features:**
- Replies maintain proper thread structure with parent/root references
- Works in both timeline and thread views
- Reply buffer shows the URI of the post you're replying to

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

## Liking Posts

Like or unlike any post with a single keypress:

1. Navigate to the post you want to like
2. Press `l` (or run `M-x bluemacs-toggle-like`)
3. The timeline refreshes to show the updated like status

**Features:**
- Posts you've liked show a ♥ symbol in the stats line
- Works in both timeline and thread views

## Reposting

Repost (boost/retweet) any post to share it with your followers:

1. Navigate to the post you want to repost
2. Press `R` (shift+r, or run `M-x bluemacs-toggle-repost`)
3. The timeline refreshes to show the updated repost status

**Features:**
- Posts you've reposted show a ♻ symbol in the stats line
- When viewing your timeline, reposted posts show who reposted them with a green header line
- Works in both timeline and thread views

**Display format for reposts in timeline:**
```
♻ Reposted by Alice Smith (@alice.bsky.social)
Bob Jones (@bob.bsky.social) - 2025-01-15 10:30
Post content here...
```

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

## Quote Posts

bluemacs supports both viewing and creating quote posts.

### Viewing Quote Posts

Quote posts are automatically displayed in your timeline with a bordered box around the quoted content.

**Features:**
- Quote posts are displayed with a bordered box around the quoted content
- Shows the author and text of the quoted post
- Works for both simple quote posts and quote posts with images
- Links in quoted posts are also clickable
- **Interactive**: Press `RET` (Enter) on a quoted post to view its full thread with all interactions
- The quoted post section is highlighted when you hover over it

**Usage:**
1. Navigate to a post that contains a quote (you'll see the bordered box)
2. Move your cursor anywhere within the quoted post section
3. Press `RET` (Enter) to open the quoted post's full thread
4. View all replies, likes, and reposts for that quoted post
5. Press `b` to return to your timeline

**Display format:**
```
┌─ Quoted Post ─────────────────────────────────────────────────
│ Author Name (@handle) - [press RET to view thread]
│ Text of the quoted post...
│ [Images if present]
└───────────────────────────────────────────────────────────────
```

### Creating Quote Posts

How to quote post:

1. Navigate to the post you want to quote
2. Press `Q` (shift+q, or run `M-x bluemacs-quote`)
3. A compose buffer opens showing which post you're quoting
4. Write your commentary (max 300 characters)
5. Press `C-c C-c` to send or `C-c C-k` to cancel

## Notifications

View your Bluesky notifications to see who has interacted with your posts:

```elisp
M-x bluemacs-notifications
```

Or press `N` from the timeline buffer.

**Features:**
- View all your Bluesky notifications in a dedicated buffer
- Different icons for each notification type:
  - ♥ Likes
  - ♻ Reposts
  - 👤 Follows
  - @ Mentions
  - ↩ Replies
  - 💬 Quote posts
- **Unread notifications** are shown in bold text
- Shows the post text (when applicable)
- Press `t` on any notification to view the full thread
- Fetches the 50 most recent notifications

**Display format:**
```
♥ Alice (@alice.bsky.social) liked your post
  Your post text here...
  [2025-10-22T10:30:00Z] - press 't' to view thread
--------------------------------------------------------------------------------
```

## User Profiles

View detailed user profiles including avatar, banner, bio, and statistics:

**From timeline or thread:**
1. Navigate to any post
2. Press `P` to view the author's profile

**Or manually:**
```elisp
M-x bluemacs-view-profile RET username.bsky.social RET
```

**Profile display includes:**
- Banner image (if available, in graphical Emacs)
- Avatar image (if available, in graphical Emacs)
- Display name and handle
- Bio/description
- Stats: post count, follower count, following count
- DID (Decentralized Identifier)

**Note:** Profile images only display in graphical Emacs. The profile information is shown in a dedicated buffer.

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
| `bluemacs-notifications`          | View notifications                       |
| `bluemacs-compose`                | Compose a new post in buffer             |
| `bluemacs-post`                   | Post text from minibuffer                |
| `bluemacs-reply`                  | Reply to post at point                   |
| `bluemacs-quote`                  | Quote post at point with commentary      |
| `bluemacs-toggle-like`            | Like or unlike post at point             |
| `bluemacs-toggle-repost`          | Repost or unrepost post at point         |
| `bluemacs-view-thread`            | View thread/replies for post at point    |
| `bluemacs-view-quoted-post`       | View thread for quoted post at point     |
| `bluemacs-view-profile`           | View user profile                        |
| `bluemacs-back-to-timeline`       | Return to previous view or timeline      |
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
- [x] Reply to posts
- [x] Like posts
- [x] Repost posts
- [x] Display quote posts
- [x] Create quote posts
- [x] Display repost attribution in timeline
- [x] Notifications
- [x] View user profiles

Future features planned:
- [ ] View recent posts when viewing profile

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

See [LICENSE](LICENSE) file for details.

## Links

- Repository: https://github.com/annapawlicka/bluemacs.el
- Bluesky: https://bsky.app
- AT Protocol Documentation: https://atproto.com
