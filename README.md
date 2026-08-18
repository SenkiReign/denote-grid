# denote-grid.el

It transforms denote directory into an Are.na style visual grid/moodboard inside emacs.

<img width="1379" height="942" alt="gridlight" src="https://github.com/user-attachments/assets/6817a495-5061-4120-a76d-068f11768d32" />



* **Denote-dired view:** Follows denote-dired matching. (whatever files are currently visible in a `dired` or `denote-dired` buffer)
* **Cluster view:** Groups linked notes together
* **Filtered view:** Filter/query based on tags or substring

### Thumbnail generation
* **Image Files:** Native Emacs image scaling (zero dependencies).
* **Notes (`.md`, `.org`, `.txt`):** Synthesizes a small SVG "card" on the fly using title, body snippet, and tags (uses `svg.el`, built into Emacs 27+).
* **Video Files:** Frame thumbnail generated via `ffmpeg` (if installed).
* **PDF Files:** First page rendered via `pdftoppm` (if installed; falls back to a placeholder card otherwise).

## Setup

Add the package to your load path and require it in your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/this/folder")
(require 'denote-grid)
```

## Usage

| Command | Description |
| :--- | :--- |
| `M-x denote-grid-open` | Opens a grid view of your entire Denote directory. |
| `M-x denote-grid-from-dired` | Opens a grid view of whatever files are currently visible in the active Dired buffer. |

## Keybindings

When in the `*denote-grid*` buffer:

| Key | Action |
| :--- | :--- |
| `RET` / `mouse-1` | Open file directly in Emacs |
| `&` | Open file with async shell command (e.g., `mpv`, `mupdf`) |
| `d` | Jump to file location in Dired |
| `/` | Filter items (supports plain text substring, `#tag`, or `#tag1 tag2`) |
| `s` | Cycle sort key (`date` → `title` → `tags` → `type`) |
| `r` | Reverse sort order |
| `c` | Toggle cluster mode (groups linked notes together) |
| `g` | Refresh (rescans files while maintaining current filter and sort) |
| `q` | Quit window |
