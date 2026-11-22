;;; denote-grid.el --- Grid view for denote -*- lexical-binding: t; -*-

;; Author:  Senki R.
;; Keywords: denote, notes, multimedia, moodboard, emacs, org-mode
;; Package-Requires: ((emacs "27.1") (denote "1.0"))
;; Version: 0.4.0

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'dired)
(require 'color)
(require 'denote)
(require 'subr-x)
(require 'xml)
(require 'seq)

(defgroup denote-grid nil
  "An are.na-style local grid for denote notes, images, pdfs, and videos."
  :group 'convenience
  :prefix "denote-grid-")

(defcustom denote-grid-thumbnail-size 400
  "Max width/height in pixels for grid thumbnails."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-front-matter-regexp "\\`\\(#\\+\\|---\\|\\+\\+\\+\\|#let\\|#set\\)"
  "Regexp matching lines to strip as front matter when generating note snippets."
  :type 'regexp
  :group 'denote-grid)

(defcustom denote-grid-note-snippet-length 600
  "How many characters of a note's body to show on its card."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-card-aspect 0.8
  "Card height as a fraction of `denote-grid-thumbnail-size'."
  :type 'float
  :group 'denote-grid)

(defcustom denote-grid-card-radius 2
  "Corner radius of cards in pixels.  0 gives sharp are.na-like corners."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-gutter 12
  "Pixel gap between cards, both horizontally and vertically."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-max-columns 5
  "Maximum number of cards per row."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-image-fit 'cover
  "How image, video and PDF thumbnails fill their card.
`cover' crops to fill the card edge to edge (are.na style).
`contain' shows the whole picture with padding."
  :type '(choice (const cover) (const contain))
  :group 'denote-grid)

(defcustom denote-grid-ffprobe-executable "ffprobe"
  "ffprobe executable used to read video durations for badges. Optional."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-pdfinfo-executable "pdfinfo"
  "pdfinfo executable used to read PDF page for badges. Optional."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-note-font-size 14
  "Font size in pixels for text inside note cards.  Line height follows it."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-note-font-family "sans-serif"
  "Font family used for text inside note cards."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-thumbnail-oversample 1
  "Resolution multiplier for generated video/pdf thumbnail files."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-lazy-batch-size 6
  "How many offscreen thumbnails to generate per idle tick."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-ripgrep-executable "rg"
  "Path to ripgrep executable used for fast full-text search."
  :type '(choice (const :tag "Disable ripgrep" nil) string)
  :group 'denote-grid)

(defcustom denote-grid-ffmpeg-executable "ffmpeg"
  "ffmpeg executable used for video thumbnails. Optional."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-pdftoppm-executable "pdftoppm"
  "pdftoppm executable used for PDF thumbnails. Optional."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-image-extensions '("jpg" "jpeg" "png" "gif" "webp" "bmp" "svg")
  "File extensions treated as images."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-video-extensions '("mp4" "webm" "mov" "mkv")
  "File extensions treated as videos."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-pdf-extensions '("pdf")
  "File extensions treated as PDFs."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-text-extensions '("md" "org" "txt")
  "File extensions treated as note text."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-ignore-regexp "~\\'"
  "Regexp matching file names that denote-grid skips entirely.
Matched against the file name without its directory.  The default
skips Emacs backup files (`foo.org~' and numbered `foo.org.~1~'),
which would otherwise show up as duplicates of the real note."
  :type 'regexp :group 'denote-grid)

(defun denote-grid--ignored-file-p (file)
  "Non-nil if FILE's name matches `denote-grid-ignore-regexp'."
  (and denote-grid-ignore-regexp
       (string-match-p denote-grid-ignore-regexp (file-name-nondirectory file))))

(defface denote-grid-title-face
  '((t :inherit bold))
  "Face for the current card's title in the header line.")

(defface denote-grid-tag-face
  '((t :inherit shadow))
  "Face for tags in the header line.")

(defface denote-grid-selection-face
  '((((background dark)) :foreground "#8a9bff")
    (t :foreground "#3b4cc0"))
  "Face whose foreground is the border color of the selected card.")

(defface denote-grid-chip-face
  '((t :inherit denote-grid-tag-face :underline t))
  "Face for clickable tag chips in the header line.")

(defvar denote-grid--cache-dir nil)

(defvar-local denote-grid--selected-start nil
  "Buffer position of the card currently drawn as selected.")
(defvar-local denote-grid--link-counts nil
  "Hash table: item id -> number of denote links to/from that item.")
(defvar-local denote-grid--links-cache nil)
(defvar-local denote-grid--links-cache-key nil)

(defun denote-grid--cache-dir-for (root)
  (let ((dir (expand-file-name ".denote-grid-thumbs/" root)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

;;; Theme-safe colors
;;
;; SVG cannot parse Emacs-only color names (e.g. "gray50"), nor
;; "unspecified-fg"/"unspecified-bg", and falls back to black.  Every color
;; that goes into an SVG is therefore resolved to a #rrggbb string here.

(defun denote-grid--hex (color)
  "Return COLOR as a #rrggbb string, or nil if it cannot be resolved."
  (when (and (stringp color)
             (not (string-prefix-p "unspecified" color)))
    (when-let ((rgb (color-name-to-rgb color)))
      (apply #'color-rgb-to-hex (append rgb '(2))))))

(defun denote-grid--bg ()
  "Default background as #rrggbb."
  (or (denote-grid--hex (face-background 'default nil t))
      (denote-grid--hex (frame-parameter nil 'background-color))
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#000000" "#ffffff")))

(defun denote-grid--fg ()
  "Default foreground as #rrggbb."
  (or (denote-grid--hex (face-foreground 'default nil t))
      (denote-grid--hex (frame-parameter nil 'foreground-color))
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#ffffff" "#000000")))

(defun denote-grid--muted ()
  "Foreground blended toward background: always readable, always dimmer."
  (let ((fg (color-name-to-rgb (denote-grid--fg)))
        (bg (color-name-to-rgb (denote-grid--bg))))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (f b) (+ b (* 0.65 (- f b)))) fg bg)
                   '(2)))))

(defconst denote-grid--name-re
  "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)--\\([^_]+\\)\\(?:__\\(.+\\)\\)?\\'")

(defconst denote-grid--id-re "[0-9]\\{8\\}T[0-9]\\{6\\}")

(cl-defstruct denote-grid-item
  id title tags path type mtime snippet-fetched snippet count)

(defun denote-grid--file-type (ext)
  (cond
   ((member ext denote-grid-image-extensions) 'image)
   ((member ext denote-grid-video-extensions) 'video)
   ((member ext denote-grid-pdf-extensions) 'pdf)
   ((member ext denote-grid-text-extensions) 'text)
   (t 'other)))

(defun denote-grid--clean-line (line)
  "Turn an org/markdown LINE into plain display text.
Headings are prefixed with a ^A marker so the card can draw them bold."
  (let ((s line) (heading nil))
    (cond ((string-match "\\`\\*+[ \t]+" s)
           (setq heading t s (substring s (match-end 0))))
          ((string-match "\\`#+[ \t]+" s)
           (setq heading t s (substring s (match-end 0)))))
    (setq s (replace-regexp-in-string "\\[\\[denote:[^]]*\\]\\]" "" s))
    (setq s (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string "\\[\\[\\([^]]*\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string "\\[\\([^]]+\\)\\]([^)]*)" "\\1" s))
    (setq s (replace-regexp-in-string "\\`\\([ \t]*\\)[-+] \\[ \\]" "\\1☐" s))
    (setq s (replace-regexp-in-string "\\`\\([ \t]*\\)[-+] \\[[xX]\\]" "\\1☑" s))
    (setq s (replace-regexp-in-string "\\`\\([ \t]*\\)[-+] " "\\1• " s))
    (setq s (replace-regexp-in-string "\\*\\([^* \t\n][^*\n]*?\\)\\*" "\\1" s))
    (setq s (replace-regexp-in-string "=\\([^= \t\n][^=\n]*?\\)=" "\\1" s))
    (setq s (replace-regexp-in-string "~\\([^~ \t\n][^~\n]*?\\)~" "\\1" s))
    (if heading (concat (string 1) s) s)))

(defun denote-grid--snippet (path)
  "Return a cleaned plain-text excerpt of the note at PATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path nil 0 6000)
        (goto-char (point-min))
        ;; YAML / TOML front matter block
        (when (looking-at "\\(---\\|\\+\\+\\+\\)[ \t]*\n")
          (let ((delim (match-string 1)))
            (goto-char (match-end 0))
            (if (re-search-forward (concat "^" (regexp-quote delim) "[ \t]*$") nil t)
                (forward-line 1)
              (goto-char (point-min)))))
        (let (lines (in-drawer nil))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (cond
               (in-drawer
                (when (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
                  (setq in-drawer nil)))
               ((string-match-p "\\`[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):[ \t]*\\'" line)
                (setq in-drawer t))
               ((string-match-p denote-grid-front-matter-regexp line))
               (t (push (denote-grid--clean-line line) lines))))
            (forward-line 1))
          (let ((body (string-trim (mapconcat #'identity (nreverse lines) "\n"))))
            (substring body 0 (min (length body) denote-grid-note-snippet-length)))))
    (error "")))

(defun denote-grid--get-snippet (item)
  (unless (denote-grid-item-snippet-fetched item)
    (setf (denote-grid-item-snippet item)
          (if (eq (denote-grid-item-type item) 'text)
              (denote-grid--snippet (denote-grid-item-path item))
            ""))
    (setf (denote-grid-item-snippet-fetched item) t))
  (denote-grid-item-snippet item))

(defun denote-grid--parse-file (path)
  (let* ((name (file-name-nondirectory path))
         (ext (downcase (or (file-name-extension name) "")))
         (stem (file-name-sans-extension name)))
    (when (and (not (denote-grid--ignored-file-p path))
               (string-match denote-grid--name-re stem))
      (let* ((id (match-string 1 stem))
             (title (replace-regexp-in-string "-" " " (match-string 2 stem)))
             (tags (and (match-string 3 stem) (split-string (match-string 3 stem) "_" t)))
             (type (denote-grid--file-type ext))
             (mtime (float-time (file-attribute-modification-time (file-attributes path)))))
        (make-denote-grid-item
         :id id :title title :tags tags :path path :type type :mtime mtime
         :snippet-fetched nil :snippet "")))))

(defun denote-grid--collect-items (root)
  "Collect all denote items under ROOT.
ROOT can be a directory path string or a list of directory path strings."
  (let ((dirs (if (listp root) root (list root)))
        items)
    (dolist (dir dirs)
      (when (and dir (file-directory-p dir))
        (dolist (f (directory-files-recursively
                    dir ".*" nil
                    (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d))))))
          (unless (string-prefix-p "." (file-name-nondirectory f))
            (when-let ((item (denote-grid--parse-file f)))
              (push item items))))))
    (nreverse items)))

(defun denote-grid--dired-visible-files ()
  (unless (derived-mode-p 'dired-mode)
    (user-error "denote-grid: not in a dired buffer"))
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((f (dired-get-filename nil t)))
          (when (and f (not (file-directory-p f)))
            (push f files)))
        (forward-line 1)))
    (nreverse files)))

(defun denote-grid--collect-items-from-dired ()
  (delq nil (mapcar #'denote-grid--parse-file (denote-grid--dired-visible-files))))

(defun denote-grid--file-contains-p (path query)
  (let ((rg (and denote-grid-ripgrep-executable
                 (executable-find denote-grid-ripgrep-executable))))
    (if rg
        (zerop (call-process rg nil nil nil "-q" "-i" "-F" query path))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (let ((case-fold-search t))
              (search-forward query nil t)))
        (error nil)))))

(defun denote-grid--links (items)
  (let ((ids (make-hash-table :test 'equal))
        (links (make-hash-table :test 'equal)))
    (dolist (it items) (puthash (denote-grid-item-id it) t ids))
    (dolist (it items)
      (when (eq (denote-grid-item-type it) 'text)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents (denote-grid-item-path it))
              (goto-char (point-min))
              (let (found)
                (while (re-search-forward denote-grid--id-re nil t)
                  (let ((found-id (match-string 0)))
                    (unless (or (equal found-id (denote-grid-item-id it))
                                (member found-id found)
                                (not (gethash found-id ids)))
                      (push found-id found)
                      (cl-pushnew found-id (gethash (denote-grid-item-id it) links))
                      (cl-pushnew (denote-grid-item-id it) (gethash found-id links)))))))
          (error nil))))
    links))

(defun denote-grid--clusters (items)
  (let ((links (denote-grid--links items))
        (visited (make-hash-table :test 'equal))
        (components nil))
    (dolist (it items)
      (let ((id (denote-grid-item-id it)))
        (unless (gethash id visited)
          (let ((queue (list id))
                (component nil))
            (while queue
              (let ((cur (pop queue)))
                (unless (gethash cur visited)
                  (puthash cur t visited)
                  (push cur component)
                  (dolist (nb (gethash cur links)) (push nb queue)))))
            (when component
              (push component components))))))
    (setq components (sort components (lambda (a b) (> (length a) (length b)))))
    (let ((cluster-of (make-hash-table :test 'equal))
          (n 0))
      (dolist (comp components)
        (setq n (1+ n))
        (dolist (id comp)
          (puthash id n cluster-of)))
      cluster-of)))

(defun denote-grid--links-cached (items)
  "Link table for ITEMS, recomputed only when the set of items changes."
  (let ((key (mapcar #'denote-grid-item-id items)))
    (unless (equal key denote-grid--links-cache-key)
      (setq denote-grid--links-cache (denote-grid--links items)
            denote-grid--links-cache-key key))
    denote-grid--links-cache))

(defun denote-grid--build-link-counts ()
  (let ((h (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (puthash k (length v) h))
             (denote-grid--links-cached denote-grid--items))
    h))

(defun denote-grid--link-count (item)
  (if (hash-table-p denote-grid--link-counts)
      (gethash (denote-grid-item-id item) denote-grid--link-counts 0)
    0))

;;; Channels (folders)

(defvar-local denote-grid--roots nil
  "Slash-terminated, expanded top-level directories of this grid.")
(defvar-local denote-grid--dirs nil
  "Directories being shown: all roots, or a single folder inside one.")

(defun denote-grid--norm-dir (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun denote-grid--deep-p ()
  "Non-nil when the grid shows a whole subtree (flat) instead of one folder."
  (or (not (string-empty-p denote-grid--filter))
      denote-grid--cluster-p denote-grid--orphan-p))

(defun denote-grid--scope-items (deep)
  "Items under the current directories; only direct children unless DEEP."
  (cl-remove-if-not
   (lambda (it)
     (let* ((p (expand-file-name (denote-grid-item-path it)))
            (d (cl-find-if (lambda (d) (string-prefix-p d p)) denote-grid--dirs)))
       (and d (or deep (not (string-match-p "/" (substring p (length d))))))))
   denote-grid--items))

(defun denote-grid--channels ()
  "Channel cards for the sub-folders of the current directories."
  (let ((table (make-hash-table :test 'equal)) channels)
    (dolist (it denote-grid--items)
      (let* ((p (expand-file-name (denote-grid-item-path it)))
             (d (cl-find-if (lambda (d) (string-prefix-p d p)) denote-grid--dirs))
             (slash (and d (string-match "/" (substring p (length d))))))
        (when slash
          (let* ((key (concat d (substring p (length d) (+ (length d) slash)) "/"))
                 (cur (gethash key table)))
            (puthash key (cons (1+ (or (car cur) 0))
                               (max (or (cdr cur) 0) (denote-grid-item-mtime it)))
                     table)))))
    (maphash (lambda (key v)
               (push (make-denote-grid-item
                      :id key :path key :type 'channel :tags nil
                      :title (file-name-nondirectory (directory-file-name key))
                      :count (car v) :mtime (cdr v) :snippet-fetched t :snippet "")
                     channels))
             table)
    (sort channels (lambda (a b) (string-collate-lessp (denote-grid-item-title a)
                                                       (denote-grid-item-title b)
                                                       nil t)))))

(defun denote-grid--ago (time)
  (let ((d (max 0 (- (float-time) time))))
    (or (cl-loop for (secs . name) in '((31536000 . "year") (2592000 . "month")
                                         (86400 . "day") (3600 . "hour") (60 . "minute"))
                 when (>= d secs)
                 return (let ((n (floor d secs)))
                          (format "%d %s%s ago" n name (if (= n 1) "" "s"))))
        "just now")))

(defun denote-grid--channel-svg (item)
  "A folder card: bordered square with its name, block count and age."
  (let* ((w denote-grid-thumbnail-size) (h (denote-grid--card-height))
         (bg (denote-grid--bg)) (fg (denote-grid--fg)) (muted (denote-grid--muted))
         (border (denote-grid--mix bg fg 0.45))
         (n (or (denote-grid-item-count item) 0))
         (max-chars (floor (- w 40) 9.4))
         (lines (mapcar (lambda (l) (truncate-string-to-width l max-chars nil nil "…"))
                        (seq-take (denote-grid--wrap-text (denote-grid-item-title item) max-chars) 3)))
         (y (+ (- (/ h 2) 14) (* -11 (1- (length lines)))))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx denote-grid-card-radius)
    (svg-rectangle svg 1 1 (- w 2) (- h 2) :fill "none" :stroke-color border
                   :stroke-width 2 :rx denote-grid-card-radius)
    (dolist (line lines)
      (svg-text svg line :x (/ w 2) :y y :fill fg :font-size 17 :text-anchor "middle"
                :font-family denote-grid-note-font-family)
      (setq y (+ y 22)))
    (svg-text svg (format "%d block%s" n (if (= n 1) "" "s")) :x (/ w 2) :y (+ y 8)
              :fill fg :font-size 11 :text-anchor "middle"
              :font-family denote-grid-note-font-family)
    (svg-text svg (denote-grid--ago (denote-grid-item-mtime item)) :x (/ w 2) :y (+ y 25)
              :fill muted :font-size 11 :text-anchor "middle"
              :font-family denote-grid-note-font-family)
    (svg-image svg :ascent 'center)))

;;; Card drawing

(defun denote-grid--card-height ()
  (round (* denote-grid-thumbnail-size denote-grid-card-aspect)))

(defun denote-grid--mix (from to amount)
  "Blend color FROM toward TO by AMOUNT (0..1), as #rrggbb."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (a b) (+ a (* amount (- b a))))
                            (color-name-to-rgb from) (color-name-to-rgb to))
                 '(2))))

(defun denote-grid--wrap-text (str width)
  "Wrap STR to WIDTH chars, keeping paragraph breaks as empty strings.
Lines of heading paragraphs carry the text property `denote-grid-bold'."
  (let (lines)
    (dolist (raw (split-string str "\n"))
      (let* ((bold (string-prefix-p (string 1) raw))
             (para (if bold (substring raw 1) raw)))
        (if (string-blank-p para)
            (unless (or (null lines) (equal (car lines) ""))
              (push "" lines))
          (let ((cur "")
                (emit (lambda (l)
                        (push (if bold (propertize l 'denote-grid-bold t) l) lines))))
            (dolist (w (split-string para))
              (cond ((string-empty-p cur) (setq cur w))
                    ((<= (+ (length cur) 1 (length w)) width)
                     (setq cur (concat cur " " w)))
                    (t (funcall emit cur) (setq cur w))))
            (unless (string-empty-p cur) (funcall emit cur))))))
    (nreverse lines)))

(defun denote-grid--note-svg (item)
  "Are.na style text block: bordered box, body first, title as caption."
  (let* ((w denote-grid-thumbnail-size) (h (denote-grid--card-height))
         (bg (denote-grid--bg))
         (fg (denote-grid--fg))
         (muted (denote-grid--muted))
         (surface (denote-grid--mix bg fg 0.05))
         (border (denote-grid--mix bg fg 0.28))
         (font denote-grid-note-font-family)
         (pad 14) (fs denote-grid-note-font-size) (lh (round (* fs 1.42))) (footer 30)
         (chars (floor (- w (* 2 pad)) (* fs 0.56)))
         (body (denote-grid--get-snippet item))
         (empty (string-empty-p (string-trim body)))
         (title (denote-grid-item-title item))
         (nl (denote-grid--link-count item))
         (badge (and (> nl 0) (format "%d link%s" nl (if (= nl 1) "" "s"))))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill surface :rx denote-grid-card-radius)
    (svg-rectangle svg 0.5 0.5 (1- w) (1- h) :fill "none" :stroke-color border
                   :stroke-width 1 :rx denote-grid-card-radius)
    (if empty
        (let ((y (+ pad 8)))
          (dolist (line (seq-take (denote-grid--wrap-text title (floor (- w (* 2 pad)) 8.4)) 4))
            (svg-text svg line :x pad :y (+ y 6) :fill fg :font-size 15
                      :font-weight "bold" :font-family font)
            (setq y (+ y 22))))
      (let* ((max-lines (floor (- h footer pad) lh))
             (all (denote-grid--wrap-text body chars))
             (shown (seq-take all max-lines))
             (cut (> (length all) max-lines))
             (n (length shown))
             (y (+ pad fs -2))
             (i 0))
        (dolist (line shown)
          (if (string-empty-p line)
              (setq y (- y (/ lh 2)))
            (let ((bold (get-text-property 0 'denote-grid-bold line))
                  (txt (if (and cut (= i (1- n)))
                           (concat (string-trim-right line) "…")
                         line))
                  (op (cond ((and cut (= i (1- n))) "0.3")
                            ((and cut (= i (- n 2))) "0.65")
                            (t "1"))))
              (apply #'svg-text svg txt :x pad :y y :fill fg :fill-opacity op
                     :font-size fs :font-family font
                     (and bold '(:font-weight "bold")))))
          (setq y (+ y lh) i (1+ i)))))
    ;; footer: hairline, title caption, link badge
    (svg-rectangle svg 0 (- h footer) w 1 :fill border)
    (let ((tx pad) (base (+ (- h (/ footer 2)) 4)))
      (when badge
        (svg-text svg badge :x (- w pad) :y base :fill muted :font-size 10
                  :text-anchor "end" :font-family font))
      (svg-text svg (truncate-string-to-width
                     title
                     (floor (- w tx pad (if badge (+ 8 (* (length badge) 5.8)) 0))
                            (* 10.5 0.56))
                     nil nil "…")
                :x tx :y base :fill muted :font-size 10.5 :font-family font))
    (svg-image svg :ascent 'center)))

(defun denote-grid--placeholder-svg (item label)
  (let* ((w denote-grid-thumbnail-size) (h (denote-grid--card-height))
         (bg (denote-grid--bg))
         (fg (denote-grid--fg))
         (accent (denote-grid--muted))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx denote-grid-card-radius)
    (svg-rectangle svg 0 0 w h :fill accent :fill-opacity "0.12" :rx denote-grid-card-radius)
    (svg-text svg label :x (/ w 2) :y (/ h 2) :fill accent :font-size 22
              :font-weight "bold" :font-family "sans-serif" :text-anchor "middle")
    (svg-text svg (truncate-string-to-width (denote-grid-item-title item) 26 nil nil "…")
              :x 14 :y (- h 14) :fill fg :font-size 11 :font-family "sans-serif")
    (svg-image svg :ascent 'center)))

(defun denote-grid--cache-file (item ext)
  (expand-file-name (format "%s-%d.%s" (denote-grid-item-id item)
                            (round (denote-grid-item-mtime item)) ext)
                    denote-grid--cache-dir))

(defun denote-grid--mime-for (file)
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "jpg" "jpeg") "image/jpeg")
    ("png" "image/png")
    ("gif" "image/gif")
    ("webp" "image/webp")
    ("svg" "image/svg+xml")
    ("bmp" "image/bmp")
    (_ "image/png")))

(defun denote-grid--cached-probe (item ext fn)
  "Return the string cached for ITEM under EXT, computing it with FN once."
  (let ((f (denote-grid--cache-file item ext)))
    (if (file-exists-p f)
        (let ((v (string-trim (with-temp-buffer (insert-file-contents f) (buffer-string)))))
          (and (not (string-empty-p v)) v))
      (let ((v (ignore-errors (funcall fn))))
        (ignore-errors (with-temp-file f (insert (or v ""))))
        v))))

(defun denote-grid--format-duration (secs)
  (let* ((s (round secs)) (h (/ s 3600)) (m (/ (mod s 3600) 60)) (r (mod s 60)))
    (if (> h 0) (format "%d:%02d:%02d" h m r) (format "%d:%02d" m r))))

(defun denote-grid--video-duration (item)
  (when (executable-find denote-grid-ffprobe-executable)
    (denote-grid--cached-probe
     item "dur"
     (lambda ()
       (with-temp-buffer
         (when (zerop (call-process denote-grid-ffprobe-executable nil t nil
                                    "-v" "error" "-show_entries" "format=duration"
                                    "-of" "csv=p=0" (denote-grid-item-path item)))
           (let ((d (string-to-number (buffer-string))))
             (and (> d 0) (denote-grid--format-duration d)))))))))

(defun denote-grid--pdf-pages (item)
  (when (executable-find denote-grid-pdfinfo-executable)
    (denote-grid--cached-probe
     item "pages"
     (lambda ()
       (with-temp-buffer
         (call-process denote-grid-pdfinfo-executable nil t nil (denote-grid-item-path item))
         (goto-char (point-min))
         (when (re-search-forward "^Pages:[ \t]+\\([0-9]+\\)" nil t)
           (format "%s p" (match-string 1))))))))

(defun denote-grid--draw-badge (svg w text play)
  "Draw a small dark pill with TEXT (and a play triangle if PLAY) at the top right."
  (let* ((text (or text ""))
         (tw (+ 12 (* (length text) 6.4) (if play 10 0)))
         (x (- w tw 8)) (y 8))
    (svg-rectangle svg x y tw 18 :fill "#000000" :fill-opacity "0.65" :rx 3)
    (when play
      (svg-polygon svg (list (cons (+ x 7) (+ y 5)) (cons (+ x 7) (+ y 13))
                             (cons (+ x 14) (+ y 9)))
                   :fill "#ffffff"))
    (unless (string-empty-p text)
      (svg-text svg text :x (+ x (if play 18 6)) :y (+ y 13) :fill "#ffffff"
                :font-size 10 :font-family "sans-serif"))))

(defun denote-grid--boxed-raster (item raw-file label &optional badge-text play)
  (if (null raw-file)
      (denote-grid--placeholder-svg item label)
    (let* ((w denote-grid-thumbnail-size) (h (denote-grid--card-height))
           (dim (ignore-errors (image-size (create-image raw-file nil nil) t))))
      (if (not dim)
          (denote-grid--placeholder-svg item label)
        (let* ((iw (car dim)) (ih (cdr dim))
               (cover (eq denote-grid-image-fit 'cover))
               (pad (if cover 0 6))
               (scale (min (/ (float (- w (* 2 pad))) iw) (/ (float (- h (* 2 pad))) ih)))
               (dw (max 1 (round (* iw scale))))
               (dh (max 1 (round (* ih scale))))
               (x (round (/ (- w dw) 2.0)))
               (y (round (/ (- h dh) 2.0)))
               (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink"))
               (bg (denote-grid--bg)))
          (svg-rectangle svg 0 0 w h :fill bg :rx denote-grid-card-radius)
          (condition-case nil
              (progn
                (if cover
                    (svg-embed svg raw-file (denote-grid--mime-for raw-file) nil
                               :x 0 :y 0 :width w :height h
                               :preserveAspectRatio "xMidYMid slice")
                  (svg-embed svg raw-file (denote-grid--mime-for raw-file) nil
                             :x x :y y :width dw :height dh))
                (when (or badge-text play)
                  (denote-grid--draw-badge svg w badge-text play))
                (svg-image svg :ascent 'center))
            (error (denote-grid--placeholder-svg item label))))))))

(defvar-local denote-grid--image-cache nil)

(defun denote-grid--video-thumb (item)
  (let ((out nil))
    (when (executable-find denote-grid-ffmpeg-executable)
      (setq out (denote-grid--cache-file item "jpg"))
      (unless (file-exists-p out)
        (call-process denote-grid-ffmpeg-executable nil nil nil
                      "-y" "-ss" "1" "-i" (denote-grid-item-path item)
                      "-frames:v" "1"
                      "-vf" (format "scale=%d:-1" (* denote-grid-thumbnail-oversample
                                                    denote-grid-thumbnail-size))
                      "-loglevel" "quiet" out))
      (unless (file-exists-p out) (setq out nil)))
    (denote-grid--boxed-raster item out "VIDEO"
                               (and out (denote-grid--video-duration item)) t)))

(defun denote-grid--pdf-thumb (item)
  (let ((out nil))
    (when (executable-find denote-grid-pdftoppm-executable)
      (let* ((base (denote-grid--cache-file item "pdfpage"))
             (png (concat base ".png")))
        (unless (file-exists-p png)
          (call-process denote-grid-pdftoppm-executable nil nil nil
                        "-png" "-f" "1" "-singlefile"
                        "-scale-to" (number-to-string (* denote-grid-thumbnail-oversample
                                                         denote-grid-thumbnail-size))
                        (denote-grid-item-path item) base))
        (when (file-exists-p png) (setq out png))))
    (denote-grid--boxed-raster item out "PDF"
                               (and out (denote-grid--pdf-pages item)))))

(defun denote-grid--image-thumb (item)
  (let ((out (denote-grid-item-path item)))
    (if (executable-find denote-grid-ffmpeg-executable)
        (let ((cached (denote-grid--cache-file item "jpg")))
          (unless (file-exists-p cached)
            ;; Force yuv420p output so high-res PNG/WEBP/HEIC convert cleanly to JPG
            (call-process denote-grid-ffmpeg-executable nil nil nil
                          "-y" "-i" (expand-file-name (denote-grid-item-path item))
                          "-vf" (format "scale=%d:-1,format=yuv420p"
                                        (* denote-grid-thumbnail-oversample
                                           denote-grid-thumbnail-size))
                          "-loglevel" "error" cached))
          (when (and (file-exists-p cached)
                     (> (file-attribute-size (file-attributes cached)) 0))
            (setq out cached)))
      ;; Fallback warning if ffmpeg is missing
      (message "denote-grid: %s not found; using original image source"
               denote-grid-ffmpeg-executable))
    (denote-grid--boxed-raster item out "IMG")))

(defun denote-grid--get-image (item)
  (unless denote-grid--image-cache
    (setq denote-grid--image-cache (make-hash-table :test 'equal)))
  (let* ((key (denote-grid--cache-key item))
         (cached (gethash key denote-grid--image-cache)))
    (or cached
        (puthash key
                 (pcase (denote-grid-item-type item)
                   ('image (denote-grid--image-thumb item))
                   ('video (denote-grid--video-thumb item))
                   ('pdf (denote-grid--pdf-thumb item))
                   ('text (denote-grid--note-svg item))
                   ('channel (denote-grid--channel-svg item))
                   (_ (let ((ext-label (upcase (or (file-name-extension (denote-grid-item-path item)) "FILE"))))
                        (denote-grid--placeholder-svg item ext-label))))
                 denote-grid--image-cache))))

(defun denote-grid--prune-image-cache (items)
  (when (hash-table-p denote-grid--image-cache)
    (let ((live-ids (make-hash-table :test 'equal)))
      (dolist (it items) (puthash (denote-grid-item-id it) t live-ids))
      (let (stale)
        (maphash (lambda (key _val)
                   (unless (gethash (car key) live-ids)
                     (push key stale)))
                 denote-grid--image-cache)
        (dolist (key stale) (remhash key denote-grid--image-cache))))))

(defvar-local denote-grid--items nil)
(defvar-local denote-grid--filter "")
(defvar-local denote-grid--sort-key 'date)
(defvar-local denote-grid--sort-desc t)
(defvar-local denote-grid--cluster-p nil)
(defvar-local denote-grid--orphan-p nil)
(defvar-local denote-grid--current-item nil)
(defvar-local denote-grid--card-starts nil)
(defvar-local denote-grid--source-directory nil)
(defvar-local denote-grid--source-dired-buffer nil)
(defvar-local denote-grid--last-win-width nil)
(defvar-local denote-grid--clusters-cache nil)
(defvar-local denote-grid--clusters-cache-key nil)
(defvar-local denote-grid--pending-fill nil)
(defvar-local denote-grid--fill-timer nil)

(defvar denote-grid-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'denote-grid-open-at-point)
    (define-key m [mouse-1] #'denote-grid-open-at-point)
    (define-key m (kbd "^") #'denote-grid-up)
    (define-key m (kbd "u") #'denote-grid-up)
    (define-key m (kbd "d") #'denote-grid-jump-to-dired)
    (define-key m (kbd "/") #'denote-grid-filter)
    (define-key m (kbd "s") #'denote-grid-sort-cycle)
    (define-key m (kbd "r") #'denote-grid-sort-reverse)
    (define-key m (kbd "c") #'denote-grid-toggle-cluster)
    (define-key m (kbd "t") #'denote-grid-filter-tag)
    (define-key m (kbd "o") #'denote-grid-toggle-orphan)
    (define-key m (kbd "g") #'denote-grid-refresh)
    (define-key m (kbd "q") #'quit-window)
    (define-key m (kbd "<right>") #'denote-grid-next-card)
    (define-key m (kbd "TAB") #'denote-grid-next-card)
    (define-key m (kbd "n") #'denote-grid-next-card)
    (define-key m (kbd "<left>") #'denote-grid-prev-card)
    (define-key m (kbd "<backtab>") #'denote-grid-prev-card)
    (define-key m (kbd "p") #'denote-grid-prev-card)
    (define-key m (kbd "<down>") #'denote-grid-down-card)
    (define-key m (kbd "<up>") #'denote-grid-up-card)
    m))

(define-derived-mode denote-grid-mode special-mode "Denote-Grid"
  "Major mode for browsing denote files as an image-dired style grid."
  (setq truncate-lines t)
  (setq-local line-spacing denote-grid-gutter)
  (setq header-line-format '(:eval (denote-grid--header-line)))
  (add-hook 'post-command-hook #'denote-grid--update-point-info nil t)
  (add-hook 'window-size-change-functions #'denote-grid--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'denote-grid--render nil t)
  (add-hook 'kill-buffer-hook #'denote-grid--cleanup nil t))

(defun denote-grid--cleanup ()
  (when (timerp denote-grid--fill-timer)
    (cancel-timer denote-grid--fill-timer))
  (when (hash-table-p denote-grid--image-cache)
    (clrhash denote-grid--image-cache))
  (setq denote-grid--image-cache nil
        denote-grid--clusters-cache nil
        denote-grid--pending-fill nil
        denote-grid--fill-timer nil))

(defvar denote-grid--header-map
  (let ((m (make-sparse-keymap)))
    (define-key m [header-line mouse-1] #'denote-grid-header-click)
    m))

(defun denote-grid--chip (label filter help)
  "Clickable header-line LABEL that sets the grid filter to FILTER."
  (propertize (replace-regexp-in-string "%" "%%" label)
              'face 'denote-grid-chip-face
              'mouse-face 'highlight
              'help-echo help
              'denote-grid-filter filter
              'local-map denote-grid--header-map))

(defun denote-grid-header-click (event)
  "Act on the header-line chip or breadcrumb that was clicked."
  (interactive "e")
  (let* ((start (event-start event))
         (ps (posn-string start))
         (filter (and ps (get-text-property (cdr ps) 'denote-grid-filter (car ps))))
         (dir (and ps (get-text-property (cdr ps) 'denote-grid-dir (car ps)))))
    (with-current-buffer (window-buffer (posn-window start))
      (cond (dir (denote-grid--enter (list dir) (denote-grid--child-toward dir)))
            (filter (setq denote-grid--filter filter)
                    (denote-grid--render))))))

(defun denote-grid--crumbs ()
  "Breadcrumb for the current folder; ancestors are clickable."
  (let* ((cur (car denote-grid--dirs))
         (root (and cur (null (cdr denote-grid--dirs))
                    (cl-find-if (lambda (r) (string-prefix-p r cur)) denote-grid--roots))))
    (if (not root)
        (propertize "denote" 'face 'denote-grid-title-face)
      (let ((acc root)
            (crumbs (list (cons (file-name-nondirectory (directory-file-name root)) root))))
        (dolist (r (split-string (substring cur (length root)) "/" t))
          (setq acc (concat acc r "/"))
          (push (cons r acc) crumbs))
        (setq crumbs (nreverse crumbs))
        (let ((tail (car (last crumbs))))
          (mapconcat
           (lambda (c)
             (let ((label (replace-regexp-in-string "%" "%%" (car c))))
               (if (eq c tail)
                   (propertize label 'face 'denote-grid-title-face)
                 (propertize label 'face 'denote-grid-chip-face 'mouse-face 'highlight
                             'help-echo (format "Go to %s" (car c))
                             'denote-grid-dir (cdr c)
                             'local-map denote-grid--header-map))))
           crumbs (propertize " / " 'face 'shadow)))))))

(defun denote-grid--child-toward (dir)
  "The folder directly below DIR on the way to the current one."
  (let ((cur (car denote-grid--dirs)))
    (when (and cur (string-prefix-p dir cur) (not (equal dir cur)))
      (concat dir (car (split-string (substring cur (length dir)) "/" t)) "/"))))

(defun denote-grid--header-line ()
  (let ((clear (if (string-empty-p denote-grid--filter)
                   ""
                 (concat "  " (denote-grid--chip "✕ clear filter" "" "Clear the filter"))))
        (it denote-grid--current-item))
    (concat
     " " (denote-grid--crumbs) "   "
     (cond
      ((and it (eq (denote-grid-item-type it) 'channel))
       (propertize (format "%d blocks" (or (denote-grid-item-count it) 0)) 'face 'shadow))
      (it
       (concat
        (propertize (replace-regexp-in-string "%" "%%" (denote-grid-item-title it))
                    'face 'denote-grid-title-face)
        "  " (propertize (substring (denote-grid-item-id it) 0 8) 'face 'shadow) "  "
        (mapconcat (lambda (tg)
                     (denote-grid--chip (concat "#" tg) (concat "#" tg)
                                        (format "Show only #%s" tg)))
                   (denote-grid-item-tags it) " ")))
      (t
       (format "%d items  sort:%s%s%s%s"
               (length denote-grid--items) denote-grid--sort-key
               (if denote-grid--sort-desc "↓" "↑")
               (if denote-grid--cluster-p "  [clustered]" "")
               (if denote-grid--orphan-p "  [orphans]" ""))))
     clear)))

(defun denote-grid--selection-color ()
  (or (denote-grid--hex (face-attribute 'denote-grid-selection-face :foreground nil t))
      (denote-grid--fg)))

(defun denote-grid--select-decorate (img item)
  "Return a copy of IMG with a selection border (and a title caption for media)."
  (let ((data (and (eq (car-safe img) 'image) (plist-get (cdr img) :data))))
    (if (not (stringp data))
        img
      (let* ((w denote-grid-thumbnail-size) (h (denote-grid--card-height))
             (col (denote-grid--selection-color))
             (caption
              (unless (memq (denote-grid-item-type item) '(text channel))
                (format "<rect x=\"0\" y=\"%d\" width=\"%d\" height=\"26\" fill=\"%s\" fill-opacity=\"0.85\"/><text x=\"10\" y=\"%d\" font-family=\"sans-serif\" font-size=\"11\" fill=\"%s\">%s</text>"
                        (- h 26) w (denote-grid--bg) (- h 9) (denote-grid--fg)
                        (xml-escape-string
                         (truncate-string-to-width (denote-grid-item-title item)
                                                   (floor (- w 20) 6.2) nil nil "…")))))
             (rect (format "<rect x=\"1.5\" y=\"1.5\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"%s\" stroke-width=\"3\" rx=\"%d\"/>"
                           (- w 3) (- h 3) col denote-grid-card-radius))
             (pos (string-match "</svg>[ \t\n]*\\'" data)))
        (if (not pos)
            img
          (let ((new (copy-sequence img)))
            (setcdr new (plist-put (copy-sequence (cdr img)) :data
                                   (concat (substring data 0 pos) (or caption "") rect
                                           (substring data pos))))
            new))))))

(defun denote-grid--cached-image (item)
  (and (hash-table-p denote-grid--image-cache)
       (gethash (denote-grid--cache-key item) denote-grid--image-cache)))

(defun denote-grid--display-image (item)
  "Image to show for ITEM now: cached, a placeholder if deferred, or freshly made."
  (or (denote-grid--cached-image item)
      (if (denote-grid--raster-p item)
          (denote-grid--placeholder-svg
           item (pcase (denote-grid-item-type item)
                  ('image "IMG") ('video "VIDEO") ('pdf "PDF") (_ "FILE")))
        (denote-grid--get-image item))))

(defun denote-grid--refresh-card (start selected)
  "Redraw the card at START, with the selection border if SELECTED."
  (when-let ((item (get-text-property start 'denote-grid-item)))
    (let* ((img (denote-grid--display-image item))
           (img (if selected (denote-grid--select-decorate img item) img)))
      ;; `insert-image' puts the title text under the image, so a card spans
      ;; several characters: recolor the whole run, not just the first one.
      (with-silent-modifications
        (put-text-property
         start (next-single-property-change start 'denote-grid-item nil (point-max))
         'display img)))))

(defun denote-grid--update-point-info ()
  (setq denote-grid--current-item (get-text-property (point) 'denote-grid-item))
  (when (and (derived-mode-p 'denote-grid-mode)
             denote-grid--card-starts
             (> (length denote-grid--card-starts) 0))
    (let* ((idx (denote-grid--card-index-at (point)))
           (start (and (>= idx 0) (aref denote-grid--card-starts idx))))
      (unless (eql start denote-grid--selected-start)
        (when denote-grid--selected-start
          (denote-grid--refresh-card denote-grid--selected-start nil))
        (setq denote-grid--selected-start start)
        (when start (denote-grid--refresh-card start t)))))
  (force-mode-line-update))

(defun denote-grid--cards-per-row ()
  "How many cards (plus gutters) fit on a line."
  (if-let* ((win (get-buffer-window (current-buffer)))
            (win-width (window-body-width win t)))
      (min denote-grid-max-columns
           (max 1 (floor (- win-width (frame-char-width))
                         (+ denote-grid-thumbnail-size denote-grid-gutter))))
    1))

(defun denote-grid--gap ()
  "A horizontal gutter of `denote-grid-gutter' pixels."
  (propertize " " 'display `(space :width (,denote-grid-gutter))))

(defun denote-grid--on-window-size-change (win)
  (when (and (eq (window-buffer win) (current-buffer))
             (derived-mode-p 'denote-grid-mode))
    (let ((w (window-pixel-width win)))
      (unless (equal w denote-grid--last-win-width)
        (setq denote-grid--last-win-width w)
        (denote-grid--render)))))

(defun denote-grid--matches-p (item filter)
  (if (string-empty-p filter)
      t
    (if (string-prefix-p "#" filter)
        (let ((wanted (split-string (downcase (substring filter 1)) "[, ]+" t))
              (tags (mapcar #'downcase (denote-grid-item-tags item))))
          (and wanted (cl-every (lambda (tg) (member tg tags)) wanted)))
      (let ((hay (downcase (concat (denote-grid-item-title item) " "
                                   (mapconcat #'identity (denote-grid-item-tags item) " ")))))
        (or (string-match-p (regexp-quote (downcase filter)) hay)
            (and (eq (denote-grid-item-type item) 'text)
                 (denote-grid--file-contains-p (denote-grid-item-path item) filter)))))))

(defun denote-grid--sort-value (item key)
  (pcase key
    ('date (denote-grid-item-id item))
    ('title (denote-grid-item-title item))
    ('tags (or (car (denote-grid-item-tags item)) ""))
    ('type (symbol-name (denote-grid-item-type item)))))

(defun denote-grid--clusters-cached (items)
  (let ((key (mapcar (lambda (it) (denote-grid-item-id it)) items)))
    (unless (equal key denote-grid--clusters-cache-key)
      (setq denote-grid--clusters-cache (denote-grid--clusters items)
            denote-grid--clusters-cache-key key))
    denote-grid--clusters-cache))

(defun denote-grid--visible-items ()
  (let* ((filtered (cl-remove-if-not (lambda (it) (denote-grid--matches-p it denote-grid--filter))
                                     (denote-grid--scope-items (denote-grid--deep-p))))
         (sorted (sort (copy-sequence filtered)
                        (lambda (a b)
                          (let ((va (denote-grid--sort-value a denote-grid--sort-key))
                                (vb (denote-grid--sort-value b denote-grid--sort-key)))
                            (if denote-grid--sort-desc (string> va vb) (string< va vb))))))
         (links (denote-grid--links-cached denote-grid--items)))
    (cond
     (denote-grid--cluster-p
      (let* ((clusters (denote-grid--clusters-cached denote-grid--items))
             (connected (cl-remove-if-not
                         (lambda (it) (> (length (gethash (denote-grid-item-id it) links)) 0))
                         sorted)))
        (sort (copy-sequence connected)
              (lambda (a b)
                (let ((ca (gethash (denote-grid-item-id a) clusters))
                      (cb (gethash (denote-grid-item-id b) clusters)))
                  (if (= ca cb)
                      (string> (denote-grid-item-id a) (denote-grid-item-id b))
                    (< ca cb)))))))

     (denote-grid--orphan-p
      (cl-remove-if (lambda (it) (> (length (gethash (denote-grid-item-id it) links)) 0))
                    sorted))

     (t sorted))))

(defun denote-grid--raster-p (item)
  (memq (denote-grid-item-type item) '(image video pdf)))

(defun denote-grid--cache-key (item)
  (list (denote-grid-item-id item) (denote-grid-item-mtime item)
        (denote-grid-item-type item) denote-grid-thumbnail-size
        (denote-grid--bg) (denote-grid--fg)
        denote-grid-card-aspect denote-grid-card-radius denote-grid-image-fit
        denote-grid-note-font-size
        (denote-grid-item-count item)
        (and (eq (denote-grid-item-type item) 'text) (denote-grid--link-count item))))

(defun denote-grid--render ()
  "Render channels then items with hard line breaks and pixel gutters."
  (when (timerp denote-grid--fill-timer)
    (cancel-timer denote-grid--fill-timer)
    (setq denote-grid--fill-timer nil))
  (setq denote-grid--selected-start nil)
  (setq denote-grid--link-counts (denote-grid--build-link-counts))
  (let ((inhibit-read-only t)
        (pos (point))
        (clusters (and denote-grid--cluster-p (denote-grid--clusters-cached denote-grid--items)))
        (starts nil)
        (pending nil)
        (cols (denote-grid--cards-per-row))
        (count 0))
    (setq-local truncate-lines t)
    (erase-buffer)
    (let ((cards (append (unless (denote-grid--deep-p) (denote-grid--channels))
                         (denote-grid--visible-items)))
          (last-cluster nil))
      (if (null cards)
          (insert (propertize "\n  (nothing here)\n" 'face 'shadow))
        (dolist (it cards)
          (when (and denote-grid--cluster-p clusters)
            (let ((c (gethash (denote-grid-item-id it) clusters)))
              (unless (eq c last-cluster)
                (unless (null last-cluster) (insert "\n"))
                (insert (propertize (format "·· cluster %d ··\n" c) 'face 'shadow))
                (setq last-cluster c)
                (setq count 0))))
          (when (= (mod count cols) 0)
            (insert (denote-grid--gap)))
          (let* ((deferred (and (denote-grid--raster-p it)
                                (not (denote-grid--cached-image it))))
                 (img (denote-grid--display-image it))
                 (start (point)))
            (push start starts)
            (insert-image img (denote-grid-item-title it))
            (when deferred
              (push (list start (point) it) pending))
            (put-text-property start (point) 'denote-grid-item it)
            (put-text-property start (point) 'help-echo
                               (format "%s\n%s" (denote-grid-item-title it)
                                       (mapconcat (lambda (tg) (concat "#" tg))
                                                  (denote-grid-item-tags it) " ")))
            (setq count (1+ count))
            (if (= (mod count cols) 0)
                (insert "\n")
              (insert (denote-grid--gap)))))))
    (setq denote-grid--card-starts (vconcat (nreverse starts)))
    (setq denote-grid--pending-fill (nreverse pending))
    (goto-char (min pos (point-max))))
  (when denote-grid--pending-fill
    (denote-grid--fill-visible)
    (denote-grid--schedule-idle-fill)))

(defun denote-grid--apply-thumbnail (card)
  (pcase-let ((`(,start ,end ,item) card))
    (when (<= end (point-max))
      (let* ((inhibit-read-only t)
             (img (denote-grid--get-image item)))
        (when (eql start denote-grid--selected-start)
          (setq img (denote-grid--select-decorate img item)))
        (put-text-property start end 'display img)))))

(defun denote-grid--fill-visible ()
  (when denote-grid--pending-fill
    (let (still-pending)
      (dolist (card denote-grid--pending-fill)
        (let ((start (car card)) (visible nil))
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (when (and (>= start (window-start win)) (<= start (window-end win)))
              (setq visible t)))
          (if visible
              (denote-grid--apply-thumbnail card)
            (push card still-pending))))
      (setq denote-grid--pending-fill (nreverse still-pending)))))

(defun denote-grid--schedule-idle-fill ()
  (when (timerp denote-grid--fill-timer)
    (cancel-timer denote-grid--fill-timer))
  (when denote-grid--pending-fill
    (setq denote-grid--fill-timer
          (run-with-idle-timer 0.3 t #'denote-grid--idle-fill-tick (current-buffer)))))

(defun denote-grid--idle-fill-tick (buf)
  (if (not (buffer-live-p buf))
      (when (timerp denote-grid--fill-timer) (cancel-timer denote-grid--fill-timer))
    (with-current-buffer buf
      (denote-grid--fill-visible)
      (let ((n 0))
        (while (and denote-grid--pending-fill (< n denote-grid-lazy-batch-size))
          (denote-grid--apply-thumbnail (pop denote-grid--pending-fill))
          (setq n (1+ n))))
      (unless denote-grid--pending-fill
        (when (timerp denote-grid--fill-timer)
          (cancel-timer denote-grid--fill-timer)
          (setq denote-grid--fill-timer nil))))))

(defun denote-grid--card-index-at (pos)
  (let ((vec denote-grid--card-starts)
        (low 0)
        (high (1- (length denote-grid--card-starts)))
        (ans 0))
    (while (<= low high)
      (let ((mid (/ (+ low high) 2)))
        (if (<= (aref vec mid) pos)
            (progn
              (setq ans mid)
              (setq low (1+ mid)))
          (setq high (1- mid)))))
    ans))

(defun denote-grid--snap-to-card (&optional direction)
  "Snap point to the nearest valid card starting position.
DIRECTION non-nil means move forward, nil means move backward."
  (let ((dir (or direction 1)))
    (while (and (not (get-text-property (point) 'denote-grid-item))
                (not (if (> dir 0) (eobp) (bobp))))
      (forward-line dir))
    (when-let ((item (get-text-property (point) 'denote-grid-item)))
      (let ((idx (denote-grid--card-index-at (point))))
        (when (>= idx 0)
          (goto-char (aref denote-grid--card-starts idx)))))))

;;; Interactive Commands

;;;###autoload
(defun denote-grid--goto-path (path)
  "Move point to the card whose path is PATH, else to the first card."
  (let ((starts denote-grid--card-starts))
    (when (> (length starts) 0)
      (goto-char
       (or (and path
                (cl-find-if (lambda (st)
                              (let ((it (get-text-property st 'denote-grid-item)))
                                (and it (equal (denote-grid-item-path it) path))))
                            starts))
           (aref starts 0))))))

(defun denote-grid--enter (dirs &optional focus-path)
  "Show DIRS (a list of directories), leaving point on FOCUS-PATH if given."
  (setq denote-grid--dirs dirs)
  (denote-grid--render)
  (denote-grid--goto-path focus-path))

(defun denote-grid-up ()
  "Go up to the parent folder."
  (interactive)
  (let ((cur (car denote-grid--dirs)))
    (cond ((cdr denote-grid--dirs) (user-error "Already at the top"))
          ((member cur denote-grid--roots)
           (if (cdr denote-grid--roots)
               (denote-grid--enter denote-grid--roots cur)
             (user-error "Already at the top")))
          (t (denote-grid--enter
              (list (file-name-directory (directory-file-name cur))) cur)))))

(defun denote-grid-open (&optional dir)
  "Open the denote grid view for DIR.
If DIR is nil, automatically fallback to `denote-directory` (or its first
entry if `denote-directory` is a list)."
  (interactive
   (list (when current-prefix-arg
           (let ((default-dir (if (listp denote-directory)
                                  (car denote-directory)
                                denote-directory)))
             (read-directory-name "Denote directory: " default-dir)))))
  (let* ((target (or dir denote-directory))
         (primary-dir (expand-file-name (if (listp target) (car target) target)))
         (buf-name (format "*denote-grid: %s*" (file-name-nondirectory (directory-file-name primary-dir))))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (denote-grid-mode)
      (setq-local denote-grid--source-directory target)
      (setq-local denote-grid--roots (mapcar #'denote-grid--norm-dir
                                             (if (listp target) target (list target))))
      (setq-local denote-grid--dirs denote-grid--roots)
      (setq-local denote-grid--cache-dir (denote-grid--cache-dir-for primary-dir))
      (setq-local denote-grid--items (denote-grid--collect-items target))
      (denote-grid--render))
    (switch-to-buffer buf)))

;;;###autoload
(defun denote-grid-from-dired ()
  "Open denote grid view displaying files from the current Dired buffer."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "denote-grid: not in a Dired buffer"))
  (let* ((dired-buf (current-buffer))
         (dir (expand-file-name default-directory))
         (buf-name (format "*denote-grid dired: %s*" (buffer-name dired-buf)))
         (buf (get-buffer-create buf-name))
         (items (denote-grid--collect-items-from-dired)))
    (unless items
      (user-error "denote-grid: no valid denote files found in this Dired buffer"))
    (with-current-buffer buf
      (denote-grid-mode)
      (setq-local denote-grid--source-directory dir)
      (setq-local denote-grid--source-dired-buffer dired-buf)
      (setq-local denote-grid--roots (list (denote-grid--norm-dir dir)))
      (setq-local denote-grid--dirs denote-grid--roots)
      (setq-local denote-grid--cache-dir (denote-grid--cache-dir-for dir))
      (setq-local denote-grid--items items)
      (denote-grid--render))
    (switch-to-buffer buf)))

(defun denote-grid-jump-to-dired ()
  "Jump to the file under point in a Dired buffer."
  (interactive)
  (if-let* ((it (get-text-property (point) 'denote-grid-item))
            (file (expand-file-name (denote-grid-item-path it))))
      (let ((dired-buf denote-grid--source-dired-buffer))
        (if (and dired-buf (buffer-live-p dired-buf))
            (progn
              (pop-to-buffer dired-buf)
              (dired-goto-file file))
          (dired (file-name-directory file))
          (dired-goto-file file)))
    (user-error "No denote item at point")))

(defun denote-grid-open-at-point ()
  "Open the card at point: enter a folder, or visit the file."
  (interactive)
  (if-let ((item (get-text-property (point) 'denote-grid-item)))
      (if (eq (denote-grid-item-type item) 'channel)
          (denote-grid--enter (list (denote-grid-item-path item)))
        (find-file (denote-grid-item-path item)))
    (user-error "No item at point")))

(defun denote-grid-toggle-orphan ()
  "Toggle orphan view mode in denote-grid."
  (interactive)
  (setq denote-grid--orphan-p (not denote-grid--orphan-p))
  (when denote-grid--orphan-p
    (setq denote-grid--cluster-p nil))
  (denote-grid--render))

(defun denote-grid-toggle-cluster ()
  "Toggle cluster view mode in denote-grid."
  (interactive)
  (setq denote-grid--cluster-p (not denote-grid--cluster-p))
  (when denote-grid--cluster-p
    (setq denote-grid--orphan-p nil))
  (denote-grid--render))

(defun denote-grid-filter (query)
  "Filter items in the grid buffer by QUERY."
  (interactive (list (read-string "Filter grid (text or #tag): " denote-grid--filter)))
  (setq denote-grid--filter query)
  (denote-grid--render))

(defun denote-grid-filter-tag ()
  "Filter the grid by a tag chosen with completion (empty input clears)."
  (interactive)
  (let* ((tags (sort (delete-dups
                      (apply #'append (mapcar #'denote-grid-item-tags denote-grid--items)))
                     #'string<))
         (tag (completing-read "Filter by tag (empty to clear): " tags nil t)))
    (setq denote-grid--filter (if (string-empty-p tag) "" (concat "#" tag)))
    (denote-grid--render)))

(defun denote-grid-sort-cycle ()
  "Cycle through sorting keys (date -> title -> tags -> type)."
  (interactive)
  (setq denote-grid--sort-key
        (pcase denote-grid--sort-key
          ('date 'title)
          ('title 'tags)
          ('tags 'type)
          (_ 'date)))
  (denote-grid--render))

(defun denote-grid-sort-reverse ()
  "Toggle ascending/descending order for current sort."
  (interactive)
  (setq denote-grid--sort-desc (not denote-grid--sort-desc))
  (denote-grid--render))

(defun denote-grid-refresh ()
  "Refresh the grid buffer."
  (interactive)
  (when denote-grid--source-directory
    (setq denote-grid--items
          (if (and denote-grid--source-dired-buffer
                   (buffer-live-p denote-grid--source-dired-buffer))
              (with-current-buffer denote-grid--source-dired-buffer
                (denote-grid--collect-items-from-dired))
            (denote-grid--collect-items denote-grid--source-directory)))
    (setq denote-grid--links-cache-key nil
          denote-grid--clusters-cache-key nil)
    (denote-grid--prune-image-cache denote-grid--items)
    (denote-grid--render)))

(defun denote-grid-next-card (&optional count)
  "Move cursor forward by COUNT cards."
  (interactive "p")
  (let* ((cnt (or count 1))
         (idx (denote-grid--card-index-at (point)))
         (max-idx (1- (length denote-grid--card-starts)))
         (target (min max-idx (+ idx cnt))))
    (when (and (>= target 0) denote-grid--card-starts)
      (goto-char (aref denote-grid--card-starts target)))))

(defun denote-grid-prev-card (&optional count)
  "Move cursor backward by COUNT cards."
  (interactive "p")
  (denote-grid-next-card (- (or count 1))))

(defun denote-grid-down-card (&optional count)
  "Move cursor down by COUNT rows in the grid."
  (interactive "p")
  (let ((cols (denote-grid--cards-per-row)))
    (denote-grid-next-card (* (or count 1) cols))))

(defun denote-grid-up-card (&optional count)
  "Move cursor up by COUNT rows in the grid."
  (interactive "p")
  (denote-grid-down-card (- (or count 1))))

(provide 'denote-grid)
;;; denote-grid.el ends here
