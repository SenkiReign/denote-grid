;;; denote-grid.el --- An are.na-style grid for denote -*- lexical-binding: t; -*-

;; Author:  Senki R.
;; Keywords: denote, notes, multimedia, moodboard, emacs, org-mode
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.1

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'dired)
(require 'color)

(defgroup denote-grid nil
  "An are.na-style local grid for denote notes, images, pdfs, and videos."
  :group 'convenience
  :prefix "denote-grid-")

(defcustom denote-grid-directory nil
  "Directory of denote files to browse as a grid."
  :type '(choice (const :tag "Use denote-directory / ask" nil) directory)
  :group 'denote-grid)

(defcustom denote-grid-thumbnail-size 220
  "Max width/height in pixels for grid thumbnails."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-note-snippet-length 220
  "How many characters of a note's body to show on its card."
  :type 'integer
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

(defface denote-grid-title-face
  '((t :inherit bold))
  "Face for the current card's title in the header line.")

(defface denote-grid-tag-face
  '((t :inherit shadow))
  "Face for tags in the header line.")

(defvar denote-grid--cache-dir nil)

(defun denote-grid--cache-dir-for (root)
  (let ((dir (expand-file-name ".denote-grid-thumbs/" root)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

(defconst denote-grid--name-re
  "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)--\\([^_]+\\)\\(?:__\\(.+\\)\\)?\\'")

(defconst denote-grid--id-re "[0-9]\\{8\\}T[0-9]\\{6\\}")

(cl-defstruct denote-grid-item
  id title tags path type mtime snippet-fetched snippet)

(defun denote-grid--file-type (ext)
  (cond
   ((member ext denote-grid-image-extensions) 'image)
   ((member ext denote-grid-video-extensions) 'video)
   ((member ext denote-grid-pdf-extensions) 'pdf)
   ((member ext denote-grid-text-extensions) 'text)
   (t 'other)))

(defun denote-grid--snippet (path)
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path nil 0 4000)
        (goto-char (point-min))
        (let (lines)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-match-p "\\`\\(#\\+\\|---\\)" line)
                (push line lines)))
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
    (when (string-match denote-grid--name-re stem)
      (let* ((id (match-string 1 stem))
             (title (replace-regexp-in-string "-" " " (match-string 2 stem)))
             (tags (and (match-string 3 stem) (split-string (match-string 3 stem) "_" t)))
             (type (denote-grid--file-type ext))
             (mtime (float-time (file-attribute-modification-time (file-attributes path)))))
        (make-denote-grid-item
         :id id :title title :tags tags :path path :type type :mtime mtime
         :snippet-fetched nil :snippet "")))))

(defun denote-grid--collect-items (root)
  (let (items)
    (dolist (f (directory-files-recursively root ".*" nil
                                             (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d))))))
      (unless (string-prefix-p "." (file-name-nondirectory f))
        (when-let ((item (denote-grid--parse-file f)))
          (push item items))))
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
        (cluster-of (make-hash-table :test 'equal))
        (n 0))
    (dolist (it items)
      (let ((id (denote-grid-item-id it)))
        (unless (gethash id cluster-of)
          (setq n (1+ n))
          (let ((queue (list id)))
            (while queue
              (let ((cur (pop queue)))
                (unless (gethash cur cluster-of)
                  (puthash cur n cluster-of)
                  (dolist (nb (gethash cur links)) (push nb queue)))))))))
    cluster-of))

(defun denote-grid--tag-counts (items)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (it items)
      (dolist (tag (denote-grid-item-tags it))
        (puthash tag (1+ (gethash tag counts 0)) counts)))
    counts))

(defun denote-grid--color-for (tags counts)
  (when-let ((tag (car-safe tags)))
    (when (>= (gethash tag counts 0) 2)
      (let* ((hash (secure-hash 'sha256 tag))
             (h1 (string-to-number (substring hash 0 4) 16))
             (h2 (string-to-number (substring hash 4 6) 16))
             (h3 (string-to-number (substring hash 6 8) 16))
             (hue (/ (mod (* h1 2654435761) 65536) 65536.0))
             (sat (+ 0.45 (* (/ h2 255.0) 0.50)))
             (lum (+ 0.40 (* (/ h3 255.0) 0.30)))
             (rgb (color-hsl-to-rgb hue sat lum)))
        (apply #'color-rgb-to-hex (append rgb '(2)))))))

(defun denote-grid--wrap-text (str width)
  (let ((words (split-string str))
        (lines nil) (cur ""))
    (dolist (w words)
      (if (< (length cur) 1)
          (setq cur w)
        (if (<= (+ (length cur) 1 (length w)) width)
            (setq cur (concat cur " " w))
          (push cur lines) (setq cur w))))
    (when (> (length cur) 0) (push cur lines))
    (nreverse lines)))

(defun denote-grid--note-svg (item counts)
  (let* ((w denote-grid-thumbnail-size) (h (round (* w 0.72)))
         (bg (face-background 'default nil t))
         (fg (face-foreground 'default nil t))
         (muted (face-foreground 'shadow nil t))
         (color (denote-grid--color-for (denote-grid-item-tags item) counts))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx 10)
    (when color
      (svg-rectangle svg 0 0 6 h :fill color :rx 3))
    (svg-text svg (truncate-string-to-width (denote-grid-item-title item) 24 nil nil "…")
              :x 16 :y 26 :fill fg :font-size 15 :font-weight "bold" :font-family "sans-serif")
    (let ((y 48))
      (dolist (line (denote-grid--wrap-text (denote-grid--get-snippet item) 32))
        (when (< y (- h 22))
          (svg-text svg line :x 16 :y y :fill muted :font-size 11 :font-family "sans-serif")
          (setq y (+ y 15)))))
    (when color
      (let ((tagstr (mapconcat (lambda (tg) (concat "#" tg)) (denote-grid-item-tags item) "  ")))
        (svg-text svg (truncate-string-to-width tagstr 36 nil nil "…")
                  :x 16 :y (- h 12) :fill color :font-size 10 :font-family "sans-serif")))
    (svg-image svg :ascent 'center)))

(defun denote-grid--placeholder-svg (item label counts)
  (let* ((w denote-grid-thumbnail-size) (h (round (* w 0.72)))
         (bg (face-background 'default nil t))
         (fg (face-foreground 'default nil t))
         (color (denote-grid--color-for (denote-grid-item-tags item) counts))
         (accent (or color (face-foreground 'shadow nil t)))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx 10)
    (svg-rectangle svg 0 0 w h :fill accent :fill-opacity "0.12" :rx 10)
    (when color
      (svg-rectangle svg 0 0 6 h :fill color :rx 3))
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

(defun denote-grid--boxed-raster (item raw-file label counts)
  (if (null raw-file)
      (denote-grid--placeholder-svg item label counts)
    (let* ((w denote-grid-thumbnail-size) (h (round (* w 0.72)))
           (dim (ignore-errors (image-size (create-image raw-file nil nil) t))))
      (if (not dim)
          (denote-grid--placeholder-svg item label counts)
        (let* ((iw (car dim)) (ih (cdr dim))
               (pad 6)
               (scale (min (/ (float (- w (* 2 pad))) iw) (/ (float (- h (* 2 pad))) ih)))
               (dw (max 1 (round (* iw scale))))
               (dh (max 1 (round (* ih scale))))
               (x (round (/ (- w dw) 2.0)))
               (y (round (/ (- h dh) 2.0)))
               (color (denote-grid--color-for (denote-grid-item-tags item) counts))
               (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink"))
               (bg (face-background 'default nil t)))
          (svg-rectangle svg 0 0 w h :fill bg :rx 10)
          (condition-case nil
              (progn
                (svg-embed svg raw-file (denote-grid--mime-for raw-file) nil
                           :x x :y y :width dw :height dh)
                (when color
                  (svg-rectangle svg 0 0 6 h :fill color :rx 3))
                (svg-image svg :ascent 'center))
            (error (denote-grid--placeholder-svg item label counts))))))))

(defvar denote-grid--image-cache (make-hash-table :test 'equal))

(defun denote-grid--video-thumb (item counts)
  (let ((out nil))
    (when (executable-find denote-grid-ffmpeg-executable)
      (setq out (denote-grid--cache-file item "jpg"))
      (unless (file-exists-p out)
        (call-process denote-grid-ffmpeg-executable nil nil nil
                       "-y" "-ss" "1" "-i" (denote-grid-item-path item)
                       "-frames:v" "1"
                       "-vf" (format "scale=%d:-1" (* 2 denote-grid-thumbnail-size))
                       "-loglevel" "quiet" out))
      (unless (file-exists-p out) (setq out nil)))
    (denote-grid--boxed-raster item out "VIDEO" counts)))

(defun denote-grid--pdf-thumb (item counts)
  (let ((out nil))
    (when (executable-find denote-grid-pdftoppm-executable)
      (let* ((base (denote-grid--cache-file item "pdfpage"))
             (png (concat base ".png")))
        (unless (file-exists-p png)
          (call-process denote-grid-pdftoppm-executable nil nil nil
                         "-png" "-f" "1" "-singlefile"
                         "-scale-to" (number-to-string (* 2 denote-grid-thumbnail-size))
                         (denote-grid-item-path item) base))
        (when (file-exists-p png) (setq out png))))
    (denote-grid--boxed-raster item out "PDF" counts)))

(defun denote-grid--image-thumb (item counts)
  (denote-grid--boxed-raster item (denote-grid-item-path item) "IMG" counts))

(defun denote-grid--get-image (item counts)
  (let* ((tag-color (denote-grid--color-for (denote-grid-item-tags item) counts))
         (key (list (denote-grid-item-id item) (denote-grid-item-mtime item)
                    (denote-grid-item-type item) denote-grid-thumbnail-size
                    (face-background 'default nil t) tag-color))
         (cached (gethash key denote-grid--image-cache)))
    (or cached
        (puthash key
                 (pcase (denote-grid-item-type item)
                   ('image (denote-grid--image-thumb item counts))
                   ('video (denote-grid--video-thumb item counts))
                   ('pdf (denote-grid--pdf-thumb item counts))
                   ('text (denote-grid--note-svg item counts))
                   (_ (let ((ext-label (upcase (or (file-name-extension (denote-grid-item-path item)) "FILE"))))
                        (denote-grid--placeholder-svg item ext-label counts))))
                 denote-grid--image-cache))))

(defvar-local denote-grid--items nil)
(defvar-local denote-grid--filter "")
(defvar-local denote-grid--sort-key 'date)
(defvar-local denote-grid--sort-desc t)
(defvar-local denote-grid--cluster-p nil)
(defvar-local denote-grid--current-item nil)
(defvar-local denote-grid--card-starts nil)
(defvar-local denote-grid--source-directory nil)
(defvar-local denote-grid--source-dired-buffer nil)
(defvar-local denote-grid--selection-overlay nil)
(defvar-local denote-grid--last-win-width nil)

(defvar denote-grid-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'denote-grid-open-at-point)
    (define-key m [mouse-1] #'denote-grid-open-at-point)
    (define-key m (kbd "d") #'denote-grid-jump-to-dired)
    (define-key m (kbd "/") #'denote-grid-filter)
    (define-key m (kbd "s") #'denote-grid-sort-cycle)
    (define-key m (kbd "r") #'denote-grid-sort-reverse)
    (define-key m (kbd "c") #'denote-grid-toggle-cluster)
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
  "Major mode for browsing denote files as an are.na-style grid."
  (setq truncate-lines nil)
  (setq header-line-format '(:eval (denote-grid--header-line)))
  (add-hook 'post-command-hook #'denote-grid--update-point-info nil t)
  (add-hook 'window-size-change-functions #'denote-grid--on-window-size-change nil t))

(defun denote-grid--header-line ()
  (if denote-grid--current-item
      (let ((it denote-grid--current-item))
        (format " %s  %s  %s"
                (propertize (denote-grid-item-title it) 'face 'denote-grid-title-face)
                (propertize (substring (denote-grid-item-id it) 0 8) 'face 'shadow)
                (propertize (mapconcat (lambda (tg) (concat "#" tg)) (denote-grid-item-tags it) " ")
                            'face 'denote-grid-tag-face)))
    (format " %d items  sort:%s%s  filter:%s%s"
            (length denote-grid--items) denote-grid--sort-key
            (if denote-grid--sort-desc "↓" "↑")
            (if (string-empty-p denote-grid--filter) "(none)" denote-grid--filter)
            (if denote-grid--cluster-p "  [clustered]" ""))))

(defun denote-grid--update-point-info ()
  (setq denote-grid--current-item (get-text-property (point) 'denote-grid-item))
  (when (and (derived-mode-p 'denote-grid-mode) denote-grid--card-starts)
    (unless (overlayp denote-grid--selection-overlay)
      (setq denote-grid--selection-overlay (make-overlay (point-min) (point-min)))
      (overlay-put denote-grid--selection-overlay 'face '(:box (:line-width 2 :color "#5b6ee1"))))
    (let ((idx (denote-grid--card-index-at (point))))
      (if (>= idx 0)
          (let ((start (aref denote-grid--card-starts idx)))
            (move-overlay denote-grid--selection-overlay start (1+ start)))
        (delete-overlay denote-grid--selection-overlay))))
  (force-mode-line-update))

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
                                    (mapconcat #'identity (denote-grid-item-tags item) " ") " "
                                    (denote-grid--get-snippet item)))))
        (or (condition-case nil (string-match-p filter hay) (error nil))
            (string-match-p (regexp-quote (downcase filter)) hay))))))

(defun denote-grid--sort-value (item key)
  (pcase key
    ('date (denote-grid-item-id item))
    ('title (denote-grid-item-title item))
    ('tags (or (car (denote-grid-item-tags item)) ""))
    ('type (symbol-name (denote-grid-item-type item)))))

(defun denote-grid--visible-items ()
  (let* ((filtered (cl-remove-if-not (lambda (it) (denote-grid--matches-p it denote-grid--filter))
                                      denote-grid--items))
         (sorted (sort (copy-sequence filtered)
                        (lambda (a b)
                          (let ((va (denote-grid--sort-value a denote-grid--sort-key))
                                (vb (denote-grid--sort-value b denote-grid--sort-key)))
                            (if denote-grid--sort-desc (string> va vb) (string< va vb)))))))
    (if denote-grid--cluster-p
        (let ((clusters (denote-grid--clusters sorted)))
          (sort (copy-sequence sorted)
                (lambda (a b)
                  (let ((ca (gethash (denote-grid-item-id a) clusters))
                        (cb (gethash (denote-grid-item-id b) clusters)))
                    (if (= ca cb)
                        (string> (denote-grid-item-id a) (denote-grid-item-id b))
                      (< ca cb))))))
      sorted)))

(defun denote-grid--render ()
  (let ((inhibit-read-only t)
        (pos (point))
        (clusters (and denote-grid--cluster-p (denote-grid--clusters denote-grid--items)))
        (starts nil))
    (erase-buffer)
    (let* ((items (denote-grid--visible-items))
           (counts (denote-grid--tag-counts items))
           (last-cluster nil))
      (if (null items)
          (insert (propertize "\n  (no items match)\n" 'face 'shadow))
        (dolist (it items)
          (when (and denote-grid--cluster-p clusters)
            (let ((c (gethash (denote-grid-item-id it) clusters)))
              (unless (eq c last-cluster)
                (unless (null last-cluster) (insert "\n\n"))
                (insert (propertize (format "  ·· cluster %d ··\n" c) 'face 'shadow))
                (setq last-cluster c))))
          (let ((img (denote-grid--get-image it counts))
                (start (point)))
            (push start starts)
            (insert-image img (denote-grid-item-title it))
            (put-text-property start (point) 'denote-grid-item it)
            (put-text-property start (point) 'help-echo
                                (format "%s\n%s\n%s"
                                        (denote-grid-item-title it)
                                        (denote-grid-item-id it)
                                        (mapconcat (lambda (tg) (concat "#" tg)) (denote-grid-item-tags it) " ")))
            (insert "  ")))))
    (setq denote-grid--card-starts (vconcat (nreverse starts)))
    (goto-char (min pos (point-max)))))

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

(defun denote-grid--calculate-columns ()
  (let* ((win-w (window-pixel-width))
         (card-w (+ denote-grid-thumbnail-size 20)))
    (max 1 (/ win-w card-w))))

(defun denote-grid-next-card (&optional n)
  (interactive "p")
  (when (> (length denote-grid--card-starts) 0)
    (let* ((cur (denote-grid--card-index-at (point)))
           (target (min (1- (length denote-grid--card-starts)) (+ cur (or n 1)))))
      (goto-char (aref denote-grid--card-starts target)))))

(defun denote-grid-prev-card (&optional n)
  (interactive "p")
  (when (> (length denote-grid--card-starts) 0)
    (let* ((cur (denote-grid--card-index-at (point)))
           (target (max 0 (- cur (or n 1)))))
      (goto-char (aref denote-grid--card-starts target)))))

(defun denote-grid-down-card (&optional n)
  (interactive "p")
  (when (> (length denote-grid--card-starts) 0)
    (let* ((cols (denote-grid--calculate-columns))
           (cur (denote-grid--card-index-at (point)))
           (target (min (1- (length denote-grid--card-starts)) (+ cur (* cols (or n 1))))))
      (goto-char (aref denote-grid--card-starts target)))))

(defun denote-grid-up-card (&optional n)
  (interactive "p")
  (when (> (length denote-grid--card-starts) 0)
    (let* ((cols (denote-grid--calculate-columns))
           (cur (denote-grid--card-index-at (point)))
           (target (max 0 (- cur (* cols (or n 1))))))
      (goto-char (aref denote-grid--card-starts target)))))

(defun denote-grid-open-at-point ()
  (interactive)
  (if-let ((it (get-text-property (point) 'denote-grid-item)))
      (find-file (denote-grid-item-path it))
    (user-error "No item at point")))

(defun denote-grid-jump-to-dired ()
  (interactive)
  (if-let ((it (get-text-property (point) 'denote-grid-item)))
      (dired-jump nil (denote-grid-item-path it))
    (user-error "No item at point")))

(defun denote-grid-filter (filter)
  (interactive (list (read-string "Filter (#tag or text): " denote-grid--filter)))
  (setq denote-grid--filter filter)
  (denote-grid--render))

(defun denote-grid-sort-cycle ()
  (interactive)
  (setq denote-grid--sort-key
        (pcase denote-grid--sort-key
          ('date 'title) ('title 'tags) ('tags 'type) ('type 'date)))
  (denote-grid--render))

(defun denote-grid-sort-reverse ()
  (interactive)
  (setq denote-grid--sort-desc (not denote-grid--sort-desc))
  (denote-grid--render))

(defun denote-grid-toggle-cluster ()
  (interactive)
  (setq denote-grid--cluster-p (not denote-grid--cluster-p))
  (denote-grid--render))

(defun denote-grid-refresh ()
  (interactive)
  (clrhash denote-grid--image-cache)
  (cond
   (denote-grid--source-dired-buffer
    (if (buffer-live-p denote-grid--source-dired-buffer)
        (setq denote-grid--items
              (with-current-buffer denote-grid--source-dired-buffer
                (denote-grid--collect-items-from-dired)))
      (message "denote-grid: source dired buffer is gone, keeping last known items")))
   (denote-grid--source-directory
    (setq denote-grid--items (denote-grid--collect-items denote-grid--source-directory))))
  (denote-grid--render))

;;;###autoload
(defun denote-grid-open (&optional dir)
  (interactive)
  (let* ((root (expand-file-name
                (or dir denote-grid-directory
                    (and (fboundp 'denote-directory) (denote-directory))
                    (read-directory-name "Denote directory: "))))
         (buf (get-buffer-create (format "*denote-grid: %s*" (file-name-nondirectory (directory-file-name root))))))
    (setq denote-grid--cache-dir (denote-grid--cache-dir-for root))
    (with-current-buffer buf
      (denote-grid-mode)
      (setq denote-grid--source-directory root)
      (setq denote-grid--items (denote-grid--collect-items root))
      (denote-grid--render))
    (switch-to-buffer buf)))

;;;###autoload
(defun denote-grid-from-dired ()
  (interactive)
  (let* ((src (current-buffer))
         (root (expand-file-name default-directory))
         (items (denote-grid--collect-items-from-dired))
         (buf (get-buffer-create (format "*denote-grid: %s*" (file-name-nondirectory (directory-file-name root))))))
    (setq denote-grid--cache-dir (denote-grid--cache-dir-for root))
    (with-current-buffer buf
      (denote-grid-mode)
      (setq denote-grid--source-dired-buffer src)
      (setq denote-grid--items items)
      (denote-grid--render))
    (switch-to-buffer buf)))

(provide 'denote-grid)
;;; denote-grid.el ends here
