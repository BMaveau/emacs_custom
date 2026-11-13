;;; mermaid-preview.el --- Inline mermaid previews in markdown buffers  -*- lexical-binding: t; -*-

;; Renders ```mermaid fenced blocks with `mmdc' (mermaid-cli) to PNG and
;; shows the image in an overlay right after the block.

(defvar bm/mermaid-mmdc "/home/Benjamin.Maveau/.local/bin/mmdc")
(defvar bm/mermaid-cache-dir (expand-file-name "mermaid-cache/" user-emacs-directory))
(defvar bm/mermaid-scale 2)
;; Chromium's sandbox is unavailable here (AppArmor userns restriction).
(defvar bm/mermaid-puppeteer-config
  (expand-file-name "roamCustom/puppeteer-config.json" user-emacs-directory))

(defun bm/mermaid--clear ()
  (remove-overlays (point-min) (point-max) 'bm-mermaid t))

(defun bm/mermaid--show (buf end png)
  (when (and (buffer-live-p buf) (file-exists-p png))
    (with-current-buffer buf
      (save-excursion
        (let ((ov (make-overlay end end nil t)))
          (overlay-put ov 'bm-mermaid t)
          (overlay-put ov 'after-string
                       (concat "\n" (propertize
                                     " " 'display
                                     (create-image png 'png nil
                                                   :max-width (- (window-pixel-width) 40)))
                               "\n")))))))

(defun bm/mermaid--render (buf end code)
  (make-directory bm/mermaid-cache-dir t)
  (let* ((hash (md5 code))
         (in (expand-file-name (concat hash ".mmd") bm/mermaid-cache-dir))
         (png (expand-file-name (concat hash ".png") bm/mermaid-cache-dir)))
    (if (file-exists-p png)
        (bm/mermaid--show buf end png)
      (let ((coding-system-for-write 'utf-8))
        (write-region code nil in nil 'silent))
      (make-process
       :name "mmdc" :noquery t
       :buffer (get-buffer-create " *mmdc*")
       :command (list bm/mermaid-mmdc "-p" bm/mermaid-puppeteer-config
                      "-i" in "-o" png "-s"
                      (number-to-string bm/mermaid-scale) "-b" "white")
       :sentinel (lambda (p _)
                   (when (eq (process-status p) 'exit)
                     (if (zerop (process-exit-status p))
                         (bm/mermaid--show buf end png)
                       (message "mermaid: mmdc failed (see  *mmdc*)"))))))))

(defun bm/mermaid-preview ()
  "Show inline previews of all mermaid blocks in the buffer."
  (interactive)
  (unless (executable-find bm/mermaid-mmdc)
    (user-error "mmdc not found"))
  (bm/mermaid--clear)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*```[ \t]*mermaid[ \t]*$" nil t)
      (let ((start (1+ (match-end 0))))
        (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
          (let ((code (buffer-substring-no-properties
                       start (match-beginning 0)))
                (end (line-end-position)))
            (bm/mermaid--render (current-buffer) end code)))))))

(defun bm/mermaid-preview-clear ()
  (interactive)
  (bm/mermaid--clear))

(define-minor-mode bm/mermaid-preview-mode
  "Inline mermaid previews, refreshed on open and on save."
  :lighter " Mmd"
  (if bm/mermaid-preview-mode
      (progn (add-hook 'after-save-hook #'bm/mermaid-preview nil t)
             (bm/mermaid-preview))
    (remove-hook 'after-save-hook #'bm/mermaid-preview t)
    (bm/mermaid--clear)))

(provide 'mermaid-preview)
;;; mermaid-preview.el ends here
