;;; confluence.el --- Export an org section as HTML to the clipboard  -*- lexical-binding: t; -*-

(require 'ox-html)

(defun bm/org-section-to-clipboard-html ()
  "Export the current org subtree to HTML and copy it as text/html.
Pasting into Confluence then preserves the formatting.  Needs `wl-copy'
(Wayland) or `xclip' (X11)."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org buffer"))
  (let* ((html (org-export-as 'html t nil t
                             '(:with-toc nil :section-numbers nil)))
         (cmd (cond ((executable-find "wl-copy")
                     '("wl-copy" "--type" "text/html"))
                    ((executable-find "xclip")
                     '("xclip" "-selection" "clipboard" "-t" "text/html"))
                    (t (user-error "Install wl-clipboard or xclip")))))
    (with-temp-buffer
      (insert html)
      (let ((coding-system-for-write 'utf-8))
        (unless (zerop (apply #'call-process-region (point-min) (point-max)
                              (car cmd) nil nil nil (cdr cmd)))
          (error "%s failed" (car cmd)))))
    (message "Copied subtree as HTML (%d chars)" (length html))))

(provide 'confluence)
;;; confluence.el ends here
