((org-mode . ((org-export-with-toc . nil)
	      (after-save-hook lambda ()
			       (when (string= (file-name-extension buffer-file-name) "org")
				 ;; we use a slightly modified
				 ;; `org-jekyll-md-export-to-md' where
				 ;; we can simple pass an optional
				 ;; destination directory
				 (org-jekyll-md-export-to-md "../"))))))
