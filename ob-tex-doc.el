(defcustom ob-tex-doc-default-cmd "pdflatex"
  "Default TeX compiler command")

(defcustom ob-tex-doc-default-documentclass "standalone"
  "Default document class")

(defcustom ob-tex-doc-cmd-separator "&&"
  "Separator for the commands provided through the :cmd header
  argument.")

(defun ob-tex-doc-build-command (cmd)
  "Given the content of the :cmd header argument. A one-liner
for executing all those commands is returned."
  (string-join
   (seq-map
    ;; FIXME: Use shell-quote-argument wisely. Recall that flags are
    ;; also included in the cmd argument.
    (lambda (x) (concat (string-trim x) " main"))
    (split-string cmd ob-tex-doc-cmd-separator t))
   " && "))

(defvar ob-tex-doc-temp-dir nil
  "This variable is not intended to be modified since its value
  is automatically set.")

(defun ob-tex-doc-set-temp-dir ()
  "Set the directory where code blocks are tangled, output
files are saved and log files created by TeX compilers are
saved.

The directory is fixed so that you can point a PDF viewer to
the main.pdf file in that directory."
  (unless ob-tex-doc-temp-dir
    (setq ob-tex-doc-temp-dir (make-temp-file "babel-" t))))

(defun ob-tex-doc-temp-dir-in-tmp ()
  "Check directory, which contain the files that are generated
when compiling, is under the /tmp/ directory.

This is done to ensure that `ob-tex-doc-temp-dir-clean' doesn't
remove unintended files."
  ;; TODO: Check directory doesn't contain symbolic links.
  (if (or (not (string-match "^/tmp/" ob-tex-doc-temp-dir))
	  (string-match "/../" ob-tex-doc-temp-dir))
      nil
    t))

(defun ob-tex-doc-temp-dir-clean ()
  (unless (ob-tex-doc-temp-dir-in-tmp)
    (error "Value of ob-tex-doc-temp-dir is not under /tmp/. Not
    proceeding to delete files."))

  ;; TODO: Include directories since some packages create them such as
  ;; minted.
  (dolist (file (directory-files-recursively ob-tex-doc-temp-dir ""))
    (delete-file file)))

(defun org-babel-expand-body:tex-doc (body params)
  (catch 'done
    
    (let ((expand (cdr (assq :expand params))))
      (when (equal expand "no")
	(throw 'done body)))
    
    (let ((prologue (cdr (assq :prologue params)))
	  (epilogue (cdr (assq :epilogue params)))
	  (class (cdr (assq :class params)))
	  (preamble (cdr (assq :preamble params)))
	  (enclose (cdr (assq :enclose params)))
	  (pkg (cdr (assq :pkg params)))
	  (env (cdr (assq :env params)))
	  (cmd (cdr (assq :cmd params)))
	  (comment (cdr (assq :comment params)))
	  content)

      (if (equal comment "no")
	  (setq cmd nil)
	(progn
	  (unless cmd
	    (setq cmd ob-tex-doc-default-cmd))
	  
	  (setq cmd
		(string-join
		 `("%% This file is intended to be compiled by executing the following"
		   "%% commands:"
		   ,@(seq-map
		      (lambda (x) (concat "%% $ " (string-trim x) " {filename}"))
		      (split-string cmd ob-tex-doc-cmd-separator t)))
		 "\n"))))

      (when pkg
	(setq pkg
	      (string-join
	       (seq-map
		(lambda (x) (concat "\\usepackage{" x "}"))
		(split-string pkg " " t))
	       "\n")))

      (if (equal class "no")
	  (setq class nil)
	(setq class (concat "\\documentclass{"
			    (or class ob-tex-doc-default-documentclass)
			    "}")))

      (setq body (if (equal enclose "no")
		     body
		   (concat (string-join `(,(concat "\\begin{document}"
						   (unless (equal env "no")
						     (concat "\n\\begin{" env "}")))
					  ,(replace-regexp-in-string "^" "  " body)
					  ,(concat (unless (equal env "no")
						     (concat "\\end{" env "}\n"))
						   "\\end{document}"))
					"\n\n"))))
      
      (setq content `(,cmd
		      ,prologue
		      ,class
		      ,pkg
		      ,preamble
		      ,body
		      ,epilogue))
      
      ;; nil is deleted to ensure that string-join doesn't insert
      ;; newlines when some header arguments haven't been provided.
      (setq content (delq nil content))

      (string-join content "\n\n"))))

(defun org-babel-execute:tex-doc (body params)
  (let ((cmd (cdr (assq :cmd params)))
	command)

    (ob-tex-doc-set-temp-dir)

    (unless cmd
      (setq cmd ob-tex-doc-default-cmd))
    
    (setq cmd (ob-tex-doc-build-command cmd))

    (let ((buffer-name "*Async Shell Command* (tex-doc)")
	  (async-shell-command-buffer 'confirm-kill-process)
	  (default-directory ob-tex-doc-temp-dir)
	  (org-babel-default-header-args:tex-doc
	   `((:tangle . ,(concat ob-tex-doc-temp-dir "/main.tex"))))
	  (display-buffer-overriding-action
	   '(display-buffer-no-window)))

      ;; Remove auxiliary and log files to ensure that automatically
      ;; created files by previous compilations doesn't interfere with
      ;; the current one.
      (ob-tex-doc-temp-dir-clean)
      
      ;; Tangle the source code block at point.
      (let ((current-prefix-arg '(4)))
	(call-interactively 'org-babel-tangle))
      
      ;; Compile the document
      (message "Executing %s" cmd)
      (async-shell-command cmd buffer-name))))

(setq org-babel-default-header-args:tex-doc
      '((:results . "silent")))

(add-to-list 'org-src-lang-modes '("tex-doc" . latex))

(provide 'ob-tex-doc)
 
