;; -*- buffer-read-only: t -*-

(defcustom ob-latex-doc-default-compile "pdflatex"
  "Default TeX compiler")

(defcustom ob-latex-doc-default-documentclass "standalone"
  "Default document class")

(defcustom ob-latex-doc-compile-separator "&&"
  "Separator for the commands provided through the :compile header
  argument.")

(defun ob-latex-doc-build-command (compile)
  "Given the content of the :compile header argument. A one-liner
for executing all those commands is returned."
  (string-join
   (seq-map
    ;; FIXME: Use shell-quote-argument wisely. Recall that flags are
    ;; also included in the compile argument.
    (lambda (x) (concat (string-trim x) " main"))
    (split-string compile ob-latex-doc-compile-separator t))
   " && "))

(defvar ob-latex-doc-temp-dir nil
  "This variable is not intended to be modified since its value
  is automatically set.")

(defun ob-latex-doc-set-temp-dir ()
  "Set the directory where code blocks are tangled, output
files are saved and log files created by TeX compilers are
saved.

The directory is fixed so that you can point a PDF viewer to
the main.pdf file in that directory."
  (unless ob-latex-doc-temp-dir
    (setq ob-latex-doc-temp-dir (make-temp-file "babel-" t))))

(defun ob-latex-doc-temp-dir-is-safe ()
  "Check that the directory which contain the files that are going to
be deleted is under the /tmp/ directory."
  ;; TODO: Check directory doesn't contain symbolic links.
  (if (or (not (string-match "^/tmp/" ob-latex-doc-temp-dir))
	  (string-match "/../" ob-latex-doc-temp-dir))
      nil
    t))

(defun ob-latex-doc-temp-dir-clean ()
  (unless (ob-latex-doc-temp-dir-is-safe)
    (error "Value of ob-latex-doc-temp-dir is not considered
    safe. Not proceeding to delete files."))

  ;; TODO: Include directories since some packages create them such as
  ;; minted.
  (dolist (file (directory-files-recursively ob-latex-doc-temp-dir ""))
    (delete-file file)))

(defun org-babel-expand-body:latex-doc (body params)
  (catch 'done
    
    (let ((expand (cdr (assq :expand params))))
      (when (equal expand "no")
	(throw 'done body)))
    
    (let ((prologue (cdr (assq :prologue params)))
	  (epilogue (cdr (assq :epilogue params)))
	  (class (cdr (assq :class params)))
	  (preamble (cdr (assq :preamble params)))
	  (enclose (cdr (assq :enclose params)))
	  (package (cdr (assq :package params)))
	  (compile (cdr (assq :compile params)))
	  content)

      (unless compile
	(setq compile ob-latex-doc-default-compile))
      
      (when compile
	(setq compile
	      (string-join
	       `("%% This file is intended to be compiled by executing the following"
		 "%% commands:"
		 ,@(seq-map
		    (lambda (x) (concat "%% $ " (string-trim x) " {filename}"))
		    (split-string compile ob-latex-doc-compile-separator t)))
	       "\n")))

      (when package
	(setq package
	      (string-join
	       (seq-map
		(lambda (x) (concat "\\usepackage{" x "}"))
		(split-string package " " t))
	       "\n")))

      (if (equal class "none")
	  (setq class nil)
	(setq class (concat "\\documentclass{"
			    (or class ob-latex-doc-default-documentclass)
			    "}")))
      
      (setq body (if (equal enclose "no")
		     body
		   (concat (string-join `("\\begin{document}"
					  ,body
					  "\\end{document}") "\n\n"))))
      
      (setq content `(,compile
		      ,prologue
		      ,class
		      ,package
		      ,preamble
		      ,body
		      ,epilogue))
      
      ;; nil is deleted to ensure that string-join doesn't insert
      ;; newlines when some header arguments haven't been provided.
      (setq content (delq nil content))

      (string-join content "\n\n"))))

(defun org-babel-execute:latex-doc (body params)
  (let ((compile (cdr (assq :compile params)))
	command)

    (ob-latex-doc-set-temp-dir)

    (unless compile
      (setq compile ob-latex-doc-default-compile))
    
    (setq compile (ob-latex-doc-build-command compile))

    (let ((buffer-name "*Async Shell Command* (latex-doc)")
	  (async-shell-command-buffer 'confirm-kill-process)
	  (default-directory ob-latex-doc-temp-dir)
	  (org-babel-default-header-args:latex-doc
	   `((:tangle . ,(concat ob-latex-doc-temp-dir "/main.tex"))))
	  (display-buffer-overriding-action
	   '(display-buffer-no-window)))

      ;; Remove auxiliary and log files to ensure that automatically
      ;; created files by previous compilations doesn't interfere with
      ;; the current one.
      (ob-latex-doc-temp-dir-clean)
      
      ;; Tangle the source code block at point.
      (let ((current-prefix-arg '(4)))
	(call-interactively 'org-babel-tangle))
      
      ;; Compile the document
      (message "Executing %s" compile)
      (async-shell-command compile buffer-name))))

(setq org-babel-default-header-args:latex-doc
      '((:results . "silent")))

(add-to-list 'org-src-lang-modes '("latex-doc" . latex))
