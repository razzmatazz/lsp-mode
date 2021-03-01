;;; lsp-csharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jostein Kjønigsen, Saulius Menkevicius

;; Author: Saulius Menkevicius <saulius.menkevicius@fastmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lsp-csharp client

;;; Code:

(require 'lsp-mode)
(require 'gnutls)
(require 'f)

(defgroup lsp-csharp nil
  "LSP support for C#, using the Omnisharp Language Server.
Version 1.34.3 minimum is required."
  :group 'lsp-mode
  :link '(url-link "https://github.com/OmniSharp/omnisharp-roslyn"))

(defcustom lsp-csharp-server-install-dir
  (f-join lsp-server-install-dir "omnisharp-roslyn/")
  "Installation directory for OmniSharp Roslyn server."
  :group 'lsp-csharp
  :type 'directory)

(defcustom lsp-csharp-server-path
  nil
  "The path to the OmniSharp Roslyn language-server binary.
Set this if you have the binary installed or have it built yourself."
  :group 'lsp-csharp
  :type '(string :tag "Single string value or nil"))

(defcustom lsp-csharp-test-run-buffer-name
  "*lsp-csharp test run*"
  "The name of buffer used for outputing lsp-csharp test run results."
  :group 'lsp-csharp
  :type 'string)

(defcustom lsp-csharp-enable-decompilation-support
  nil
  "Decompile bytecode when browsing method metadata for types in assemblies.
Otherwise only declarations for the methods are visible (the default)."
  :group 'lsp-csharp
  :type 'boolean)

(defun lsp-csharp--environment-fn ()
  "Build environment structure for current values of lsp-csharp customizables.

See https://github.com/OmniSharp/omnisharp-roslyn/wiki/Configuration-Options"
  `(("OMNISHARP_RoslynExtensionsOptions:enableDecompilationSupport" . ,(if lsp-csharp-enable-decompilation-support "true" "false"))))

(defun lsp-csharp--version-list-latest (lst)
  (->> lst
       (-sort (lambda (a b) (not (version<= (substring a 1)
                                            (substring b 1)))))
       cl-first))

(defun lsp-csharp--latest-installed-version ()
  "Returns latest version of the server installed on the machine (if any)."
  (lsp-csharp--version-list-latest
   (when (f-dir? lsp-csharp-server-install-dir)
     (seq-filter
      (lambda (f) (s-starts-with-p "v" f))
      (seq-map 'f-filename (f-entries lsp-csharp-server-install-dir))))))

(defun lsp-csharp--fetch-json (url)
  "Retrieves and parses JSON from URL."
  (with-temp-buffer
    (url-insert-file-contents url)
    (let ((json-false :false)) (json-read))))

(defun lsp-csharp--latest-available-version ()
  "Returns latest version of the server available from github."
  (lsp-csharp--version-list-latest
   (seq-map (lambda (elt) (s-trim (cdr (assq 'name elt))))
            (lsp-csharp--fetch-json "https://api.github.com/repos/OmniSharp/omnisharp-roslyn/releases"))))

(defun lsp-csharp--server-dir (version)
  "The location of the installed OmniSharp server for VERSION."
  (when version
    (f-join (expand-file-name lsp-csharp-server-install-dir) version)))

(defun lsp-csharp--server-bin (version)
  "The location of OmniSharp executable/script to use to start the server."
  (let ((server-dir (lsp-csharp--server-dir version)))
    (when server-dir
      (f-join server-dir (cond ((eq system-type 'windows-nt) "OmniSharp.exe")
                               (t "run"))))))

(defun lsp-csharp--server-package-filename ()
  "Returns name of tgz/zip file to be used for downloading the server
for auto installation.

On Windows we're trying to avoid a crash starting 64bit .NET PE binaries in
Emacs by using x86 version of omnisharp-roslyn on older (<= 26.4) versions
of Emacs. See https://lists.nongnu.org/archive/html/bug-gnu-emacs/2017-06/msg00893.html"
  (cond ((eq system-type 'windows-nt)
         (if (and (string-match "^x86_64-.*" system-configuration)
                  (version<= "26.4" emacs-version))
             "omnisharp-win-x64.zip"
           "omnisharp-win-x86.zip"))
        ((eq system-type 'darwin)
         "omnisharp-osx.tar.gz")
        ((and (eq system-type 'gnu/linux)
              (or (eq (string-match "^x86_64" system-configuration) 0)
                  (eq (string-match "^i[3-6]86" system-configuration) 0)))
         "omnisharp-linux-x64.tar.gz")
        (t "omnisharp-mono.tar.gz")))

(defun lsp-csharp--server-package-url (version)
  "Returns URL to tgz/zip file to be used for downloading the server VERSION
for installation."
  (concat "https://github.com/OmniSharp/omnisharp-roslyn/releases/download"
          "/" version
          "/" (lsp-csharp--server-package-filename)))

(defun lsp-csharp--extract-server (url filename reinstall)
  "Downloads and extracts a tgz/zip into the same directory."
  ;; remove the file if reinstall is set
  (when (and reinstall (f-exists-p filename))
    (f-delete filename))

  (lsp-csharp--download url filename)

  (let ((target-dir (f-dirname filename)))
    (message "lsp-csharp: extracting \"%s\" to \"%s\"" (f-filename filename) target-dir)
    (lsp-csharp--extract filename target-dir)))

(defun lsp-csharp-update-server ()
  "Checks if the currently installed version (if any) is lower than then one
available on github and if so, downloads and installs a newer version."
  (interactive)
  (let ((latest-version (lsp-csharp--latest-available-version))
        (installed-version (lsp-csharp--latest-installed-version)))
    (if latest-version
        (progn
          (when (and latest-version
                     (or (not installed-version)
                         (version< (substring installed-version 1)
                                   (substring latest-version 1))))
            (lsp-csharp--install-server latest-version nil))
          (message "lsp-csharp-update-server: latest installed version is %s; latest available is %s"
                   (lsp-csharp--latest-installed-version)
                   latest-version))
      (message "lsp-csharp-update-server: cannot retrieve latest version info"))))

(defun lsp-csharp--install-server (update-version ask-confirmation)
  "Installs (or updates to UPDATE-VERSION) server binary unless it is already installed."
  (let ((installed-version (lsp-csharp--latest-installed-version))
        (target-version (or update-version (lsp-csharp--latest-available-version))))
    (when (and target-version
               (not (string-equal installed-version target-version)))
      (message "lsp-csharp-update-server: current version is %s; installing %s.."
               (or installed-version "(none)")
               target-version)
      (when (or (not ask-confirmation)
                (yes-or-no-p (format "OmniSharp Roslyn Server %s. Do you want to download and install %s now?"
                                     (if installed-version
                                         (format "can be updated, currently installed version is %s" installed-version)
                                       "is not installed")
                                     target-version)))
        (let ((new-server-dir (lsp-csharp--server-dir target-version))
              (new-server-bin (lsp-csharp--server-bin target-version))
              (package-filename (lsp-csharp--server-package-filename))
              (package-url (lsp-csharp--server-package-url target-version)))

          (mkdir new-server-dir t)

          (lsp-csharp--extract-server package-url
                                      (f-join new-server-dir package-filename)
                                      nil)

          (unless (and new-server-bin (file-exists-p new-server-bin))
            (error "Failed to auto-install the server %s; file \"%s\" was not found"
                   target-version new-server-bin)))))))

(defun lsp-csharp--get-or-install-server ()
  "Resolves path to server binary installed, otherwise, if not found
will ask the user if we can download and install it.
Returns location of script or a binary to use to start the server."
  (let ((installed-bin (lsp-csharp--server-bin (lsp-csharp--latest-installed-version))))
    (if (and installed-bin (file-exists-p installed-bin))
        installed-bin
      (lsp-csharp--install-server nil t)
      (let ((installed-bin (lsp-csharp--server-bin (lsp-csharp--latest-installed-version))))
        (unless installed-bin (error "Server binary is required for LSP C# to work."))
        installed-bin))))

(defun lsp-csharp--download (url filename)
  "Downloads file from URL as FILENAME. Will not do anything should
the file exist already."
  (unless (f-exists-p filename)
    (message (format "lsp-csharp: downloading from \"%s\"..." url))
    (let ((gnutls-algorithm-priority
           (if (and (not gnutls-algorithm-priority)
                    (boundp 'libgnutls-version)
                    (>= libgnutls-version 30603)
                    (version<= emacs-version "26.2"))
               "NORMAL:-VERS-TLS1.3"
             gnutls-algorithm-priority)))
      (url-copy-file url filename nil))))

(defun lsp-csharp--extract (filename target-dir)
  "Extracts FILENAME which is a downloaded omnisharp-roslyn server
tarball or a zip file (based on a current platform) to TARGET-DIR."
  (cond
   ((eq system-type 'windows-nt)
    ;; on windows, we attempt to use powershell v5+, available on Windows 10+
    (let ((powershell-version (substring
                               (shell-command-to-string "powershell -command \"(Get-Host).Version.Major\"")
                               0 -1)))
      (if (>= (string-to-number powershell-version) 5)
          (call-process "powershell"
                        nil
                        nil
                        nil
                        "-command"
                        (concat "add-type -assembly system.io.compression.filesystem;"
                                "[io.compression.zipfile]::ExtractToDirectory(\"" filename "\", \"" target-dir "\")"))

        (message (concat "lsp-csharp: for automatic server installation procedure"
                         " to work on Windows you need to have powershell v5+ installed")))))

   ((or (eq system-type 'gnu/linux)
        (eq system-type 'darwin))
    (call-process "tar" nil nil t "xf" filename "-C" target-dir))

   (t (error "lsp-csharp cannot extract \"%s\" on platform %s (yet)" filename system-type))))

(defun lsp-csharp--language-server-path ()
  "Resolves path to use to start the server."
  (if lsp-csharp-server-path
      lsp-csharp-server-path
    (lsp-csharp--server-bin (lsp-csharp--latest-installed-version))))

(defun lsp-csharp--language-server-command ()
  "Resolves path and arguments to use to start the server."
  (list (lsp-csharp--language-server-path) "-lsp"))

(lsp-defun lsp-csharp-open-project-file ()
  "Open corresponding project file  (.csproj) for the current file."
  (interactive)
  (-let* ((project-info-req (lsp-make-omnisharp-project-information-request :file-name (buffer-file-name)))
          (project-info (lsp-request "o#/project" project-info-req))
          ((&omnisharp:ProjectInformation :ms-build-project) project-info)
          ((&omnisharp:MsBuildProject :path) ms-build-project))
    (find-file path)))

(defun lsp-csharp--get-buffer-cs-elements ()
  "Retrieve code structure by calling into the /v2/codestructure endpoint.
Returns :elements from omnisharp:CodeStructureResponse."
  (-let* ((code-structure (lsp-request "o#/v2/codestructure"
                                       (lsp-make-omnisharp-code-structure-request :file-name (buffer-file-name))))
          ((&omnisharp:CodeStructureResponse :elements) code-structure))
    elements))

(defun lsp-csharp--cs-inspect-elements-recursively (fn elements)
  (seq-each
   (lambda (el)
     (funcall fn el)
     (-let (((&omnisharp:CodeElement :children) el))
       (lsp-csharp--cs-inspect-elements-recursively fn children)))
   elements))

(defun lsp-csharp--cs-collect-recursively (predicate elements)
  (let ((results nil))
    (lsp-csharp--cs-inspect-elements-recursively (lambda (el)
                                                   (if (funcall predicate el)
                                                       (setq results (cons el results))))
                                                 elements)
    results))

(defun lsp-csharp--cs-l-c-within-range (l c range)
  "Return 't when L (line) and C (column) are within the RANGE."
  (-let* (((&omnisharp:Range :start :end) range)
          ((&omnisharp:Point :line start-l :column start-c) start)
          ((&omnisharp:Point :line end-l :column end-c) end))
    (or (and (= l start-l) (>= c start-c) (or (> end-l start-l) (<= c end-c)))
        (and (> l start-l) (< l end-l))
        (and (= l end-l) (<= c end-c)))))

(defun lsp-csharp--cs-element-stack-on-l-c (l c elements)
  (let ((matching-element (seq-find (lambda (el)
                                      (-let* (((&omnisharp:CodeElement :ranges) el)
                                              ((&hash "full" full-range) ranges))
                                        (lsp-csharp--cs-l-c-within-range l c full-range)))
                                    elements)))
    (if matching-element
        (-let (((&omnisharp:CodeElement :children) matching-element))
          (cons matching-element (lsp-csharp--cs-element-stack-on-l-c l c children))))))

(defun lsp-csharp--cs-element-stack-at-point ()
  (let ((pos-line (- (line-number-at-pos) 1))
        (pos-col (current-column)))
    (lsp-csharp--cs-element-stack-on-l-c pos-line
                                         pos-col
                                         (lsp-csharp--get-buffer-cs-elements))))

(defun lsp-csharp--cs-unit-test-method-p (el)
  (-when-let* (((&omnisharp:CodeElement :kind :properties) el)
               ((&hash "testMethodName" test-method-name
                       "testFramework" test-framework) properties))
    (list test-method-name test-framework)))

(defun lsp-csharp--unit-test-start (test-method-framework test-method-names)
  (if (and test-method-framework test-method-names)
      (let ((request-message (lsp-make-omnisharp-run-tests-in-class-request
                              :file-name (buffer-file-name)
                              :test-frameworkname test-method-framework
                              :method-names test-method-names)))
        (lsp-csharp--reset-test-buffer t)
        (lsp-session-set-metadata "last-test-method-framework" test-method-framework)
        (lsp-session-set-metadata "last-test-method-names" test-method-names)
        (lsp-request-async "o#/v2/runtestsinclass"
                           (json-parse-string (json-encode request-message))
                           (lambda (_)
                             (message "lsp-csharp: Test run has started"))))
    (message "lsp-csharp: No test methods to run")))

(defun lsp-csharp--reset-test-buffer (present-buffer)
  "Create new or reuse an existing test result output buffer.

PRESENT-BUFFER will make the buffer be presented to the user."
  (let ((existing-buffer (get-buffer lsp-csharp-test-run-buffer-name))
        (project-root-dir (lsp--suggest-project-root)))
    (if existing-buffer
        (progn
          (with-current-buffer existing-buffer
            (setq buffer-read-only nil)
            (erase-buffer)
            (setq buffer-read-only t)
            (setq default-directory project-root-dir))
          existing-buffer)
      (let ((buffer (get-buffer-create lsp-csharp-test-run-buffer-name)))
        (with-current-buffer buffer
          (setq default-directory project-root-dir)
          (compilation-mode)
          buffer))))

  (if present-buffer
      (display-buffer lsp-csharp-test-run-buffer-name)))

(defun lsp-csharp--test-message (message)
  "Emit a MESSAGE to lsp-csharp test run buffer."
  (let ((existing-buffer (get-buffer lsp-csharp-test-run-buffer-name)))
    (if existing-buffer
        (with-current-buffer existing-buffer
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert message)
          (insert "\n")
          (setq buffer-read-only t)))))

(defun lsp-csharp-run-test-at-point ()
  "Start test run at current point (if any)."
  (interactive)
  (let* ((stack (lsp-csharp--cs-element-stack-at-point))
         (element-on-point (car (last stack)))
         (test-method (lsp-csharp--cs-unit-test-method-p element-on-point))
         (test-method-name (car test-method))
         (test-method-framework (car (cdr test-method))))
    (lsp-csharp--unit-test-start test-method-framework (list test-method-name))))

(defun lsp-csharp-run-all-tests-in-buffer ()
  "Start test run for all test methods in current buffer."
  (interactive)
  (let* ((elements (lsp-csharp--get-buffer-cs-elements))
         (test-methods (lsp-csharp--cs-collect-recursively 'lsp-csharp--cs-unit-test-method-p elements))
         (test-method-framework (car (cdr (lsp-csharp--cs-unit-test-method-p (car test-methods)))))
         (test-method-names (mapcar (lambda (method)
                                      (car (lsp-csharp--cs-unit-test-method-p method)))
                                    test-methods)))
    (lsp-csharp--unit-test-start test-method-framework test-method-names)))


(defun lsp-csharp-run-test-in-buffer ()
  "Start test run for selected method in current buffer."
  (interactive)
  (when-let* ((elements (lsp-csharp--get-buffer-cs-elements))
              (test-methods (lsp-csharp--cs-collect-recursively 'lsp-csharp--cs-unit-test-method-p elements))
              (test-method-framework (car (cdr (lsp-csharp--cs-unit-test-method-p (car test-methods)))))
              (test-method-names (mapcar (lambda (method)
                                           (car (lsp-csharp--cs-unit-test-method-p method)))
                                         test-methods))
              (selected-test-method-name (lsp--completing-read "Select test:" test-method-names 'identity)))
    (lsp-csharp--unit-test-start test-method-framework (list selected-test-method-name))))

(defun lsp-csharp-run-last-tests ()
  "Start tests that we ran last time on this workspace."
  (interactive)
  (if-let ((last-test-method-framework (lsp-session-get-metadata "last-test-method-framework"))
           (last-test-method-names (lsp-session-get-metadata "last-test-method-names")))
      (lsp-csharp--unit-test-start last-test-method-framework last-test-method-names)
    (message "lsp-csharp: No test method(s) found to be ran previously on this workspace")))

(lsp-defun lsp-csharp--handle-os-error (_workspace (&omnisharp:ErrorMessage :file-name :text))
  "Handle the 'o#/error' (interop) notification by displaying a message with lsp-warn."
  (lsp-warn "%s: %s" file-name text))

(lsp-defun lsp-csharp--handle-os-testmessage (_workspace (&omnisharp:TestMessageEvent :message-level :message))
  "Handle the 'o#/testmessage and display test message on lsp-csharp
test output buffer."
  (lsp-csharp--test-message message))

(lsp-defun lsp-csharp--handle-os-testcompleted (_workspace (&omnisharp:DotNetTestResult
                                                            :method-name
                                                            :outcome
                                                            :error-message
                                                            :error-stack-trace
                                                            :standard-output
                                                            :standard-error))
  "Handle the 'o#/testcompleted' message and display the results on the
lsp-csharp test output buffer."
  (let ((passed (string-equal "passed" outcome)))
    (lsp-csharp--test-message
     (format "[%s] %s "
             (propertize
              (upcase outcome)
              'font-lock-face (if passed
                                  '(:foreground "green" :weight bold)
                                '(:foreground "red" :weight bold)))
             method-name))

    (unless passed
      (lsp-csharp--test-message error-message)

      (if error-stack-trace
          (lsp-csharp--test-message error-stack-trace))

      (unless (= (seq-length standard-output) 0)
        (lsp-csharp--test-message "STANDARD OUTPUT:")
        (seq-doseq (stdout-line standard-output)
          (lsp-csharp--test-message stdout-line)))

      (unless (= (seq-length standard-error) 0)
        (lsp-csharp--test-message "STANDARD ERROR:")
        (seq-doseq (stderr-line standard-error)
          (lsp-csharp--test-message stderr-line))))))

(lsp-defun lsp-csharp--action-client-find-references ((&Command :arguments?))
  "Read first argument from ACTION as Location and display xrefs for that location
using the `textDocument/references' request."
  (-if-let* (((&Location :uri :range) (lsp-seq-first arguments?))
             ((&Range :start range-start) range)
             (find-refs-params (append (lsp--text-document-position-params (list :uri uri) range-start)
                                       (list :context (list :includeDeclaration json-false))))
             (locations-found (lsp-request "textDocument/references" find-refs-params)))
      (lsp-show-xrefs (lsp--locations-to-xref-items locations-found) nil t)
    (message "No references found")))

(lsp-defun lsp-csharp--handle-os-error (_workspace (&omnisharp:ErrorMessage :file-name :text))
  "Handle the 'o#/error' (interop) notification by displaying a message with lsp-warn."
  (lsp-warn "%s: %s" file-name text))

(lsp-defun lsp-csharp--omnisharp-metadata-uri-handler (uri)
  "Handles omnisharp:/(metadata) uri from omnisharp-roslyn server.

The uri is parsed and then 'o#/metadata' request is issued to retrieve
metadata from the server. A cache file is created on project root dir that
stores this metadata and filename is returned so lsp-mode can display this file."
  (string-match "^omnisharp:/metadata/Project/\\(.+\\)/Assembly/\\(.+\\)/Symbol/\\(.+\\)\.cs$" uri)
  (-when-let* ((project-name (url-unhex-string (match-string 1 uri)))
               (assembly-name (url-unhex-string (match-string 2 uri)))
               (type-name (url-unhex-string (match-string 3 uri)))
               (metadata-req (lsp-make-omnisharp-metadata-request :project-name project-name
                                                                  :assembly-name assembly-name
                                                                  :type-name type-name))
               (metadata (lsp-request "o#/metadata" metadata-req))
               ((&omnisharp:MetadataResponse :source-name :source) metadata)
               (filename (f-join ".cache"
                                 "lsp-csharp"
                                 "metadata"
                                 "projects" project-name
                                 "assemblies" assembly-name
                                 "types" (concat type-name ".cs")))
               (file-location (expand-file-name filename (lsp--suggest-project-root)))
               (metadata-file-location (concat file-location ".metadata-uri"))
               (path (f-dirname file-location)))

    (unless (find-buffer-visiting file-location)
      (unless (file-directory-p path)
        (make-directory path t))

      (with-temp-file metadata-file-location
        (insert uri))

      (with-temp-file file-location
        (insert source)))

    file-location))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-csharp--language-server-command
                                   (lambda ()
                                     (when-let ((binary (lsp-csharp--language-server-path)))
                                       (f-exists? binary))))

                  :major-modes '(csharp-mode csharp-tree-sitter-mode)
                  :server-id 'csharp
                  :environment-fn #'lsp-csharp--environment-fn
                  :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                  :notification-handlers (ht ("o#/projectadded" 'ignore)
                                             ("o#/projectchanged" 'ignore)
                                             ("o#/projectremoved" 'ignore)
                                             ("o#/packagerestorestarted" 'ignore)
                                             ("o#/msbuildprojectdiagnostics" 'ignore)
                                             ("o#/packagerestorefinished" 'ignore)
                                             ("o#/unresolveddependencies" 'ignore)
                                             ("o#/error" 'lsp-csharp--handle-os-error)
                                             ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                             ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                             ("o#/projectconfiguration" 'ignore)
                                             ("o#/projectdiagnosticstatus" 'ignore))
                  :uri-handlers (lsp-ht ("omnisharp" #'lsp-csharp--omnisharp-metadata-uri-handler))
                  :before-file-open-fn (lambda (_workspace)
                                         (let ((metadata-file-name (concat buffer-file-name ".metadata-uri")))
                                           (setq-local lsp-buffer-uri
                                                       (when (file-exists-p metadata-file-name)
                                                         (with-temp-buffer (insert-file-contents metadata-file-name)
                                                                           (buffer-string))))))
                  :download-server-fn
                  (lambda (_client callback error-callback _update?)
                    (condition-case err
                        (progn
                          (lsp-csharp--install-server nil nil)
                          (funcall callback))
                      (error (funcall error-callback (error-message-string err)))))))

(provide 'lsp-csharp)
;;; lsp-csharp.el ends here
