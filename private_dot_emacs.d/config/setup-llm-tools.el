;;; setup-llm-tools.el --- Provide LLM tools for gptel -*- lexical-binding: t; -*-

;;; Commentary:

;; LLM tools:
;; - `create_file'
;; - `make_directory'
;; - `tree_dir'
;; - `search_in_files'
;; - `find-files'
;; - `open_file_or_dir'
;; - `list_buffers'
;; - `read_buffer'
;; - `execute_command'
;; - `duckduckgo_search'

;; TODO: Refactor LLM tools
;; 1. Merging similar tools to reduce tokens like this:
;;    file_operations = create, move, copy, remove files & directories and open
;;                      file_info (exiftool and etc)
;; (gptel-make-tool
;;  ...
;;  :args (list '(:name "operation"
;; 		     :type enum
;; 		     :enum '(("create_file" . "Create new file")
;; 			     ("move_file" . "...")
;; 			     ("delete_file" . "...")
;; 			     ("link_file" . "...")
;; 			     ........
;;
;; editing_operations = replace in file + insert into file
;; search_operations = search_in_files + find_files
;; buffer_operations = list_buffers + read_buffer
;; tree_view: tree_dir
;; execute_command
;; git_operations = git status, log and diff
;; web = duckduckgo_search + tavily-mpc (for web fetch) + ... fetch url
;;
;; See https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem
;; See https://github.com/munen/emacs.d?tab=readme-ov-file#tool-use
;; See https://codeberg.org/bajsicki/gptel-got/src/branch/main/gptel-got.el

;;; Code:

;; `create_file' tool
;; ############################################
;; Create a new file with the specified content

(gptel-make-tool
 :name "create_file"
 :function (lambda (path filename content)
             (with-temp-message (format "Creating file: %s in %s" filename path)
               (condition-case err
                   (let ((full-path (expand-file-name filename path)))
                     (with-temp-buffer
                       (insert content)
                       (write-file full-path))
                     (format "Created file %s in %s" filename path))
                 (error (format "Error creating file %s in %s: %s"
                                filename path (error-message-string err))))))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
                     :type string
                     :description "The directory where to create the file")
             '(:name "filename"
                     :type string
                     :description "The name of the file to create")
             '(:name "content"
                     :type string
                     :description "The content to write to the file"))
 :category "filesystem"
 :confirm t)

;; `make_directory' tool
;; ############################################################################
;; Create a new directory with the given name in the specified parent directory

(gptel-make-tool
 :name "make_directory"
 :function (lambda (parent name)
             (with-temp-message (format "Creating directory: %s in %s" name parent)
               (condition-case err
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s: %s"
                                name parent (error-message-string err))))))
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent"
                     :type string
                     :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
                     :type string
                     :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem"
 :confirm t)

;; `tree-dir' tool
;; #############################################################
;; Display directory structure as tree with optional depth limit

(gptel-make-tool
 :name "tree_dir"
 :description "Display directory structure as tree with optional depth limit"
 :args (list '(:name "path"
                     :type string
                     :description "Directory path to display")
             '(:name "max_depth"
                     :type number
                     :optional t
                     :description "Maximum recursion depth (default: 2)"))
 :function (lambda (path &optional max-depth)
             (let* ((depth (or max-depth 2))
                    (cmd (format "tree --charset=ascii -L %d '%s'"
                                 depth (expand-file-name path))))
               (condition-case err
                   (let ((result (shell-command-to-string cmd)))
                     (if (string-empty-p (string-trim result))
                         (format "Directory is empty or invalid: %s" path)
                       (string-trim result)))
                 (error (format "Error displaying tree for %s: %s"
                                path (error-message-string err))))))
 :category "filesystem"
 :confirm t)

;; `search_in_files' tool
;; #############################################################
;; Search for text patterns inside file contents (using ripgrep)

(gptel-make-tool
 :name "search_in_files"
 :description "Search for text patterns inside file contents (using ripgrep)"
 :args (list '(:name "pattern"
                     :type string
                     :description "Text pattern to search for in file contents")
             '(:name "directory"
                     :type string
                     :optional t
                     :description "Directory to search in"))
 :function (lambda (pattern &optional directory)
             (let ((dir (or directory default-directory)))
               (condition-case err
                   (let ((result (shell-command-to-string
                                  (format "rg -n -H '%s' %s" pattern dir))))
                     (if (string-empty-p (string-trim result))
                         (format "No text matches found for '%s' in %s" pattern dir)
                       (string-trim result)))
                 (error (format "Search error: %s" (error-message-string err))))))
 :category "search"
 :confirm t)

;; `find_files' tool
;; ##########################################
;; Find files by filename patterns (using fd)

(gptel-make-tool
 :name "find_files"
 :description "Find files by filename patterns (using fd)"
 :args (list '(:name "pattern"
                     :type string
                     :description "Filename pattern to search for")
             '(:name "directory"
                     :type string
                     :optional t
                     :description "Directory to search in"))
 :function (lambda (pattern &optional directory)
             (let ((dir (or directory default-directory)))
               (condition-case err
                   (let ((result (shell-command-to-string
                                  (format "fd -t f --glob '%s' %s" pattern dir))))
                     (if (string-empty-p (string-trim result))
                         (format "No files found matching '%s' in %s" pattern dir)
                       (string-trim result)))
                 (error (format "Search error: %s" (error-message-string err))))))
 :category "search"
 :confirm t)

;; `open_file_or_dir' tool
;; #####################################################
;; Open a file or directory in the current Emacs session

(gptel-make-tool
 :name "open_file_or_dir"
 :description "Open a file or directory in the current Emacs session"
 :args (list '(:name "path"
                     :type string
                     :description "Path to file or directory to open"))
 :function (lambda (path)
             (condition-case err
                 (let ((expanded-path (expand-file-name path)))
                   (if (file-exists-p expanded-path)
                       (progn
                         (find-file expanded-path)
                         (format "Opened: %s" path))
                     (format "File/Directory not found: %s" path)))
               (error (format "Error opening %s: %s"
                              path (error-message-string err)))))
 :category "emacs")

;; `list_buffers' tool
;; ######################################################
;; List all open Emacs buffers with their names and modes

(gptel-make-tool
 :name "list_buffers"
 :description "List all open Emacs buffers with their names and modes"
 :args nil
 :function (lambda ()
             (string-join
              (mapcar (lambda (buffer)
                        (format "%s (%s)"
                                (buffer-name buffer)
                                (with-current-buffer buffer major-mode)))
                      (buffer-list))
              "\n"))
 :category "emacs"
 :confirm t)

;; `read_buffer' tool
;; ######################################
;; Return the contents of an Emacs buffer

(gptel-make-tool
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer_name"
                     :type string
                     :description "Name of the buffer to read"))
 :function (lambda (buffer-name)
             (condition-case err
                 (let ((buffer (get-buffer buffer-name)))
                   (if buffer
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                     (format "Bufer not found: %s" buffer-name)))
               (error (format "Error reading buffer %s: %s"
                              buffer-name (error-message-string err)))))
 :category "emacs"
 :confirm t)

;; `execute_command' tool
;; #############################################
;; Execute a shell command and return its output

(gptel-make-tool
 :name "execute_command"
 :description "Execute a shell command and return its output"
 :args (list '(:name "command"
                     :type string
                     :description "Shell command to execute"))
 :function (lambda (command)
             (condition-case err
                 (let ((output (shell-command-to-string command)))
                   (if (string-empty-p (string-trim output))
                       "Command executed successfully (no output)"
                     (string-trim output)))
               (error (format "Error executing command '%s': %s"
                              command (error-message-string err)))))
 :category "system"
 :confirm t
 :include t)

;; `duckduckgo_search' tool
;; ###############################
;; Search the web using DuckDuckGo

(gptel-make-tool
 :name "duckduckgo_search"
 :description "Search the web using DuckDuckGo. Return search result with links and descriptions"
 :args (list '(:name "query"
                     :type string
                     :description "Search query string"))
 :function (lambda (query)
             (condition-case err
                 (let ((result (shell-command-to-string
                                (format "ddgr --noua --noprompt --expand --nocolor -- '%s'"
                                        (shell-quote-argument query)))))
                   (if (string-empty-p (string-trim result))
                       (format "No results found for query: %s" query)
                     (string-trim result)))
               (error (format "Search error for '%s': %s"
                              query (error-message-string err)))))
 :category "web")

(provide 'setup-llm-tools)
;;; setup-llm-tools.el ends here
