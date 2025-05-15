;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'ewoc)
(require 'subr-x)
(require 'cl-lib)

(defvar chat-buffer-name "*Chat*"
  "Name of the chat buffer.")

(defvar-local chat-ewoc nil
  "EWOC instance for chat messages.")

(defvar-local chat-message-history nil
  "List of chat messages stored as a data structure.")

(defvar-local chat-message-count 0
  "Number of messages currently in memory.")

(defvar chat-max-messages 200
  "Maximum number of messages to keep in memory and visible in buffer.")

(defface chat-sender-face
  '((t :foreground "MediumPurple1" :weight bold))
  "Face for chat senders.")

(defface chat-system-face
  '((t :foreground "gray" :slant italic))
  "Face for system messages.")

(defface chat-timestamp-face
  '((t :foreground "gray60" :height 0.8 :slant italic))
  "Face for timestamps.")

(cl-defstruct chat-message
  id
  sender
  content
  timestamp
  node)

(define-derived-mode chat-mode special-mode "Chat"
  "Major mode for a simple EWOC-based chat interface."
  (setq buffer-read-only t)
  (setq-local chat-message-history nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq chat-ewoc (ewoc-create #'chat--format-message))
  ;; Init with existing history (oldest first)
  (dolist (msg (reverse chat-message-history))
    (ewoc-enter-last chat-ewoc msg))
  (chat--add-system-message "Welcome to Emacs Chat!"))

(defun chat--chat-buffer ()
  "Get or create the chat buffer."
  (get-buffer-create chat-buffer-name))

(defun chat--current-timestamp ()
  "Return a human-readable timestamp string."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun chat--generate-id ()
  "Generate a random ID (UUID-like)."
  (substring (md5 (format "%s%s" (random) (float-time))) 0 8))

(defvar-local chat--last-sender nil
  "Internal tracker for grouping messages visually.")

(defun chat--format-message (msg)
  "Format a single chat message for display in EWOC, grouped by sender."
  (let* ((sender (chat-message-sender msg))
         (content (chat-message-content msg))
         (timestamp (chat-message-timestamp msg))
         (same-sender-as-last (equal sender chat--last-sender))
         (icon (cond
                ((string= sender "[System]") "🛠️ ")
                ((string= sender "You") "💬 ")
                (t "✉️  ")))
         (sender-face (cond
                       ((string= sender "[System]") 'font-lock-comment-face)
                       ((string= sender "You") 'font-lock-keyword-face)
                       (t 'font-lock-variable-name-face)))
         (timestamp-face '(:foreground "gray60" :height 0.8 :slant italic)))

    ;; Remember for the next message
    (setq chat--last-sender sender)

    ;; Insert line with or without sender
    (if same-sender-as-last
        ;; Indented continuation
        (insert (propertize "            " 'face 'default)) ;; 12 spaces = icon + sender field
      (insert icon)
      (insert (propertize (format "%-10s" sender) 'face sender-face))
      (insert ": "))

    ;; Content
    (insert (propertize content 'face 'default))
    ;; Timestamp
    (let ((ts (propertize (format "  [%s]" timestamp) 'face timestamp-face)))
      (insert ts))
    (insert "\n")))

(defun chat--refresh-ewoc ()
  "Refresh the EWOC display and reset grouping state."
  (let ((inhibit-read-only t))
    (setq chat--last-sender nil)
    (ewoc-refresh chat-ewoc)))

(defun chat--add-message (sender content)
  "Add a message from SENDER with CONTENT, efficiently tracked and bounded."
  (let* ((msg (make-chat-message
               :id (chat--generate-id)
               :sender sender
               :content content
               :timestamp (chat--current-timestamp)))
         (node (ewoc-enter-last chat-ewoc msg)))
    (setf (chat-message-node msg) node)

    ;; Insert at front
    (setq chat-message-history (cons msg chat-message-history))
    (cl-incf chat-message-count)

    ;; Trim if over capacity
    (when (> chat-message-count chat-max-messages)
      ;; Drop the last cons cell (O(n) traversal to n-1)
      (let ((walker chat-message-history)
            (prev nil)
            (i 1))
        (while (< i chat-max-messages)
          (setq prev walker
                walker (cdr walker))
          (cl-incf i))
        (when (cdr walker)
          (let ((node-to-delete (chat-message-node (car (cdr walker)))))
            (chat--delete-message-node node-to-delete)))
        ;; Cut the list
        (setcdr walker nil)
        (setq chat-message-count chat-max-messages)))

    (goto-char (point-max))
    msg))

(defun chat--delete-message-node (node)
  "Efficiently delete a message from the EWOC using its NODE reference."
  (when node
    (let ((inhibit-read-only t))
      (ewoc-delete chat-ewoc node))))

(defun chat--add-system-message (text)
  "Add a system message."
  (chat--add-message "[System]" text))

(defun chat-send-message (text)
  "Send TEXT as the current user."
  (interactive)
  (unless (string-blank-p text)
    (chat--add-message "You" text)))

(defun chat-read-and-send-message ()
  "Prompt for a message in the minibuffer and send it."
  (interactive)
  (let ((input (read-from-minibuffer "Message: ")))
    (chat-send-message input)))

(defun chat--find-message-by-id (id)
  "Find the message struct with the given ID."
  (cl-find-if (lambda (msg)
                (string= (chat-message-id msg) id))
              chat-message-history))

(defun chat--find-ewoc-node-by-id (id)
  "Find the EWOC node corresponding to message with ID."
  (ewoc-locate chat-ewoc
               (lambda (msg)
                 (string= (chat-message-id msg) id))))

(defun chat-delete-message-by-id (id)
  "Delete the message with ID from the chat."
  (interactive "sMessage ID to delete: ")
  (let ((node (chat--find-ewoc-node-by-id id)))
    (if node
        (progn
          (setq chat-message-history
                (cl-remove-if (lambda (msg)
                                (string= (chat-message-id msg) id))
                              chat-message-history))
          (ewoc-delete chat-ewoc node)
          (message "Deleted message with id: %s" id))
      (message "Message not found: %s" id))))

(defun chat-edit-message-by-id (id)
  "Edit the content of the message with ID."
  (interactive "sMessage ID to edit: ")
  (let* ((msg (chat--find-message-by-id id))
         (node (chat--find-ewoc-node-by-id id)))
    (if (and msg node)
        (let ((new-content (read-from-minibuffer "New message: " (chat-message-content msg))))
          (setf (chat-message-content msg) new-content
                (chat-message-timestamp msg) (chat--current-timestamp))
          (ewoc-invalidate chat-ewoc node)
          (message "Edited message %s" id))
      (message "Message not found: %s" id))))

(defun chat-edit-message-at-point ()
  "Edit the message at point."
  (interactive)
  (let* ((node (ewoc-locate chat-ewoc))
         (msg (and node (ewoc-data node))))
    (if (and node msg)
        (let ((new-content (read-from-minibuffer "New message: " (chat-message-content msg))))
          (setf (chat-message-content msg) new-content
                (chat-message-timestamp msg) (chat--current-timestamp))
          (ewoc-invalidate chat-ewoc node)
          (chat--refresh-ewoc)
          (message "Edited message."))
      (message "No message at point."))))

(defun chat-delete-message-at-point ()
  "Delete the message at point."
  (interactive)
  (let* ((node (ewoc-locate chat-ewoc))
         (msg (and node (ewoc-data node))))
    (if (and node msg)
        (let ((id (chat-message-id msg)))
          (setq chat-message-history
                (cl-remove-if (lambda (m)
                                (string= (chat-message-id m) id))
                              chat-message-history))
          (let ((inhibit-read-only t))
            (ewoc-delete chat-ewoc node)
            (chat--refresh-ewoc))
          (message "Deleted message."))
      (message "No message at point."))))

(defun open-chat ()
  "Open the chat buffer."
  (interactive)
  (let ((buf (chat--chat-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'chat-mode)
        (chat-mode)))
    (pop-to-buffer buf)))

(defun chat-start ()
  "Open chat and immediately prompt for input."
  (interactive)
  (open-chat)
  (chat-read-and-send-message))

(define-key chat-mode-map (kbd "C-c m m") #'chat-read-and-send-message)
(define-key chat-mode-map (kbd "C-c m d") #'chat-delete-message-at-point)
(define-key chat-mode-map (kbd "C-c m e") #'chat-edit-message-at-point)

(provide 'libchat)
