(require 'ewoc)
(require 'subr-x)
(require 'cl-lib)

(defvar chat-buffer-name "*Chat*"
  "Name of the chat buffer.")

(defvar-local chat-ewoc nil
  "EWOC instance for chat messages.")

(defvar-local chat-message-history nil
  "List of chat messages stored as a data structure.")

(cl-defstruct chat-message
  id
  sender
  content
  timestamp)

(define-derived-mode chat-mode special-mode "Chat"
  "Major mode for a simple EWOC-based chat interface."
  (setq buffer-read-only t)
  (setq-local chat-message-history nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq chat-ewoc (ewoc-create #'chat--format-message))
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

(defun chat--format-message (msg)
  "Format a single chat message for display in EWOC with hover tooltip."
  (let* ((sender (chat-message-sender msg))
         (content (chat-message-content msg))
         (timestamp (chat-message-timestamp msg))
         (start (point)))
    (insert (format "%s: %s\n" sender content))
    (add-text-properties
     start (point)
     `(help-echo ,(format "Sent at: %s" timestamp)))))

(defun chat--add-message (sender content)
  "Add a message from SENDER with CONTENT."
  (let ((msg (make-chat-message
              :id (chat--generate-id)
              :sender sender
              :content content
              :timestamp (chat--current-timestamp))))
    (setq chat-message-history (append chat-message-history (list msg)))
    (ewoc-enter-last chat-ewoc msg)
    msg))

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
            (ewoc-delete chat-ewoc node))
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
