(require 'ewoc)
(require 'subr-x) ;; for `string-blank-p` and `string-join`

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
  "Format a single chat message for display in EWOC."
  (let ((sender (chat-message-sender msg))
        (content (chat-message-content msg))
        (timestamp (chat-message-timestamp msg)))
    (insert (format "[%s] %s: %s\n" timestamp sender content))))

(defun chat--add-message (sender content)
  "Add a message from SENDER with CONTENT."
  (let ((msg (make-chat-message
              :id (chat--generate-id)
              :sender sender
              :content content
              :timestamp (chat--current-timestamp))))
    (setq chat-message-history (append chat-message-history (list msg)))
    (ewoc-enter-last chat-ewoc msg)))

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
