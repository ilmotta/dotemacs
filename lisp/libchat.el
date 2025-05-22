(defvar chat-buffer-name "*Chat*"
  "Name of the chat buffer.")

(defvar-local chat-message-history '()
  "List of chat messages in the current buffer.")

(define-derived-mode chat-mode special-mode "Chat"
  "Major mode for a simple chat interface."
  (setq buffer-read-only t)
  (setq-local chat-message-history '())
  (chat--insert-system-message "Welcome to Emacs Chat!"))

(defun chat--chat-buffer ()
  "Get or create the chat buffer."
  (get-buffer-create chat-buffer-name))

(defun chat--insert-message (msg)
  "Insert MSG into the chat buffer."
  (with-current-buffer (chat--chat-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert msg "\n"))))

(defun chat--insert-system-message (msg)
  "Insert a system MSG."
  (chat--insert-message (format "[System]: %s" msg)))

(defun chat-send-message (text)
  "Send TEXT to the chat. Abstracted for future network logic."
  (push text chat-message-history)
  (chat--insert-message (format "You: %s" text))
  ;; Placeholder for server/network call
  ;; (chat--send-to-server text)
  )

(defun chat-read-and-send-message ()
  "Prompt for a message in the minibuffer and send it."
  (interactive)
  (let ((input (read-from-minibuffer "Message: ")))
    (chat-send-message input)))

(defun chat--maybe-submit ()
  "Optional post-command hook for enhancements (e.g. Enter to send)."
  ;; Currently unused, placeholder for future enhancement
  )

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

;; Optional: bind key to keep sending messages

;; (define-key chat-mode-map (kbd "C-c m m") #'chat-read-and-send-message)
