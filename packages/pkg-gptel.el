;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defvar pkg-gptel--backend-gemini nil)
(defvar pkg-gptel--backend-openai nil)
(defvar pkg-gptel--backend-deepseek-r1 nil)

;; Fix `gptel-api-key-from-auth-source' by adding :max 1
;; (auth-source-search :host host :user user :max 1 :require '(:secret))

;; transient version 0.4.1 lower than minimum
(when (>= emacs-major-version 30)
  (lib-util/pkg gptel
    :ensure (:host github
             :repo "karthink/gptel"
             :ref "aa62573ee7b14830ffa1cdb3709e588b8fa64f2e")
    :init
    (setq gptel-log-level nil) ; Can be 'debug
    (setq gptel-use-curl t)
    (setq gptel-model "gemini-pro")

    :config
    (setq pkg-gptel--backend-gemini
          (gptel-make-gemini "Gemini"
                             :key (lambda () (lib-util/read-password :host "api.gemini.com"))
                             :stream t))

    (setq pkg-gptel--backend-openai
          (gptel-make-openai "ChatGPT"
                             :key (lambda () (lib-util/read-password :host "api.openai.com"))
                             :stream t))

    (setq pkg-gptel--backend-deepseek-r1
          (gptel-make-ollama "DeepSeek R1"
                             :host "localhost:11434"
                             :stream t
                             :models '(deepseek-r1:8b)))

    (setq gptel-directives
          '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
            (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
            (writing . "You are a large language model and a writing assistant. Respond concisely.")
            (rephrasing . "You are an expert English writer. Rephrase the text to be grammatically correct while preserving the original writing style.")))

    ;; Default backend
    (setq gptel-backend pkg-gptel--backend-deepseek-r1)))

(provide 'pkg-gptel)
