;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg gptel
  :elpaca (:host github
           :repo "karthink/gptel"
           :ref "73ec10831bf3ad85384ead962878b2437928def4")
  (setq gptel-log-level nil) ; Can be 'debug
  (setq gptel-use-curl t)

  (setq gptel-model "gemini-pro")
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key (lambda () (lib-util/read-password :host "api.gemini.com"))
                        :stream t))

  ;; Failing with OpenAI.
  (comment
    (setq gptel-backend (gptel-make-openai "ChatGPT"
                          :key (lambda () (lib-util/read-password :host "api.openai.com"))
                          :stream t))))

(provide 'pkg-gptel)
