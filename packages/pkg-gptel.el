;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defvar pkg-gptel--backend-gemini nil)
(defvar pkg-gptel--backend-openai nil)

;; Fix `gptel-api-key-from-auth-source' by adding :max 1
;; (auth-source-search :host host :user user :max 1 :require '(:secret))

(lib-util/pkg gptel
  :elpaca (:host github
           :repo "karthink/gptel"
           :ref "b1c4cb13c690913aee64c1a5add4add939de86e8")
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

  ;; Default backend
  (setq gptel-backend pkg-gptel--backend-gemini))

(provide 'pkg-gptel)
