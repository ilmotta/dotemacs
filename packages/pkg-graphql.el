;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a set of generic functions for interacting with GraphQL web
;; services, such as a macro to build a valid query.

;;; Code:

(my/package graphql
  :elpaca (:ref "67237f284f2dfb94f3cfba672ff64a37e1cb860f"
           :fetcher github
           :repo "vermiculus/graphql.el"
           :files (:defaults))
  :defer t)

(provide 'pkg-graphql)
