((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
                    (:package "aggressive-indent" :repo
                     "Malabarba/aggressive-indent-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                       "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "f376cdc25de5c0f8c330f1e053557d95ca47a540"))
 (all-the-icons :source "elpaca-menu-lock-file" :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el"
                 :fetcher github :files (:defaults "data") :source "MELPA"
                 :protocol https :inherit t :depth treeless :ref
                 "51bf77da1ebc3c199dfc11f54c0dce67559f5f40"))
 (all-the-icons-dired :source "elpaca-menu-lock-file" :recipe
                      (:package "all-the-icons-dired" :repo
                       "wyuenho/all-the-icons-dired" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a"))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el"
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (anzu :source "elpaca-menu-lock-file" :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu" :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :source "MELPA" :protocol https :inherit t :depth treeless :ref
        "5abb37455ea44fa401d5f4c1bdc58adb2448db67"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia"
            :files (:defaults ("scripts" "scripts/formatters")) :source "MELPA"
            :protocol https :inherit t :depth treeless :ref
            "5ebd6bf5819fbf2adfa18162f270825e6ca4379c"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :source "MELPA" :protocol https :inherit t :depth treeless :ref
         "bb3f31966ed65a76abe6fa4f80a960a2917f554e"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
         "LICENSE" "README*" "*-pkg.el"))
       :source "MELPA" :protocol https :inherit t :depth treeless :ref
       "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-at-remote" :repo
                    "rmuslimov/browse-at-remote" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "d81643c975e77d506fe2eb931229739c162adb5d"))
 (bui :source "elpaca-menu-lock-file" :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
         "LICENSE" "README*" "*-pkg.el"))
       :source "MELPA" :protocol https :inherit t :depth treeless :ref
       "f3a137628e112a91910fd33c0cff0948fa58d470"))
 (cargo :source "elpaca-menu-lock-file" :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :source "MELPA" :protocol https :inherit t :depth treeless :ref
         "d2720c8dc7ac3b18ce112a886d3b8696797d01cb"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :source "MELPA" :protocol https :inherit t :depth treeless :ref
        "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (cider :source "elpaca-menu-lock-file" :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider" :files
         ("lisp/*.el" "bin/*.sh" "*.el" "clojure.sh" "lein.sh"
          (:exclude ".dir-locals.el"))
         :old-names (nrepl) :source "MELPA" :protocol https :inherit t :depth
         treeless :ref "c3acf55af0a8c03a2a050f66b328bba8d2402574"))
 (cider-storm :source "elpaca-menu-lock-file" :recipe
              (:source nil :protocol https :inherit t :depth treeless :host
               github :repo "jpmonettas/cider-storm" :ref
               "58b8a50406e5e6fbe2d8cdc969543246987386cd" :package "cider-storm"))
 (clj-refactor :source "elpaca-menu-lock-file" :recipe
               (:package "clj-refactor" :fetcher github :repo
                "clojure-emacs/clj-refactor.el" :files
                (:defaults "CHANGELOG.md") :source "MELPA" :protocol https
                :inherit t :depth treeless :ref
                "dc1bbc8cdaa723bdbb6669ea7d280625c370755d"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
               (:package "clojure-mode" :repo "clojure-emacs/clojure-mode"
                :fetcher github :files ("clojure-mode.el") :source "MELPA"
                :protocol https :inherit t :depth treeless :ref
                "eabe29b076fff19db552d36d4babaa36ecf0b704"))
 (command-log-mode :source "elpaca-menu-lock-file" :recipe
                   (:package "command-log-mode" :fetcher github :repo
                    "lewang/command-log-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "af600e6b4129c8115f464af576505ea8e789db27"))
 (commenter :source "elpaca-menu-lock-file" :recipe
            (:package "commenter" :fetcher github :repo "yuutayamada/commenter"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "6d1885419434ba779270c6fda0e30d390bb074bd"))
 (concurrent :source "elpaca-menu-lock-file" :recipe
             (:package "concurrent" :repo "kiwanami/emacs-deferred" :fetcher
              github :files ("concurrent.el") :source "MELPA" :protocol https
              :inherit t :depth treeless :ref
              "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :host
           github :ref "ce38dd037769ccba93e7b854ab9b0cc0eced84ee"))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
              (:package "consult-dir" :fetcher github :repo
               "karthink/consult-dir" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
         (:defaults "extensions/corfu-*.el") :fetcher github :source "MELPA"
         :protocol https :inherit t :depth treeless :host github :ref
         "05889f194fd044223a1ddc7cfb2f96c752df5559"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo "emacsmirror/csv-mode" :branch nil :files
            ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit
            t :depth treeless :host github :ref
            "849ce3e754f291c3643bc36ed802226606955c3f"))
 (ctable :source "elpaca-menu-lock-file" :recipe
         (:package "ctable" :repo "kiwanami/emacs-ctable" :fetcher github :files
          ("ctable.el") :source "MELPA" :protocol https :inherit t :depth
          treeless :ref "48b73742757a3ae5736d825fe49e00034cc453b5"))
 (ctrlf :source "elpaca-menu-lock-file" :recipe
        (:package "ctrlf" :fetcher github :repo "radian-software/ctrlf" :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :source "MELPA" :protocol https :inherit t :depth treeless :ref
         "9b4cf6c79a961f2bfbb949805aa300fcf1eb40a6"))
 (daemons :source "elpaca-menu-lock-file" :recipe
          (:package "daemons" :fetcher github :repo "cbowdon/daemons.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "e18e84ccc13101f1609c213029cf011ae0ad1178"))
 (dap-mode :source "elpaca-menu-lock-file" :recipe
           (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher github
            :files (:defaults "icons") :source "MELPA" :protocol https :inherit
            t :depth treeless :ref "39bfaf1a3400b3ca4e9755f4d15e33abb0dda2c4"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
        ("dash.el" "dash.texi") :source "MELPA" :protocol https :inherit t
        :depth treeless :ref "3df46d7d9fe74f52a661565888e4d31fd760f0df"))
 (deadgrep :source "elpaca-menu-lock-file" :recipe
           (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "9da7183e60c75bacefd44025fc5e5335b7c5862a"))
 (deferred :source "elpaca-menu-lock-file" :recipe
           (:package "deferred" :repo "kiwanami/emacs-deferred" :fetcher github
            :files ("deferred.el") :source "MELPA" :protocol https :inherit t
            :depth treeless :ref "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (devdocs-lookup :source "elpaca-menu-lock-file" :recipe
                 (:source nil :protocol https :inherit t :depth treeless :host
                  github :repo "skeeto/devdocs-lookup" :ref
                  "233b9a2bac3c86a7c3d403d85848273086b4c453" :package
                  "devdocs-lookup"))
 (dimmer :source "elpaca-menu-lock-file" :recipe
         (:package "dimmer" :fetcher github :repo "gonewest818/dimmer.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :source "MELPA" :protocol https :inherit t :depth treeless :ref
          "a5b697580e5aed6168b571ae3d925753428284f8"))
 (dired-du :source "elpaca-menu-lock-file" :recipe
           (:package "dired-du" :repo "emacsmirror/dired-du" :branch nil :files
            ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit
            t :depth treeless :host github :ref
            "f7e1593e94388b0dfb71af8e9a3d5d07edf5a159"))
 (diredfl :source "elpaca-menu-lock-file" :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "94bd99eeced6d52a5a7b9db3745239feafd633e2"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                   "spotify/dockerfile-mode" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless
                   :ref "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline"
                 :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "65d0bd83eb7c393092e032c24b882f3ba19b4899"))
 (drag-stuff :source "elpaca-menu-lock-file" :recipe
             (:package "drag-stuff" :repo "rejeep/drag-stuff.el" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "6d06d846cd37c052d79acd0f372c13006aa7e7c8"))
 (ef-themes :source "elpaca-menu-lock-file" :recipe
            (:package "ef-themes" :repo "protesilaos/ef-themes" :files
             ("*"
              (:exclude ".git" "COPYING" "doclicense.texi" "contrast-ratios.org"))
             :source "GNU ELPA" :protocol https :inherit t :depth treeless :host
             github :ref "96aaab126510bfd303558e2a9b85a745dab32f41"))
 (eglot :source "elpaca-menu-lock-file" :recipe
        (:package "eglot" :repo "joaotavora/eglot" :branch "master" :files
         ("lisp/progmodes/eglot.el" "doc/emacs/doclicense.texi"
          "doc/emacs/docstyle.texi" "doc/misc/eglot.texi" "etc/EGLOT-NEWS"
          (:exclude ".git"))
         :source "GNU ELPA" :protocol https :inherit t :depth treeless :host
         github :ref "8b5532dd32b25276c1857508030b207f765ef9b6"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github
              :files (:defaults (:exclude "elisp-refs-bench.el")) :source
              "MELPA" :protocol https :inherit t :depth treeless :ref
              "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit t :depth treeless :repo
    "https://github.com/progfolio/elpaca.git" :ref
    "45c56a968fd5903bbcbb1fafbf3b6e50e64c4393" :files
    (:defaults "elpaca-test.el" (:exclude "extensions")) :build
    (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                      "https://github.com/progfolio/elpaca.git" :files
                      ("extensions/elpaca-use-package.el") :main
                      "extensions/elpaca-use-package.el" :build
                      (:not elpaca--compile-info) :source "Elpaca extensions"
                      :protocol https :inherit t :depth treeless :ref
                      "45c56a968fd5903bbcbb1fafbf3b6e50e64c4393"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
          (:package "emacsql" :fetcher github :repo "skeeto/emacsql" :files
           (:defaults "README.md" "sqlite") :source "MELPA" :protocol https
           :inherit t :depth treeless :host github :ref
           "ced062890061b6e4fbe4d00c0617f7ff84fff25c"))
 (emacsql-sqlite :source "elpaca-menu-lock-file" :recipe
                 (:source nil :protocol https :inherit t :depth treeless :host
                  github :repo "skeeto/emacsql" :package "emacsql-sqlite" :ref
                  "ced062890061b6e4fbe4d00c0617f7ff84fff25c"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
          ("embark.el" "embark-org.el" "embark-consult.el") :source "MELPA"
          :protocol https :inherit t :depth treeless :ref
          "d2daad08e04090391b3221fa95000492a1f8aabe"))
 (emmet-mode :source "elpaca-menu-lock-file" :recipe
             (:package "emmet-mode" :fetcher github :repo "smihica/emmet-mode"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "63b6932603184956b5ea8919036d2b307b48d7fd"))
 (epc :source "elpaca-menu-lock-file" :recipe
      (:package "epc" :repo "kiwanami/emacs-epc" :fetcher github :files
       ("epc.el" "epcs.el") :source "MELPA" :protocol https :inherit t :depth
       treeless :ref "e1bfa5ca163273859336e3cc89b4b6460f7f8cda"))
 (eros :source "elpaca-menu-lock-file" :recipe
       (:package "eros" :fetcher github :repo "xiongtx/eros" :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :source "MELPA" :protocol https :inherit t :depth treeless :ref
        "dd8910279226259e100dab798b073a52f9b4233a"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
        (:defaults "doc/build/texinfo/evil.texi"
         (:exclude "evil-test-helpers.el"))
        :source "MELPA" :protocol https :inherit t :depth treeless :ref
        "0251080640e0da6f0eec2b7d8dd70e9c9b9915d7"))
 (evil-cleverparens :source "elpaca-menu-lock-file" :recipe
                    (:package "evil-cleverparens" :fetcher github :repo
                     "emacs-evil/evil-cleverparens" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                       "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless
                     :ref "22aa03d0f50aa70ae08fbe8765a88f5020afa635"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo
                   "emacs-evil/evil-collection" :files (:defaults "modes")
                   :source "MELPA" :protocol https :inherit t :depth treeless
                   :ref "b7a75062a600b1b1d2ba51a1e3ac1ec331d19fff"))
 (evil-matchit :source "elpaca-menu-lock-file" :recipe
               (:package "evil-matchit" :fetcher github :repo
                "redguardtoo/evil-matchit" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "ec3dd819983b2d824142efddd46ef29b46a7c454"))
 (evil-nerd-commenter :source "elpaca-menu-lock-file" :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                       "redguardtoo/evil-nerd-commenter" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "8c0f23d46a3927b9f83c1c2c4590be53d0b740db"))
 (evil-smartparens :source "elpaca-menu-lock-file" :recipe
                   (:package "evil-smartparens" :fetcher github :repo
                    "expez/evil-smartparens" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "026d4a3cfce415a4dfae1457f871b385386e61d3"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround"
                 :fetcher github :old-names (surround) :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "c9e1449bf3f740b5e9b99e7820df4eca7fc7cf02"))
 (explain-pause-mode :source "elpaca-menu-lock-file" :recipe
                     (:source nil :protocol https :inherit t :depth treeless
                      :host github :repo "lastquestion/explain-pause-mode" :ref
                      "2356c8c3639cbeeb9751744dbe737267849b4b51" :package
                      "explain-pause-mode"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
       "LICENSE" "README*" "*-pkg.el"))
     :source "MELPA" :protocol https :inherit t :depth treeless :ref
     "1e7020dc0d4c52d3da9bd610d431cab13aa02d8c"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "10430dee428f7bab176743097d996182fac29daa"))
 (flycheck-clj-kondo :source "elpaca-menu-lock-file" :recipe
                     (:package "flycheck-clj-kondo" :fetcher github :repo
                      "borkdude/flycheck-clj-kondo" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf"))
 (flycheck-ledger :source "elpaca-menu-lock-file" :recipe
                  (:package "flycheck-ledger" :fetcher github :repo
                   "purcell/flycheck-ledger" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless
                   :ref "628e25ba66604946085571652a94a54f4d1ad96f"))
 (flycheck-nimsuggest :source "elpaca-menu-lock-file" :recipe
                      (:package "flycheck-nimsuggest" :fetcher github :repo
                       "yuutayamada/flycheck-nimsuggest" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :ref "dc9a5de1cb3ee05db5794d824610959a1f603bc9"))
 (flycheck-rust :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-rust" :repo "flycheck/flycheck-rust"
                 :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "a139cd53c5062697e9ed94ad80b803c37d999600"))
 (geiser :source "elpaca-menu-lock-file" :recipe
         (:package "geiser" :fetcher gitlab :repo "emacs-geiser/geiser" :files
          ("elisp/*.el" "doc/dir" "doc/geiser.texi") :source "MELPA" :protocol
          https :inherit t :depth treeless :ref
          "bfc9cce54b7ac1cb036911965198b5cbe2f43f4c"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "74d4541781d3a01933dda298eb932c06e67413f9"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes"
             :old-names (gitattributes-mode gitconfig-mode gitignore-mode)
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "be96ef14fab6a2d76cca3ebf9a15b462a695923d"))
 (gnuplot :source "elpaca-menu-lock-file" :recipe
          (:package "gnuplot" :repo "emacs-gnuplot/gnuplot" :fetcher github
           :files ("gnuplot.el" "gnuplot-gui.el" "gnuplot-context.el") :source
           "MELPA" :protocol https :inherit t :depth treeless :ref
           "fe7ce76d797b34214178ac8e470f2fa9a63b2520"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
           ("go-mode.el") :source "MELPA" :protocol https :inherit t :depth
           treeless :ref "166dfb1e090233c4609a50c2ec9f57f113c1da72"))
 (golden-ratio :source "elpaca-menu-lock-file" :recipe
               (:package "golden-ratio" :repo "roman/golden-ratio.el" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "007911d8a431b72670f5fe5f0e5b4380c2777a31"))
 (gotest :source "elpaca-menu-lock-file" :recipe
         (:package "gotest" :fetcher github :repo "nlamirault/gotest.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :source "MELPA" :protocol https :inherit t :depth treeless :ref
          "2ec82dcc70d5f6aa22f66b44f8b537be33bd7903"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :source "MELPA" :protocol https :inherit t :depth treeless :host github
         :ref "aa62573ee7b14830ffa1cdb3709e588b8fa64f2e"))
 (graphql :source "elpaca-menu-lock-file" :recipe
          (:package "graphql" :fetcher github :repo "vermiculus/graphql.el"
           :files (:defaults) :source "MELPA" :protocol https :inherit t :depth
           treeless :ref "67237f284f2dfb94f3cfba672ff64a37e1cb860f"))
 (graphql-mode :source "elpaca-menu-lock-file" :recipe
               (:package "graphql-mode" :repo "davazp/graphql-mode" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "1437b790060f6ce4a8dc57df2023443645b899e5"))
 (groovy-mode :source "elpaca-menu-lock-file" :recipe
              (:package "groovy-mode" :fetcher github :repo
               "Groovy-Emacs-Modes/groovy-emacs-modes" :files ("*groovy*.el")
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "c612ac1e9f742856914ad6e8eb9e9dc169f489ab"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher
                github :files (:defaults "NEWS" "logo.svg") :source "MELPA"
                :protocol https :inherit t :depth treeless :ref
                "79eaf444a72109f93f552abb53f834cc63bbf9f2"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "94c25337b2de2f9da60914a7c0c6cca9584c0231"))
 (hide-mode-line :source "elpaca-menu-lock-file" :recipe
                 (:package "hide-mode-line" :repo
                  "hlissner/emacs-hide-mode-line" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless
                  :ref "bc5d293576c5e08c29e694078b96a5ed85631942"))
 (highlight-parentheses :source "elpaca-menu-lock-file" :recipe
                        (:package "highlight-parentheses" :fetcher sourcehut
                         :repo "tsdh/highlight-parentheses.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                           "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth
                         treeless :ref
                         "438a1cb2563e2a2496be4678cc0df8d5b22caf5d"))
 (hnreader :source "elpaca-menu-lock-file" :recipe
           (:package "hnreader" :fetcher github :repo "thanhvg/emacs-hnreader"
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "8444e177035e236e991f9ea73074c053a45426ad"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
        "LICENSE" "README*" "*-pkg.el"))
      :source "MELPA" :protocol https :inherit t :depth treeless :ref
      "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmlize :source "elpaca-menu-lock-file" :recipe
          (:package "htmlize" :fetcher github :repo "hniksic/emacs-htmlize"
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
         (:defaults (:exclude "lv.el")) :source "MELPA" :protocol https :inherit
         t :depth treeless :ref "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (idle-highlight-mode :source "elpaca-menu-lock-file" :recipe
                      (:package "idle-highlight-mode" :fetcher codeberg :repo
                       "ideasman42/emacs-idle-highlight-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth
                       treeless :host codeberg :ref
                       "f9091c907d41e7b12d99d108a194229b8dbfc5ae"))
 (inflections :source "elpaca-menu-lock-file" :recipe
              (:package "inflections" :repo "eschulte/jump.el" :fetcher github
               :files ("inflections.el") :source "MELPA" :protocol https
               :inherit t :depth treeless :ref
               "55caa66a7cc6e0b1a76143fd40eff38416928941"))
 (just-mode :source "elpaca-menu-lock-file" :recipe
            (:package "just-mode" :repo "leon-barrett/just-mode.el" :fetcher
             github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "45a221063093f3461816913acdaba898e62b42ce"))
 (kbd-mode :source "elpaca-menu-lock-file" :recipe
           (:source nil :protocol https :inherit t :depth treeless :host github
            :repo "kmonad/kbd-mode" :ref
            "96178a43d3c9ea3167362513fe4c3fdeb7074e9f" :package "kbd-mode"))
 (kotlin-mode :source "elpaca-menu-lock-file" :recipe
              (:package "kotlin-mode" :repo
               "Emacs-Kotlin-Mode-Maintainers/kotlin-mode" :fetcher github
               :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "55eed95033a59d7448a4b2bc11879e62c05e361b"))
 (ledger-mode :source "elpaca-menu-lock-file" :recipe
              (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode"
               :files ("ledger-*.el" "doc/*.texi") :old-names (ldg-mode) :source
               "MELPA" :protocol https :inherit t :depth treeless :ref
               "8bad528d43007e0310b5e72e6e021b502b30495c"))
 (lsp-docker :source "elpaca-menu-lock-file" :recipe
             (:package "lsp-docker" :repo "emacs-lsp/lsp-docker" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "ff41f4a76b640d39dc238bacba7f654c297827fa"))
 (lsp-java :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-java" :repo "emacs-lsp/lsp-java" :fetcher github
            :files (:defaults "icons") :source "MELPA" :protocol https :inherit
            t :depth treeless :ref "de2d89814fecb9bae825baa7028c5cd8b32b9b8f"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github
            :files (:defaults "clients/*.el") :source "MELPA" :protocol https
            :inherit t :depth treeless :ref
            "5b0198457e4e6570677b3e443cda0ba73d63343b"))
 (lsp-treemacs :source "elpaca-menu-lock-file" :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher
                github :files (:defaults "icons") :source "MELPA" :protocol
                https :inherit t :depth treeless :ref
                "3e5550f278db74f15ebe34add0138b138207ec08"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
          (:defaults "lsp-ui-doc.html" "resources") :source "MELPA" :protocol
          https :inherit t :depth treeless :ref
          "00f1fecdfb41c30428734cf27e492f26f46627fb"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
            (:defaults (:exclude "init-tryout.el")) :source "MELPA" :protocol
            https :inherit t :depth treeless :ref
            "ad639c62e38a110d8d822c4f914af3e20b40ccc4"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el")
      :source "MELPA" :protocol https :inherit t :depth treeless :ref
      "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
         ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md"
          "LICENSE" ".dir-locals.el" (:exclude "lisp/magit-section.el"))
         :source "MELPA" :protocol https :inherit t :depth treeless :host github
         :ref "4992c3d1f64e0e983692c7a61d47069f47380dbf"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit"
                 :files
                 ("lisp/magit-section.el" "docs/magit-section.texi"
                  "magit-section-pkg.el")
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "4992c3d1f64e0e983692c7a61d47069f47380dbf"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :host
              github :ref "a527fb03b76a2bce1e360c6e73a095e06922c3f3"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                 "jrblevin/markdown-mode" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "d95107f5b77d6c010e89259e05adfcd79a21f26a"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                    "magnars/multiple-cursors.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "89f1a8df9b1fc721b1672b4c7b6d3ab451e7e3ef"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el"
              :fetcher github :files (:defaults "data") :source "MELPA"
              :protocol https :inherit t :depth treeless :ref
              "d972dee349395ffae8fceae790d22fedc8fe08e8"))
 (nim-mode :source "elpaca-menu-lock-file" :recipe
           (:package "nim-mode" :fetcher github :repo "nim-lang/nim-mode"
            :old-names (nimrod-mode) :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "744e076f0bea1c5ddc49f92397d9aa98ffa7eff8"))
 (nix-mode :source "elpaca-menu-lock-file" :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
            (:defaults (:exclude "nix-company.el" "nix-mode-mmm.el")) :source
            "MELPA" :protocol https :inherit t :depth treeless :ref
            "54e5626829168e22126b233e079f04dff3c71b90"))
 (ob-async :source "elpaca-menu-lock-file" :recipe
           (:package "ob-async" :repo "astahlman/ob-async" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "9aac486073f5c356ada20e716571be33a350a982"))
 (openwith :source "elpaca-menu-lock-file" :recipe
           (:package "openwith" :fetcher github :repo "jpkotta/openwith" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "1dc89670822966fab6e656f6519fdd7f01e8301a"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :host
             github :ref "c7cb04499d94ee1c17affb29b1cfcd2a45116c97"))
 (org :source "elpaca-menu-lock-file" :recipe
      (:package "org" :repo ("https://git.sr.ht/~bzg/org-mode" . "org")
       :pre-build
       (progn
         (require 'elpaca-menu-org) (setq elpaca-menu-org-make-manual t)
         (elpaca-menu-org--build))
       :autoloads "org-loaddefs.el" :depth treeless :build
       (:not elpaca--generate-autoloads-async) :files
       (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")) :source "Org"
       :protocol https :inherit t :ref
       "233a0ced97366090c31ef94562879bb2f729b120"))
 (org-cliplink :source "elpaca-menu-lock-file" :recipe
               (:package "org-cliplink" :repo "rexim/org-cliplink" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "13e0940b65d22bec34e2de4bc8cba1412a7abfbc"))
 (org-download :source "elpaca-menu-lock-file" :recipe
               (:package "org-download" :repo "abo-abo/org-download" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
 (org-make-toc :source "elpaca-menu-lock-file" :recipe
               (:package "org-make-toc" :fetcher github :repo
                "alphapapa/org-make-toc" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :host
              github :ref "33f694c57113418c7e739a824ff57dc9e7eb413c"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
           (:package "org-roam" :fetcher github :repo "org-roam/org-roam" :files
            (:defaults "extensions/*") :source "MELPA" :protocol https :inherit
            t :depth treeless :host github :ref
            "046822b512ffecdee7d110f73dd3a511802ca590"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
                   (:package "page-break-lines" :fetcher github :repo
                    "purcell/page-break-lines" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless
                    :ref "79eca86e0634ac68af862e15c8a236c37f446dcd"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
           "https://mumble.net/~campbell/git/paredit.git" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :host
           github :repo "emacsmirror/paredit" :ref
           "009c95980e52cc4d736fa1404cf17c86fe97fd7d"))
 (paren-face :source "elpaca-menu-lock-file" :recipe
             (:package "paren-face" :repo "tarsius/paren-face" :fetcher github
              :old-names (parenface) :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "bf741a6038a2554abf98d31e658421c33f8bf7a4"))
 (parseclj :source "elpaca-menu-lock-file" :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "6af22372e0fe14df882dd300b22b12ba2d7e00b0"))
 (parseedn :source "elpaca-menu-lock-file" :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "3407e4530a367b6c2b857dae261cdbb67a440aaa"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools"
             :files (:defaults "README" ("build" "Makefile") ("build" "server"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "b8079e4ebc2936f9772657332d50936350a65825"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (php-mode :source "elpaca-menu-lock-file" :recipe
           (:package "php-mode" :repo "emacs-php/php-mode" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "d01cfc9cd51706e076bf7e5cbf0cfa7ee885efb4"))
 (plantuml-mode :source "elpaca-menu-lock-file" :recipe
                (:package "plantuml-mode" :fetcher github :repo
                 "skuro/plantuml-mode" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "ea45a13707abd2a70df183f1aec6447197fc9ccc"))
 (plz :source "elpaca-menu-lock-file"
   :recipe
   (:package "plz" :repo "alphapapa/plz.el" :files (:defaults) :source
    "GNU ELPA" :protocol https :inherit t :depth treeless :ref
    "b6072edeec1f0e2465d273db74a8f2f7726e6bce" :fetcher github))
 (popper :source "elpaca-menu-lock-file" :recipe
         (:package "popper" :fetcher github :repo "karthink/popper" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :source "MELPA" :protocol https :inherit t :depth treeless :ref
          "d7560f18350faaee8362aee16481268de3cc6457"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "aa88860a16e28a311f81e18f1d9ed2e7d9e33991"))
 (promise :source "elpaca-menu-lock-file" :recipe
          (:package "promise" :repo "chuntaro/emacs-promise" :fetcher github
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "cec51feb5f957e8febe6325335cf57dc2db6be30"))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
                (:package "protobuf-mode" :fetcher github :repo
                 "protocolbuffers/protobuf" :files ("editors/protobuf-mode.el")
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "44ac12471381a0331735c253b2d954fad3f8207c"))
 (pulsar :source "elpaca-menu-lock-file" :recipe
         (:package "pulsar" :repo "protesilaos/pulsar" :files
          ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source "GNU ELPA"
          :protocol https :inherit t :depth treeless :host github :ref
          "77416e6076af7ab3ccfb7a91915dd036ad968155"))
 (queue :source "elpaca-menu-lock-file" :recipe
        (:package "queue" :repo
         ("https://git.savannah.gnu.org/git/emacs/elpa.git" . "queue") :branch
         "externals/queue" :files ("*" (:exclude ".git")) :source "GNU ELPA"
         :protocol https :inherit t :depth treeless :ref
         "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (quickrun :source "elpaca-menu-lock-file" :recipe
           (:package "quickrun" :repo "emacsorphanage/quickrun" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "7a89313c07a21eae9cd69a1a98e2a134d559e04f"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
    ("request.el") :source "MELPA" :protocol https :inherit t :depth treeless
    :ref "fe567ec0222a1ba658866697a9e7fb6b63d71ff7"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "0431b10d2520918f3f250fdf4dc96e8d2eb7ea76"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
       "LICENSE" "README*" "*-pkg.el"))
     :source "MELPA" :protocol https :inherit t :depth treeless :ref
     "e957dcb0677da18b2bb60ad867db5df5c35b5616"))
 (sesman :source "elpaca-menu-lock-file" :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :source "MELPA" :protocol https :inherit t :depth treeless :ref
          "7bca68dbbab0af26a6a23be1ff5fa97f9a18e022"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
              (:package "shrink-path" :fetcher gitlab :repo
               "bennya/shrink-path.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens"
               :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "0a23136dd6b1f326419c5828f4197ecfd820b204"))
 (spacious-padding :source "elpaca-menu-lock-file" :recipe
                   (:package "spacious-padding" :repo
                    "protesilaos/spacious-padding" :files
                    ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
                    "GNU ELPA" :protocol https :inherit t :depth treeless :host
                    github :ref "a3151f3c99d6b3b2d4644da88546476b3d31f0fe"))
 (speed-type :source "elpaca-menu-lock-file" :recipe
             (:package "speed-type" :fetcher github :repo "dakra/speed-type"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "11a8bd33711711fb5e22d93ac2ed950e4a2e76fc"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
           ("https://github.com/Malabarba/spinner.el" . "spinner") :files
           ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
           :depth treeless :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (sudo-edit :source "elpaca-menu-lock-file" :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "74eb1e6986461baed9a9269566ff838530b4379b"))
 (synosaurus :source "elpaca-menu-lock-file" :recipe
             (:package "synosaurus" :repo "hpdeifel/synosaurus" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "14d34fc92a77c3a916b4d58400424c44ae99cd81"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist"
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode"
                  :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless
                  :ref "e67459fefc871fdbf20e27be8f85b98b10b97b1b"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :host
             github :ref "d6b52b6eb7655d538b410ae60f85fbaa4d58a608"))
 (trashed :source "elpaca-menu-lock-file" :recipe
          (:package "trashed" :repo "shingo256/trashed" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "ddf5830730544435a068f2dc9ac75a81ea69df1d"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo
            "Alexander-Miller/treemacs" :files
            (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
             "src/scripts/treemacs*.py" (:exclude "src/extra/*"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "abd27c86df6b7bf8bc7d4c2a86b4ea9e425084c7"))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
        "LICENSE" "README*" "*-pkg.el"))
      :source "MELPA" :protocol https :inherit t :depth treeless :ref
      "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (typescript-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "typescript-mode" :fetcher github :repo
                   "emacs-typescript/typescript.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless
                   :ref "4fcb4594819caf472ae42ea068a1c7795cf07f46"))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:source nil :protocol https :inherit t :depth treeless :host
                github :repo "jdtsmith/ultra-scroll" :rev
                "2c517bf9b61bf432f706ff8a585ba453c7476be2" :package
                "ultra-scroll" :ref "b447044b9de68068139da9ab367126e973a6a0b0"))
 (undo-fu :source "elpaca-menu-lock-file" :recipe
          (:package "undo-fu" :fetcher codeberg :repo "emacsmirror/undo-fu"
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :host
           github :ref "0e22308de8337a9291ddd589edae167d458fbe77"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
           (:defaults "extensions/*.el") :fetcher github :source "MELPA"
           :protocol https :inherit t :depth treeless :host github :ref
           "ac82acf177a0dfc97deac8626a8a98c06bffc96c"))
 (vimrc-mode :source "elpaca-menu-lock-file" :recipe
             (:package "vimrc-mode" :fetcher github :repo "mcandre/vimrc-mode"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "13bc150a870d5d4a95f1111e4740e2b22813c30e"))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
                     (:package "visual-fill-column" :fetcher codeberg :repo
                      "joostkremers/visual-fill-column" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless
                      :ref "453d698d7fc243a547665f8ba43c55eee574e0db"))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
         ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c"
          "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h")
         :source "MELPA" :protocol https :inherit t :depth treeless :ref
         "99c1f5e76ac927825775c887c93e46036fac7327"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :source "MELPA" :protocol https :inherit t :depth treeless :ref
            "53bed1e6a8554da877c27ffad6bd65113dc758e3"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep"
         :files ("wgrep.el") :source "MELPA" :protocol https :inherit t :depth
         treeless :ref "f9687c28bbc2e84f87a479b6ce04407bb97cfb23"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "MELPA" :protocol https :inherit t :depth treeless :ref
    "f32cd7b09d518b629bfaa3eeb92b539891c6b9bc"))
 (wordnut :source "elpaca-menu-lock-file" :recipe
          (:package "wordnut" :repo "gromnitsky/wordnut" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :ref
           "feac531404041855312c1a046bde7ea18c674915"))
 (xclip :source "elpaca-menu-lock-file" :recipe
        (:package "xclip" :repo "emacsmirror/xclip" :branch nil :files
         ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
         :depth treeless :host github :ref
         "7febe164de2a881b83b9d604d3c7cf20b69f422d"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher
               github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "1a4012854c69a5cdaeb5a73d2ad705011892fca3"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :source "MELPA" :protocol https :inherit t :depth treeless :ref
        "f99ef76c80e6fc3fcf650c4fe34e10726594a4c4"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github
             :files ("yasnippet.el" "snippets") :source "MELPA" :protocol https
             :inherit t :depth treeless :ref
             "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (zones :source "elpaca-menu-lock-file" :recipe
        (:package "zones" :repo "emacsmirror/zones" :branch nil :files
         ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
         :depth treeless :host github :ref
         "f7e1b641ee8360ddb8bae1711d3978863efa692d")))
