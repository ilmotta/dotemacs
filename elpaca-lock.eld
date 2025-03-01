((elpaca :source
   "lockfile" :date (26562 54256 981742 181000) :recipe
   (:protocol https :inherit t :depth 1 :repo
    "https://github.com/progfolio/elpaca.git" :ref
    "798351f21bf91c96dea5abaf274e0f9946024fc8" :files
    (:defaults "elpaca-test.el" (:exclude "extensions")) :build
    (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "lockfile" :date (26562 54256 972934 347000)
                     :recipe
                     (:package "elpaca-use-package" :repo
                      "https://github.com/progfolio/elpaca.git" :files
                      ("extensions/elpaca-use-package.el") :main
                      "extensions/elpaca-use-package.el" :build
                      (:not elpaca--compile-info) :protocol https :inherit t
                      :depth 1 :ref "798351f21bf91c96dea5abaf274e0f9946024fc8"))
 (general :source "lockfile" :date (26562 54256 963666 806000) :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f"))
 (org :source "lockfile" :date (26562 54256 954536 980000) :recipe
      (:package "org" :local-repo "org" :repo
       "https://git.savannah.gnu.org/git/emacs/org-mode.git" :pre-build
       (progn (require 'elpaca-menu-org) (elpaca-menu-org--build)) :build
       (:not elpaca--generate-autoloads-async) :files
       (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")) :protocol https
       :inherit t :depth 1 :ref "233a0ced97366090c31ef94562879bb2f729b120"))
 (evil :source "lockfile" :date (26562 54256 946775 27000) :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
        (:defaults "doc/build/texinfo/evil.texi"
         (:exclude "evil-test-helpers.el"))
        :protocol https :inherit t :depth 1 :ref
        "0251080640e0da6f0eec2b7d8dd70e9c9b9915d7"))
 (evil-collection :source "lockfile" :date (26562 54256 937410 215000) :recipe
                  (:package "evil-collection" :fetcher github :repo
                   "emacs-evil/evil-collection" :files (:defaults "modes")
                   :protocol https :inherit t :depth 1 :ref
                   "b7a75062a600b1b1d2ba51a1e3ac1ec331d19fff"))
 (magit :source "lockfile" :date (26562 54256 929464 859000) :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
         ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md"
          "LICENSE" ".dir-locals.el" (:exclude "lisp/magit-section.el"))
         :protocol https :inherit t :depth 1 :host github :ref
         "461d5ba249b8178ae83f80596da4312e65010720"))
 (undo-fu :source "lockfile" :date (26562 54256 920062 275000) :recipe
          (:package "undo-fu" :fetcher codeberg :repo "emacsmirror/undo-fu"
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :host github :ref
           "0e22308de8337a9291ddd589edae167d458fbe77"))
 (transient :source "lockfile" :date (26562 54256 910491 272000) :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :host github :ref
             "d6b52b6eb7655d538b410ae60f85fbaa4d58a608"))
 (dash :source "lockfile" :date (26562 54256 902591 399000) :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
        ("dash.el" "dash.texi") :protocol https :inherit t :depth 1 :ref
        "3df46d7d9fe74f52a661565888e4d31fd760f0df"))
 (f :source "lockfile" :date (26562 54256 894013 324000) :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
       "LICENSE" "README*" "*-pkg.el"))
     :protocol https :inherit t :depth 1 :ref
     "1e7020dc0d4c52d3da9bd610d431cab13aa02d8c"))
 (plz :source "lockfile"
   :date (26562 54256 885765 21000) :recipe
   (:package "plz" :host github :files (:defaults) :repo "alphapapa/plz.el"
    :protocol https :inherit t :depth 1 :ref
    "b6072edeec1f0e2465d273db74a8f2f7726e6bce" :fetcher github))
 (posframe :source "lockfile" :date (26562 54256 877642 714000) :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "aa88860a16e28a311f81e18f1d9ed2e7d9e33991"))
 (promise :source "lockfile" :date (26562 54256 869914 726000) :recipe
          (:package "promise" :repo "chuntaro/emacs-promise" :fetcher github
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "cec51feb5f957e8febe6325335cf57dc2db6be30"))
 (request :source "lockfile"
   :date (26562 54256 861886 680000) :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
    ("request.el") :protocol https :inherit t :depth 1 :ref
    "fe567ec0222a1ba658866697a9e7fb6b63d71ff7"))
 (s :source "lockfile" :date (26562 54256 853662 251000) :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
       "LICENSE" "README*" "*-pkg.el"))
     :protocol https :inherit t :depth 1 :ref
     "e957dcb0677da18b2bb60ad867db5df5c35b5616"))
 (graphql :source "lockfile" :date (26562 54256 844243 945000) :recipe
          (:package "graphql" :fetcher github :repo "vermiculus/graphql.el"
           :files (:defaults) :protocol https :inherit t :depth 1 :ref
           "67237f284f2dfb94f3cfba672ff64a37e1cb860f"))
 (shrink-path :source "lockfile" :date (26562 54256 834993 20000) :recipe
              (:package "shrink-path" :fetcher gitlab :repo
               "bennya/shrink-path.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref
               "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (websocket :source "lockfile" :date (26562 54256 825979 261000) :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (ts :source "lockfile" :date (26562 54256 816087 216000) :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
        "LICENSE" "README*" "*-pkg.el"))
      :protocol https :inherit t :depth 1 :ref
      "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (consult :source "lockfile" :date (26562 54256 806668 24000) :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :host github :ref
           "07ea5421fbcb65b2207a43a0941124a95a47abc7"))
 (corfu :source "lockfile" :date (26562 54256 798165 764000) :recipe
        (:package "corfu" :repo "minad/corfu" :files
         (:defaults "extensions/corfu-*.el") :fetcher github :protocol https
         :inherit t :depth 1 :host github :ref
         "05889f194fd044223a1ddc7cfb2f96c752df5559"))
 (embark :source "lockfile" :date (26562 54256 788633 717000) :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
          ("embark.el" "embark-org.el" "embark-consult.el") :protocol https
          :inherit t :depth 1 :ref "d2daad08e04090391b3221fa95000492a1f8aabe"))
 (orderless :source "lockfile" :date (26562 54256 780201 236000) :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :host github :ref
             "96b74d2450ab4ab1a175d0e86c62f6695c4709b5"))
 (vertico :source "lockfile" :date (26562 54256 771901 30000) :recipe
          (:package "vertico" :repo "minad/vertico" :files
           (:defaults "extensions/*.el") :fetcher github :protocol https
           :inherit t :depth 1 :host github :ref
           "017ff44443bad401097b9987849625cafa348d86"))
 (all-the-icons :source "lockfile" :date (26562 54256 762771 690000) :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el"
                 :fetcher github :files (:defaults "data") :protocol https
                 :inherit t :depth 1 :ref
                 "51bf77da1ebc3c199dfc11f54c0dce67559f5f40"))
 (all-the-icons-dired :source "lockfile" :date (26562 54256 753140 423000)
                      :recipe
                      (:package "all-the-icons-dired" :repo
                       "wyuenho/all-the-icons-dired" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref
                       "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a"))
 (anzu :source "lockfile" :date (26562 54256 743601 230000) :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu" :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :inherit t :depth 1 :ref
        "5abb37455ea44fa401d5f4c1bdc58adb2448db67"))
 (ctrlf :source "lockfile" :date (26562 54256 735447 189000) :recipe
        (:package "ctrlf" :fetcher github :repo "radian-software/ctrlf" :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :inherit t :depth 1 :ref
         "9b4cf6c79a961f2bfbb949805aa300fcf1eb40a6"))
 (dimmer :source "lockfile" :date (26562 54256 725912 743000) :recipe
         (:package "dimmer" :fetcher github :repo "gonewest818/dimmer.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :inherit t :depth 1 :ref
          "a5b697580e5aed6168b571ae3d925753428284f8"))
 (diredfl :source "lockfile" :date (26562 54256 716658 130000) :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "94bd99eeced6d52a5a7b9db3745239feafd633e2"))
 (doom-modeline :source "lockfile" :date (26562 54256 707658 501000) :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline"
                 :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref
                 "65d0bd83eb7c393092e032c24b882f3ba19b4899"))
 (ef-themes :source "lockfile" :date (26562 54256 698515 672000) :recipe
            (:package "ef-themes" :host github :files ("*" (:exclude ".git"))
             :repo "protesilaos/ef-themes" :protocol https :inherit t :depth 1
             :ref "96aaab126510bfd303558e2a9b85a745dab32f41"))
 (eros :source "lockfile" :date (26562 54256 690853 552000) :recipe
       (:package "eros" :fetcher github :repo "xiongtx/eros" :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :inherit t :depth 1 :ref
        "dd8910279226259e100dab798b073a52f9b4233a"))
 (helpful :source "lockfile" :date (26562 54256 682861 984000) :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "94c25337b2de2f9da60914a7c0c6cca9584c0231"))
 (hide-mode-line :source "lockfile" :date (26562 54256 675598 202000) :recipe
                 (:package "hide-mode-line" :repo
                  "hlissner/emacs-hide-mode-line" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref
                  "bc5d293576c5e08c29e694078b96a5ed85631942"))
 (highlight-parentheses :source "lockfile" :date (26562 54256 667647 591000)
                        :recipe
                        (:package "highlight-parentheses" :fetcher sourcehut
                         :repo "tsdh/highlight-parentheses.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                           "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref
                         "438a1cb2563e2a2496be4678cc0df8d5b22caf5d"))
 (idle-highlight-mode :source "lockfile" :date (26562 54256 659761 222000)
                      :recipe
                      (:package "idle-highlight-mode" :fetcher codeberg :repo
                       "ideasman42/emacs-idle-highlight-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :host codeberg :ref
                       "f9091c907d41e7b12d99d108a194229b8dbfc5ae"))
 (marginalia :source "lockfile" :date (26562 54256 650658 517000) :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :host github :ref
              "c1365bf0c7b5d32e7531fa8f1a9a3b64a155cec0"))
 (page-break-lines :source "lockfile" :date (26562 54256 642761 858000) :recipe
                   (:package "page-break-lines" :fetcher github :repo
                    "purcell/page-break-lines" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref
                    "79eca86e0634ac68af862e15c8a236c37f446dcd"))
 (paren-face :source "lockfile" :date (26562 54256 633991 587000) :recipe
             (:package "paren-face" :repo "tarsius/paren-face" :fetcher github
              :old-names (parenface) :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "bf741a6038a2554abf98d31e658421c33f8bf7a4"))
 (pulsar :source "lockfile" :date (26562 54256 624417 518000) :recipe
         (:package "pulsar" :host github :files ("*" (:exclude ".git")) :repo
          "protesilaos/pulsar" :protocol https :inherit t :depth 1 :ref
          "57010e2c6cdee14acfd87b4c2bd75c796f04a75e"))
 (spacious-padding :source "lockfile" :date (26562 54256 614633 37000) :recipe
                   (:package "spacious-padding" :host github :files
                    ("*" (:exclude ".git")) :repo "protesilaos/spacious-padding"
                    :protocol https :inherit t :depth 1 :ref
                    "a3151f3c99d6b3b2d4644da88546476b3d31f0fe"))
 (ultra-scroll :source "lockfile" :date (26562 54256 606638 193000) :recipe
               (:protocol https :inherit t :depth 1 :host github :repo
                "jdtsmith/ultra-scroll" :rev
                "2c517bf9b61bf432f706ff8a585ba453c7476be2" :package
                "ultra-scroll" :ref "2c517bf9b61bf432f706ff8a585ba453c7476be2"))
 (xterm-color :source "lockfile" :date (26562 54256 598468 744000) :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher
               github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref
               "1a4012854c69a5cdaeb5a73d2ad705011892fca3"))
 (aggressive-indent :source "lockfile" :date (26562 54256 590632 858000) :recipe
                    (:package "aggressive-indent" :repo
                     "Malabarba/aggressive-indent-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                       "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref
                     "f376cdc25de5c0f8c330f1e053557d95ca47a540"))
 (apheleia :source "lockfile" :date (26562 54256 582597 926000) :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia"
            :files (:defaults ("scripts" "scripts/formatters")) :protocol https
            :inherit t :depth 1 :ref "5ebd6bf5819fbf2adfa18162f270825e6ca4379c"))
 (drag-stuff :source "lockfile" :date (26562 54256 574844 770000) :recipe
             (:package "drag-stuff" :repo "rejeep/drag-stuff.el" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "6d06d846cd37c052d79acd0f372c13006aa7e7c8"))
 (emmet-mode :source "lockfile" :date (26562 54256 566590 854000) :recipe
             (:package "emmet-mode" :fetcher github :repo "smihica/emmet-mode"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "63b6932603184956b5ea8919036d2b307b48d7fd"))
 (evil-cleverparens :source "lockfile" :date (26562 54256 557098 507000) :recipe
                    (:package "evil-cleverparens" :fetcher github :repo
                     "emacs-evil/evil-cleverparens" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                       "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref
                     "22aa03d0f50aa70ae08fbe8765a88f5020afa635"))
 (evil-matchit :source "lockfile" :date (26562 54256 547969 612000) :recipe
               (:package "evil-matchit" :fetcher github :repo
                "redguardtoo/evil-matchit" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref
                "ec3dd819983b2d824142efddd46ef29b46a7c454"))
 (evil-nerd-commenter :source "lockfile" :date (26562 54256 539769 712000)
                      :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                       "redguardtoo/evil-nerd-commenter" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref
                       "8c0f23d46a3927b9f83c1c2c4590be53d0b740db"))
 (evil-smartparens :source "lockfile" :date (26562 54256 530322 120000) :recipe
                   (:package "evil-smartparens" :fetcher github :repo
                    "expez/evil-smartparens" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref
                    "026d4a3cfce415a4dfae1457f871b385386e61d3"))
 (evil-surround :source "lockfile" :date (26562 54256 522773 425000) :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround"
                 :fetcher github :old-names (surround) :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref
                 "c9e1449bf3f740b5e9b99e7820df4eca7fc7cf02"))
 (flycheck :source "lockfile" :date (26562 54256 513595 917000) :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "10430dee428f7bab176743097d996182fac29daa"))
 (gptel :source "lockfile" :date (26562 54256 505174 441000) :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :inherit t :depth 1 :host github :ref
         "b1c4cb13c690913aee64c1a5add4add939de86e8"))
 (quickrun :source "lockfile" :date (26562 54256 495382 949000) :recipe
           (:package "quickrun" :repo "emacsorphanage/quickrun" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "7a89313c07a21eae9cd69a1a98e2a134d559e04f"))
 (paredit :source "lockfile" :date (26562 54256 487519 180000) :recipe
          (:package "paredit" :fetcher git :url
           "https://mumble.net/~campbell/git/paredit.git" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :host github :repo
           "emacsmirror/paredit" :ref "009c95980e52cc4d736fa1404cf17c86fe97fd7d"))
 (smartparens :source "lockfile" :date (26562 54256 479787 730000) :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens"
               :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref
               "0a23136dd6b1f326419c5828f4197ecfd820b204"))
 (cargo :source "lockfile" :date (26562 54256 471155 275000) :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :inherit t :depth 1 :ref
         "d2720c8dc7ac3b18ce112a886d3b8696797d01cb"))
 (cider :source "lockfile" :date (26562 54256 462757 943000) :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider" :files
         ("lisp/*.el" "bin/*.sh" "*.el" "clojure.sh" "lein.sh"
          (:exclude ".dir-locals.el"))
         :old-names (nrepl) :protocol https :inherit t :depth 1 :ref
         "c3acf55af0a8c03a2a050f66b328bba8d2402574"))
 (clj-refactor :source "lockfile" :date (26562 54256 453547 425000) :recipe
               (:package "clj-refactor" :fetcher github :repo
                "clojure-emacs/clj-refactor.el" :files
                (:defaults "CHANGELOG.md") :protocol https :inherit t :depth 1
                :ref "dc1bbc8cdaa723bdbb6669ea7d280625c370755d"))
 (clojure-mode :source "lockfile" :date (26562 54256 445797 742000) :recipe
               (:package "clojure-mode" :repo "clojure-emacs/clojure-mode"
                :fetcher github :files ("clojure-mode.el") :protocol https
                :inherit t :depth 1 :ref
                "25d713a67d8e0209ee74bfc0153fdf677697b43f"))
 (csv-mode :source "lockfile" :date (26562 54256 437829 234000) :recipe
           (:package "csv-mode" :host github :files ("*" (:exclude ".git"))
            :repo "emacsmirror/csv-mode" :protocol https :inherit t :depth 1
            :ref "58d1b74e5ecdff748f314bf701f5048ad35984b3" :branch nil))
 (dockerfile-mode :source "lockfile" :date (26562 54256 429887 211000) :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                   "spotify/dockerfile-mode" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref
                   "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c"))
 (flycheck-clj-kondo :source "lockfile" :date (26562 54256 421510 528000)
                     :recipe
                     (:package "flycheck-clj-kondo" :fetcher github :repo
                      "borkdude/flycheck-clj-kondo" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref
                      "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf"))
 (flycheck-ledger :source "lockfile" :date (26562 54256 413443 618000) :recipe
                  (:package "flycheck-ledger" :fetcher github :repo
                   "purcell/flycheck-ledger" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref
                   "628e25ba66604946085571652a94a54f4d1ad96f"))
 (flycheck-rust :source "lockfile" :date (26562 54256 403930 75000) :recipe
                (:package "flycheck-rust" :repo "flycheck/flycheck-rust"
                 :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref
                 "a139cd53c5062697e9ed94ad80b803c37d999600"))
 (geiser :source "lockfile" :date (26562 54256 395058 14000) :recipe
         (:package "geiser" :fetcher gitlab :repo "emacs-geiser/geiser" :files
          ("elisp/*.el" "doc/dir" "doc/geiser.texi") :protocol https :inherit t
          :depth 1 :ref "bfc9cce54b7ac1cb036911965198b5cbe2f43f4c"))
 (git-modes :source "lockfile" :date (26562 54256 385627 473000) :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes"
             :old-names (gitattributes-mode gitconfig-mode gitignore-mode)
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "be96ef14fab6a2d76cca3ebf9a15b462a695923d"))
 (gnuplot :source "lockfile" :date (26562 54256 377512 103000) :recipe
          (:package "gnuplot" :repo "emacs-gnuplot/gnuplot" :fetcher github
           :files ("gnuplot.el" "gnuplot-gui.el" "gnuplot-context.el") :protocol
           https :inherit t :depth 1 :ref
           "fe7ce76d797b34214178ac8e470f2fa9a63b2520"))
 (go-mode :source "lockfile" :date (26562 54256 368402 75000) :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
           ("go-mode.el") :protocol https :inherit t :depth 1 :ref
           "166dfb1e090233c4609a50c2ec9f57f113c1da72"))
 (gotest :source "lockfile" :date (26562 54256 359997 186000) :recipe
         (:package "gotest" :fetcher github :repo "nlamirault/gotest.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :inherit t :depth 1 :ref
          "2ec82dcc70d5f6aa22f66b44f8b537be33bd7903"))
 (graphql-mode :source "lockfile" :date (26562 54256 350039 85000) :recipe
               (:package "graphql-mode" :repo "davazp/graphql-mode" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref
                "1437b790060f6ce4a8dc57df2023443645b899e5"))
 (groovy-mode :source "lockfile" :date (26562 54256 341533 459000) :recipe
              (:package "groovy-mode" :fetcher github :repo
               "Groovy-Emacs-Modes/groovy-emacs-modes" :files ("*groovy*.el")
               :protocol https :inherit t :depth 1 :ref
               "c612ac1e9f742856914ad6e8eb9e9dc169f489ab"))
 (haskell-mode :source "lockfile" :date (26562 54256 333843 222000) :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher
                github :files (:defaults "NEWS" "logo.svg") :protocol https
                :inherit t :depth 1 :ref
                "79eaf444a72109f93f552abb53f834cc63bbf9f2"))
 (just-mode :source "lockfile" :date (26562 54256 324160 676000) :recipe
            (:package "just-mode" :repo "leon-barrett/just-mode.el" :fetcher
             github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "45a221063093f3461816913acdaba898e62b42ce"))
 (kbd-mode :source "lockfile" :date (26562 54256 316476 725000) :recipe
           (:protocol https :inherit t :depth 1 :host github :repo
            "kmonad/kbd-mode" :ref "96178a43d3c9ea3167362513fe4c3fdeb7074e9f"
            :package "kbd-mode"))
 (kotlin-mode :source "lockfile" :date (26562 54256 308591 877000) :recipe
              (:package "kotlin-mode" :repo
               "Emacs-Kotlin-Mode-Maintainers/kotlin-mode" :fetcher github
               :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref
               "55eed95033a59d7448a4b2bc11879e62c05e361b"))
 (ledger-mode :source "lockfile" :date (26562 54256 300978 305000) :recipe
              (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode"
               :files ("ledger-*.el" "doc/*.texi") :old-names (ldg-mode)
               :protocol https :inherit t :depth 1 :ref
               "8bad528d43007e0310b5e72e6e021b502b30495c"))
 (lua-mode :source "lockfile" :date (26562 54256 292647 536000) :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
            (:defaults (:exclude "init-tryout.el")) :protocol https :inherit t
            :depth 1 :ref "ad639c62e38a110d8d822c4f914af3e20b40ccc4"))
 (markdown-mode :source "lockfile" :date (26562 54256 284644 982000) :recipe
                (:package "markdown-mode" :fetcher github :repo
                 "jrblevin/markdown-mode" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref
                 "d95107f5b77d6c010e89259e05adfcd79a21f26a"))
 (nim-mode :source "lockfile" :date (26562 54256 275763 468000) :recipe
           (:package "nim-mode" :fetcher github :repo "nim-lang/nim-mode"
            :old-names (nimrod-mode) :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "744e076f0bea1c5ddc49f92397d9aa98ffa7eff8"))
 (nix-mode :source "lockfile" :date (26562 54256 267996 720000) :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
            (:defaults (:exclude "nix-company.el" "nix-mode-mmm.el")) :protocol
            https :inherit t :depth 1 :ref
            "54e5626829168e22126b233e079f04dff3c71b90"))
 (php-mode :source "lockfile" :date (26562 54256 259594 314000) :recipe
           (:package "php-mode" :repo "emacs-php/php-mode" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "d01cfc9cd51706e076bf7e5cbf0cfa7ee885efb4"))
 (plantuml-mode :source "lockfile" :date (26562 54256 251643 342000) :recipe
                (:package "plantuml-mode" :fetcher github :repo
                 "skuro/plantuml-mode" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref
                 "ea45a13707abd2a70df183f1aec6447197fc9ccc"))
 (protobuf-mode :source "lockfile" :date (26562 54256 244595 304000) :recipe
                (:package "protobuf-mode" :fetcher github :repo
                 "protocolbuffers/protobuf" :files ("editors/protobuf-mode.el")
                 :protocol https :inherit t :depth 1 :ref
                 "44ac12471381a0331735c253b2d954fad3f8207c"))
 (rust-mode :source "lockfile" :date (26562 54256 235978 860000) :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "0431b10d2520918f3f250fdf4dc96e8d2eb7ea76"))
 (terraform-mode :source "lockfile" :date (26562 54256 228147 463000) :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode"
                  :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref
                  "e67459fefc871fdbf20e27be8f85b98b10b97b1b"))
 (typescript-mode :source "lockfile" :date (26562 54256 219585 73000) :recipe
                  (:package "typescript-mode" :fetcher github :repo
                   "emacs-typescript/typescript.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref
                   "4fcb4594819caf472ae42ea068a1c7795cf07f46"))
 (vimrc-mode :source "lockfile" :date (26562 54256 212092 743000) :recipe
             (:package "vimrc-mode" :fetcher github :repo "mcandre/vimrc-mode"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "13bc150a870d5d4a95f1111e4740e2b22813c30e"))
 (web-mode :source "lockfile" :date (26562 54256 204490 793000) :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "53bed1e6a8554da877c27ffad6bd65113dc758e3"))
 (yaml-mode :source "lockfile" :date (26562 54256 195786 194000) :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7"))
 (dap-mode :source "lockfile" :date (26562 54256 187743 834000) :recipe
           (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher github
            :files (:defaults "icons") :protocol https :inherit t :depth 1 :ref
            "39bfaf1a3400b3ca4e9755f4d15e33abb0dda2c4"))
 (eglot :source "lockfile" :date (26562 54256 178602 758000) :recipe
        (:package "eglot" :host github :files ("*" (:exclude ".git")) :repo
         "joaotavora/eglot" :protocol https :inherit t :depth 1 :ref
         "8b5532dd32b25276c1857508030b207f765ef9b6"))
 (lsp-java :source "lockfile" :date (26562 54256 170654 884000) :recipe
           (:package "lsp-java" :repo "emacs-lsp/lsp-java" :fetcher github
            :files (:defaults "icons") :protocol https :inherit t :depth 1 :ref
            "de2d89814fecb9bae825baa7028c5cd8b32b9b8f"))
 (lsp-mode :source "lockfile" :date (26562 54256 162850 918000) :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github
            :files (:defaults "clients/*.el") :protocol https :inherit t :depth
            1 :ref "5b0198457e4e6570677b3e443cda0ba73d63343b"))
 (lsp-ui :source "lockfile" :date (26562 54256 154757 608000) :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
          (:defaults "lsp-ui-doc.html" "resources") :protocol https :inherit t
          :depth 1 :ref "00f1fecdfb41c30428734cf27e492f26f46627fb"))
 (htmlize :source "lockfile" :date (26562 54256 146384 342000) :recipe
          (:package "htmlize" :fetcher github :repo "hniksic/emacs-htmlize"
           :version-regexp "release/\\(.*\\)" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9"))
 (ob-async :source "lockfile" :date (26562 54256 137136 230000) :recipe
           (:package "ob-async" :repo "astahlman/ob-async" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "9aac486073f5c356ada20e716571be33a350a982"))
 (org-cliplink :source "lockfile" :date (26562 54256 128107 713000) :recipe
               (:package "org-cliplink" :repo "rexim/org-cliplink" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref
                "13e0940b65d22bec34e2de4bc8cba1412a7abfbc"))
 (org-download :source "lockfile" :date (26562 54256 118814 773000) :recipe
               (:package "org-download" :repo "abo-abo/org-download" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref
                "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
 (org-make-toc :source "lockfile" :date (26562 54256 109680 407000) :recipe
               (:package "org-make-toc" :fetcher github :repo
                "alphapapa/org-make-toc" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref
                "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa"))
 (org-modern :source "lockfile" :date (26562 54256 100912 994000) :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :host github :ref
              "33f694c57113418c7e739a824ff57dc9e7eb413c"))
 (emacsql :source "lockfile" :date (26562 54256 91200 963000) :recipe
          (:package "emacsql" :fetcher github :repo "skeeto/emacsql" :files
           (:defaults "sqlite") :protocol https :inherit t :depth 1 :host github
           :ref "fb05d0f72729a4b4452a3b1168a9b7b35a851a53"))
 (emacsql-sqlite :source "lockfile" :date (26562 54256 82644 253000) :recipe
                 (:package "emacsql-sqlite" :fetcher github :repo
                  "skeeto/emacsql" :files ("emacsql-sqlite.el" "sqlite")
                  :protocol https :inherit t :depth 1 :host github :ref
                  "fb05d0f72729a4b4452a3b1168a9b7b35a851a53"))
 (org-roam :source "lockfile" :date (26562 54256 74876 44000) :recipe
           (:package "org-roam" :fetcher github :repo "org-roam/org-roam" :files
            (:defaults "extensions/*") :protocol https :inherit t :depth 1 :ref
            "0b9fcbc97b65b349826e63bad89ca121a08fd2be"))
 (browse-at-remote :source "lockfile" :date (26562 54256 65521 638000) :recipe
                   (:package "browse-at-remote" :repo
                    "rmuslimov/browse-at-remote" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref
                    "d81643c975e77d506fe2eb931229739c162adb5d"))
 (cider-storm :source "lockfile" :date (26562 54256 57745 160000) :recipe
              (:protocol https :inherit t :depth 1 :host github :repo
               "jpmonettas/cider-storm" :ref
               "58b8a50406e5e6fbe2d8cdc969543246987386cd" :package "cider-storm"))
 (command-log-mode :source "lockfile" :date (26562 54256 49598 424000) :recipe
                   (:package "command-log-mode" :fetcher github :repo
                    "lewang/command-log-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref
                    "af600e6b4129c8115f464af576505ea8e789db27"))
 (consult-dir :source "lockfile" :date (26562 54256 41728 879000) :recipe
              (:package "consult-dir" :fetcher github :repo
               "karthink/consult-dir" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref
               "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb"))
 (daemons :source "lockfile" :date (26562 54256 33712 448000) :recipe
          (:package "daemons" :fetcher github :repo "cbowdon/daemons.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "e18e84ccc13101f1609c213029cf011ae0ad1178"))
 (deadgrep :source "lockfile" :date (26562 54256 25827 15000) :recipe
           (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "9da7183e60c75bacefd44025fc5e5335b7c5862a"))
 (devdocs-lookup :source "lockfile" :date (26562 54256 17771 436000) :recipe
                 (:protocol https :inherit t :depth 1 :host github :repo
                  "skeeto/devdocs-lookup" :ref
                  "233b9a2bac3c86a7c3d403d85848273086b4c453" :package
                  "devdocs-lookup"))
 (dired-du :source "lockfile" :date (26562 54256 9595 33000) :recipe
           (:package "dired-du" :host github :files ("*" (:exclude ".git"))
            :repo "emacsmirror/dired-du" :protocol https :inherit t :depth 1
            :ref "e5a2aa64849aae14fd6d1973919ec7e13ed76dd0" :branch nil))
 (explain-pause-mode :source "lockfile" :date (26562 54256 1669 329000) :recipe
                     (:protocol https :inherit t :depth 1 :host github :repo
                      "lastquestion/explain-pause-mode" :ref
                      "2356c8c3639cbeeb9751744dbe737267849b4b51" :package
                      "explain-pause-mode"))
 (golden-ratio :source "lockfile" :date (26562 54255 992771 112000) :recipe
               (:package "golden-ratio" :repo "roman/golden-ratio.el" :fetcher
                github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref
                "007911d8a431b72670f5fe5f0e5b4380c2777a31"))
 (hnreader :source "lockfile" :date (26562 54255 983127 305000) :recipe
           (:package "hnreader" :fetcher github :repo "thanhvg/emacs-hnreader"
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "8444e177035e236e991f9ea73074c053a45426ad"))
 (openwith :source "lockfile" :date (26562 54255 974090 577000) :recipe
           (:package "openwith" :fetcher github :repo "jpkotta/openwith" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "1dc89670822966fab6e656f6519fdd7f01e8301a"))
 (ement :source "lockfile" :date (26562 54255 964503 526000) :recipe
        (:package "ement" :host github :files ("*" (:exclude ".git")) :repo
         "alphapapa/ement.el" :protocol https :inherit t :depth 1 :ref
         "1a74446c9396ef305f11c6cb07b94688a55577d9"))
 (popper :source "lockfile" :date (26562 54255 956763 128000) :recipe
         (:package "popper" :fetcher github :repo "karthink/popper" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :inherit t :depth 1 :ref
          "d7560f18350faaee8362aee16481268de3cc6457"))
 (speed-type :source "lockfile" :date (26562 54255 947590 685000) :recipe
             (:package "speed-type" :fetcher github :repo "dakra/speed-type"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "11a8bd33711711fb5e22d93ac2ed950e4a2e76fc"))
 (sudo-edit :source "lockfile" :date (26562 54255 939246 965000) :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "74eb1e6986461baed9a9269566ff838530b4379b"))
 (synosaurus :source "lockfile" :date (26562 54255 929853 218000) :recipe
             (:package "synosaurus" :repo "hpdeifel/synosaurus" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "14d34fc92a77c3a916b4d58400424c44ae99cd81"))
 (trashed :source "lockfile" :date (26562 54255 920755 808000) :recipe
          (:package "trashed" :repo "shingo256/trashed" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "ddf5830730544435a068f2dc9ac75a81ea69df1d"))
 (visual-fill-column :source "lockfile" :date (26562 54255 911249 351000)
                     :recipe
                     (:package "visual-fill-column" :fetcher codeberg :repo
                      "joostkremers/visual-fill-column" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref
                      "453d698d7fc243a547665f8ba43c55eee574e0db"))
 (vterm :source "lockfile" :date (26562 54255 901796 783000) :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
         ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c"
          "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h")
         :protocol https :inherit t :depth 1 :ref
         "99c1f5e76ac927825775c887c93e46036fac7327"))
 (wgrep :source "lockfile" :date (26562 54255 893727 665000) :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep"
         :files ("wgrep.el") :protocol https :inherit t :depth 1 :ref
         "f9687c28bbc2e84f87a479b6ce04407bb97cfb23"))
 (wordnut :source "lockfile" :date (26562 54255 884720 983000) :recipe
          (:package "wordnut" :repo "gromnitsky/wordnut" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "feac531404041855312c1a046bde7ea18c674915"))
 (zones :source "lockfile" :date (26562 54255 876860 694000) :recipe
        (:package "zones" :host github :files ("*" (:exclude ".git")) :repo
         "emacsmirror/zones" :protocol https :inherit t :depth 1 :branch nil
         :ref "932aa3cd9e1827c0ebe68a4af7899704c4dba495"))
 (goto-chg :source "lockfile" :date (26562 54255 867242 977000) :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (annalist :source "lockfile" :date (26562 54255 858247 366000) :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el"
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (compat :source "lockfile" :date (26562 54255 848587 55000) :recipe
         (:package "compat" :host github :files ("*" (:exclude ".git")) :repo
          "emacs-straight/compat" :protocol https :inherit t :depth 1 :ref
          "c1f94ac75d6ea7f93c8da51e3f78ab3f39dbc2a2"))
 (git-commit :source "lockfile" :date (26562 54255 840641 229000) :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
              ("lisp/git-commit.el" "lisp/git-commit-pkg.el") :old-names
              (git-commit-mode) :protocol https :inherit t :depth 1 :ref
              "461d5ba249b8178ae83f80596da4312e65010720"))
 (magit-section :source "lockfile" :date (26562 54255 832787 476000) :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit"
                 :files
                 ("lisp/magit-section.el" "lisp/magit-section-pkg.el"
                  "docs/magit-section.texi" "Documentation/magit-section.texi")
                 :protocol https :inherit t :depth 1 :ref
                 "461d5ba249b8178ae83f80596da4312e65010720"))
 (with-editor :source "lockfile"
   :date (26562 54255 824145 827000) :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :protocol https :inherit t :depth 1 :ref
    "77cb2403158cfea9d8bfb8adad81b84d1d6d7c6a"))
 (nerd-icons :source "lockfile" :date (26562 54255 814638 766000) :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el"
              :fetcher github :files (:defaults "data") :protocol https :inherit
              t :depth 1 :ref "cc6c46830305df123de20b18510b15838e1608d6"))
 (elisp-refs :source "lockfile" :date (26562 54255 806028 21000) :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github
              :files (:defaults (:exclude "elisp-refs-bench.el")) :protocol
              https :inherit t :depth 1 :ref
              "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (ht :source "lockfile" :date (26562 54255 796603 696000) :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
        "LICENSE" "README*" "*-pkg.el"))
      :protocol https :inherit t :depth 1 :ref
      "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (parseedn :source "lockfile" :date (26562 54255 787322 531000) :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "3407e4530a367b6c2b857dae261cdbb67a440aaa"))
 (queue :source "lockfile" :date (26562 54255 779378 634000) :recipe
        (:package "queue" :host github :files ("*" (:exclude ".git")) :repo
         "emacs-straight/queue" :protocol https :inherit t :depth 1 :ref
         "8df1334d54d4735d2f821790422a850dfaaa08ef"))
 (spinner :source "lockfile" :date (26562 54255 770511 92000) :recipe
          (:package "spinner" :host github :files ("*" (:exclude ".git")) :repo
           "emacs-straight/spinner" :protocol https :inherit t :depth 1 :ref
           "fa117f0893788f3fe24673715a6b83bb34d238dd"))
 (sesman :source "lockfile" :date (26562 54255 759663 72000) :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :inherit t :depth 1 :ref
          "7bca68dbbab0af26a6a23be1ff5fa97f9a18e022"))
 (parseclj :source "lockfile" :date (26562 54255 750754 684000) :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "6af22372e0fe14df882dd300b22b12ba2d7e00b0"))
 (yasnippet :source "lockfile" :date (26562 54255 741757 781000) :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github
             :files ("yasnippet.el" "snippets") :protocol https :inherit t
             :depth 1 :ref "fe1f4e0e96ce42d8668920335eb22c3c009dab3e"))
 (multiple-cursors :source "lockfile" :date (26562 54255 731790 94000) :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                    "magnars/multiple-cursors.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref
                    "c870c18462461df19382ecd2f9374c8b902cd804"))
 (inflections :source "lockfile" :date (26562 54255 721801 412000) :recipe
              (:package "inflections" :repo "eschulte/jump.el" :fetcher github
               :files ("inflections.el") :protocol https :inherit t :depth 1
               :ref "55caa66a7cc6e0b1a76143fd40eff38416928941"))
 (hydra :source "lockfile" :date (26562 54255 711905 875000) :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
         (:defaults (:exclude "lv.el")) :protocol https :inherit t :depth 1 :ref
         "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (lv :source "lockfile" :date (26562 54255 701430 662000) :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el")
      :protocol https :inherit t :depth 1 :ref
      "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (let-alist :source
   "lockfile" :date (26562 54255 573531 873000) :recipe
   (:package "let-alist" :host github :files ("*" (:exclude ".git")) :repo
    "emacs-straight/let-alist" :protocol https :inherit t :depth 1 :ref
    "8141e7c941105c92e89bba44e73aae6cdd962b49"))
 (epc :source "lockfile" :date (26562 54255 565509 0) :recipe
      (:package "epc" :repo "kiwanami/emacs-epc" :fetcher github :files
       ("epc.el" "epcs.el") :protocol https :inherit t :depth 1 :ref
       "e1bfa5ca163273859336e3cc89b4b6460f7f8cda"))
 (commenter :source "lockfile" :date (26562 54255 556604 552000) :recipe
            (:package "commenter" :fetcher github :repo "yuutayamada/commenter"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref
             "6d1885419434ba779270c6fda0e30d390bb074bd"))
 (flycheck-nimsuggest :source "lockfile" :date (26562 54255 548631 259000)
                      :recipe
                      (:package "flycheck-nimsuggest" :fetcher github :repo
                       "yuutayamada/flycheck-nimsuggest" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref
                       "dc9a5de1cb3ee05db5794d824610959a1f603bc9"))
 (concurrent :source "lockfile" :date (26562 54255 539657 144000) :recipe
             (:package "concurrent" :repo "kiwanami/emacs-deferred" :fetcher
              github :files ("concurrent.el") :protocol https :inherit t :depth
              1 :ref "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (ctable :source "lockfile" :date (26562 54255 530521 552000) :recipe
         (:package "ctable" :repo "kiwanami/emacs-ctable" :fetcher github :files
          ("ctable.el") :protocol https :inherit t :depth 1 :ref
          "48b73742757a3ae5736d825fe49e00034cc453b5"))
 (deferred :source "lockfile" :date (26562 54255 522326 330000) :recipe
           (:package "deferred" :repo "kiwanami/emacs-deferred" :fetcher github
            :files ("deferred.el") :protocol https :inherit t :depth 1 :ref
            "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (hcl-mode :source "lockfile" :date (26562 54255 512540 614000) :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :inherit t :depth 1 :ref
            "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (bui :source "lockfile" :date (26562 54255 503766 52000) :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
         "LICENSE" "README*" "*-pkg.el"))
       :protocol https :inherit t :depth 1 :ref
       "f3a137628e112a91910fd33c0cff0948fa58d470"))
 (lsp-treemacs :source "lockfile" :date (26562 54255 494924 996000) :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher
                github :files (:defaults "icons") :protocol https :inherit t
                :depth 1 :ref "fb1a07ae0a3d781dea8ac78da2933e0173eb48a4"))
 (lsp-docker :source "lockfile" :date (26562 54255 485665 132000) :recipe
             (:package "lsp-docker" :repo "emacs-lsp/lsp-docker" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "ce291d0f80533f8eaca120eb745d55669e062636"))
 (eldoc :source "lockfile" :date (26562 54255 476226 977000) :recipe
        (:package "eldoc" :host github :files ("*" (:exclude ".git")) :repo
         "emacs-straight/eldoc" :protocol https :inherit t :depth 1 :ref
         "91a8ae2fa66ac09f45337f9eca6c7aa643a063f4"))
 (treemacs :source "lockfile" :date (26562 54255 466637 851000) :recipe
           (:package "treemacs" :fetcher github :repo
            "Alexander-Miller/treemacs" :files
            (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
             "src/scripts/treemacs*.py" (:exclude "src/extra/*"))
            :protocol https :inherit t :depth 1 :ref
            "63e80d4b96c2a411da0beaee8a1e46f116e05e27"))
 (ace-window :source "lockfile" :date (26562 54255 457159 66000) :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref
              "77115afc1b0b9f633084cf7479c767988106c196"))
 (pfuture :source "lockfile" :date (26562 54255 449067 697000) :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github
           :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :inherit t :depth 1 :ref
           "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (cfrs :source "lockfile" :date (26562 54255 440020 950000) :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :inherit t :depth 1 :ref
        "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (avy :source "lockfile" :date (26562 54255 430257 470000) :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
         "LICENSE" "README*" "*-pkg.el"))
       :protocol https :inherit t :depth 1 :ref
       "be612110cb116a38b8603df367942e2bb3d9bdbe"))
 (yaml :source "lockfile" :date (26562 54255 421002 869000) :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :inherit t :depth 1 :ref
        "70c4fcead97e9bd6594e418c922ae769818f4245"))
 (jsonrpc :source "lockfile" :date (26562 54255 411066 893000) :recipe
          (:package "jsonrpc" :host github :files ("*" (:exclude ".git")) :repo
           "emacs-straight/jsonrpc" :protocol https :inherit t :depth 1 :ref
           "cd597b048e2a380c5f4ebfa3bbab90848275a294"))
 (flymake :source "lockfile" :date (26562 54255 401289 705000) :recipe
          (:package "flymake" :host github :files ("*" (:exclude ".git")) :repo
           "emacs-straight/flymake" :protocol https :inherit t :depth 1 :ref
           "1c501d7398922ba8d8f2714f277feb99a58cad2e"))
 (xref :source "lockfile" :date (26562 54255 393781 167000) :recipe
       (:package "xref" :host github :files ("*" (:exclude ".git")) :repo
        "emacs-straight/xref" :protocol https :inherit t :depth 1 :ref
        "03bbbf88dc2f358cfb9662b7fe3493d3fd8f482a"))
 (external-completion :source "lockfile" :date (26562 54255 385654 296000)
                      :recipe
                      (:package "external-completion" :host github :files
                       ("*" (:exclude ".git")) :repo
                       "emacs-straight/external-completion" :protocol https
                       :inherit t :depth 1 :ref
                       "69f06e1eff72df60bb10153169edc859f27f58f2"))
 (async :source "lockfile" :date (26562 54255 377651 899000) :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :inherit t :depth 1 :ref
         "af47d6f930f93d4fdc4ca46e19e17547bf486c4c"))
 (persist :source "lockfile" :date (26562 54255 369752 592000) :recipe
          (:package "persist" :host github :files ("*" (:exclude ".git")) :repo
           "emacs-straight/persist" :protocol https :inherit t :depth 1 :ref
           "706970cf44ba797d67c0a22deabf494067112dd8"))
 (taxy :source "lockfile" :date (26562 54255 360832 99000) :recipe
       (:package "taxy" :host github :files ("*" (:exclude ".git")) :repo
        "emacs-straight/taxy" :protocol https :inherit t :depth 1 :ref
        "a35874a96e1cac5bc94ef572260dbb5f2ce68726"))
 (taxy-magit-section :source "lockfile" :date (26562 54255 351601 277000)
                     :recipe
                     (:package "taxy-magit-section" :host github :files
                      ("*" (:exclude ".git")) :repo
                      "emacs-straight/taxy-magit-section" :protocol https
                      :inherit t :depth 1 :ref
                      "d5d12922d1db83987fdebf2056c7cb014a0aefb0"))
 (svg-lib :source "lockfile" :date (26562 54255 342293 104000) :recipe
          (:package "svg-lib" :host github :files ("*" (:exclude ".git")) :repo
           "emacs-straight/svg-lib" :protocol https :inherit t :depth 1 :ref
           "341b6d13f313ae8fb1a3f6d0a3fb27c02f3925d8")))
