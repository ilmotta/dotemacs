((elpaca :source "lockfile" :date
         (25525 47896 999900 381000)
         :recipe
         (:protocol https :remotes "origin" :inherit t :depth 1 :repo "https://github.com/progfolio/elpaca.git" :ref "0712bb55458d3f3be24e4b1ddb38664d60eee17f" :build
          (:not elpaca--activate-package)
          :package "elpaca" :files
          (:defaults)))
 (use-package :source "lockfile" :date
   (25525 47896 997656 140000)
   :recipe
   (:package "use-package" :fetcher github :repo "jwiegley/use-package" :files
    (:defaults
     (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el"))
    :protocol https :remotes "origin" :inherit t :depth 1 :ref "bcf0984cf55b70fe6896c6a15f61df92b24f8ffd"))
 (dash :source "lockfile" :date
       (25525 47896 996017 708000)
       :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
        ("dash.el" "dash.texi")
        :protocol https :remotes "origin" :inherit t :depth 1 :ref "3df46d7d9fe74f52a661565888e4d31fd760f0df"))
 (f :source "lockfile" :date
    (25525 47896 993590 187000)
    :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
     :protocol https :remotes "origin" :inherit t :depth 1 :ref "d50dca48929575642912bb5bbb2585709ba38f82"))
 (posframe :source "lockfile" :date
           (25525 47896 991258 244000)
           :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "aa88860a16e28a311f81e18f1d9ed2e7d9e33991"))
 (promise :source "lockfile" :date
          (25525 47896 989001 579000)
          :recipe
          (:package "promise" :repo "chuntaro/emacs-promise" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "cec51feb5f957e8febe6325335cf57dc2db6be30"))
 (request :source "lockfile" :date
   (25525 47896 986784 106000)
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
    ("request.el")
    :protocol https :remotes "origin" :inherit t :depth 1 :ref "fe567ec0222a1ba658866697a9e7fb6b63d71ff7"))
 (s :source "lockfile" :date
    (25525 47896 984550 856000)
    :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
     :protocol https :remotes "origin" :inherit t :depth 1 :ref "e957dcb0677da18b2bb60ad867db5df5c35b5616"))
 (shrink-path :source "lockfile" :date
              (25525 47896 982097 794000)
              :recipe
              (:package "shrink-path" :fetcher gitlab :repo "bennya/shrink-path.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (ts :source "lockfile" :date
     (25525 47896 979869 491000)
     :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
      :protocol https :remotes "origin" :inherit t :depth 1 :ref "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (general :source "lockfile" :date
          (25525 47896 977506 354000)
          :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "9651024e7f40a8ac5c3f31f8675d3ebe2b667344"))
 (org :source "lockfile" :date
      (25525 47896 975129 873000)
      :recipe
      (:package "org" :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :depth nil :pre-build
       (progn
         (require 'elpaca-menu-org)
         (elpaca-menu-org--build))
       :build
       (:not elpaca--generate-autoloads-async elpaca--compile-info)
       :files
       (:defaults
        ("etc/styles/" "etc/styles/*"))
       :protocol https :remotes "origin" :inherit t :ref "0f1184a850737d22bf78ee6d7621f65fd2679d4f"))
 (evil :source "lockfile" :date
       (25525 47896 972238 407000)
       :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
        (:defaults "doc/build/texinfo/evil.texi"
         (:exclude "evil-test-helpers.el"))
        :protocol https :remotes "origin" :inherit t :depth 1 :ref "2e8576188b1d0768fbf92c6bea2fb3fbed9f019f"))
 (evil-collection :source "lockfile" :date
                  (25525 47896 970250 367000)
                  :recipe
                  (:package "evil-collection" :fetcher github :repo "emacs-evil/evil-collection" :files
                   (:defaults "modes")
                   :protocol https :remotes "origin" :inherit t :depth 1 :ref "b7a75062a600b1b1d2ba51a1e3ac1ec331d19fff"))
 (magit :source "lockfile" :date
        (25525 47896 967938 816000)
        :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
         ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
          (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "010fec9cdedb2cbe40fc92b0385823e9a21f9842"))
 (xclip :source "lockfile" :date
        (25525 47896 966357 187000)
        :recipe
        (:package "xclip" :host github :repo "emacs-straight/xclip" :protocol https :remotes "origin" :inherit t :depth 1 :ref "a1ac607f75a250dddf49866918bb493884451130" :files
         (:defaults)))
 (consult :source "lockfile" :date
          (25525 47896 964054 446000)
          :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "7c514c0a2414347c4cd0482a691371625a8a1c53"))
 (corfu :source "lockfile" :date
        (25525 47896 961750 954000)
        :recipe
        (:package "corfu" :host github :repo "emacs-straight/corfu" :protocol https :remotes "origin" :inherit t :depth 1 :ref "7bf3ec4622372ed23e83a0778ded53222c4e1187" :files
         (:defaults)))
 (embark :source "lockfile" :date
         (25525 47896 959458 189000)
         :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
          ("embark.el" "embark-org.el" "embark-consult.el")
          :protocol https :remotes "origin" :inherit t :depth 1 :ref "ee014d5f3c86eafae673a947b492fa03ffbacb4e"))
 (orderless :source "lockfile" :date
            (25525 47896 957126 333000)
            :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "e3062280f924933e9c6f5dd1a71729ed98c8493a"))
 (vertico :source "lockfile" :date
          (25525 47896 954936 128000)
          :recipe
          (:package "vertico" :host github :repo "emacs-straight/vertico" :protocol https :remotes "origin" :inherit t :depth 1 :files
           ("*.el" "extensions/*.el")
           :ref "4d2bde64e7c4a07e4c4447283af19382ead37d48"))
 (all-the-icons :source "lockfile" :date
                (25525 47896 952543 657000)
                :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el" :fetcher github :files
                 (:defaults "data")
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "51bf77da1ebc3c199dfc11f54c0dce67559f5f40"))
 (all-the-icons-dired :source "lockfile" :date
                      (25525 47896 950026 271000)
                      :recipe
                      (:package "all-the-icons-dired" :repo "wyuenho/all-the-icons-dired" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :remotes "origin" :inherit t :depth 1 :ref "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a"))
 (anzu :source "lockfile" :date
       (25525 47896 947349 896000)
       :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu" :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :remotes "origin" :inherit t :depth 1 :ref "5abb37455ea44fa401d5f4c1bdc58adb2448db67"))
 (ctrlf :source "lockfile" :date
        (25525 47896 944994 782000)
        :recipe
        (:package "ctrlf" :fetcher github :repo "radian-software/ctrlf" :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "9b4cf6c79a961f2bfbb949805aa300fcf1eb40a6"))
 (dimmer :source "lockfile" :date
         (25525 47896 942736 636000)
         :recipe
         (:package "dimmer" :fetcher github :repo "gonewest818/dimmer.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :remotes "origin" :inherit t :depth 1 :ref "a5b697580e5aed6168b571ae3d925753428284f8"))
 (diredfl :source "lockfile" :date
          (25525 47896 940308 358000)
          :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "94bd99eeced6d52a5a7b9db3745239feafd633e2"))
 (doom-modeline :source "lockfile" :date
                (25525 47896 938080 197000)
                :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "fe9ee5a2a950f9ded10261a05a12adc577ae9e36"))
 (doom-themes :source "lockfile" :date
              (25525 47896 935827 279000)
              :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
               (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el")
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "b5ff201f4bea4286e9ed015a2043cf2394182232"))
 (eros :source "lockfile" :date
       (25525 47896 933592 658000)
       :recipe
       (:package "eros" :fetcher github :repo "xiongtx/eros" :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :remotes "origin" :inherit t :depth 1 :ref "dd8910279226259e100dab798b073a52f9b4233a"))
 (helpful :source "lockfile" :date
          (25525 47896 931286 919000)
          :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "94c25337b2de2f9da60914a7c0c6cca9584c0231"))
 (hide-mode-line :source "lockfile" :date
                 (25525 47896 929266 644000)
                 :recipe
                 (:package "hide-mode-line" :repo "hlissner/emacs-hide-mode-line" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :remotes "origin" :inherit t :depth 1 :ref "bc5d293576c5e08c29e694078b96a5ed85631942"))
 (highlight-parentheses :source "lockfile" :date
                        (25525 47896 926915 389000)
                        :recipe
                        (:package "highlight-parentheses" :fetcher sourcehut :repo "tsdh/highlight-parentheses.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :remotes "origin" :inherit t :depth 1 :ref "438a1cb2563e2a2496be4678cc0df8d5b22caf5d"))
 (idle-highlight-mode :source "lockfile" :date
                      (25525 47896 924398 952000)
                      :recipe
                      (:package "idle-highlight-mode" :fetcher codeberg :repo "ideasman42/emacs-idle-highlight-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :remotes "origin" :inherit t :depth 1 :ref "0cdf8437183766de7e165d5f9ae76646ecccaaa2"))
 (kind-icon :source "lockfile" :date
            (25525 47896 921507 668000)
            :recipe
            (:package "kind-icon" :host github :repo "emacs-straight/kind-icon" :protocol https :remotes "origin" :inherit t :depth 1 :ref "42d2a41874d5a61731556e53ba57547b4ef95342" :files
             (:defaults)))
 (marginalia :source "lockfile" :date
             (25525 47896 918251 631000)
             :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "c1365bf0c7b5d32e7531fa8f1a9a3b64a155cec0"))
 (page-break-lines :source "lockfile" :date
                   (25525 47896 915729 949000)
                   :recipe
                   (:package "page-break-lines" :fetcher github :repo "purcell/page-break-lines" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :remotes "origin" :inherit t :depth 1 :ref "79eca86e0634ac68af862e15c8a236c37f446dcd"))
 (paren-face :source "lockfile" :date
             (25525 47896 912881 962000)
             :recipe
             (:package "paren-face" :repo "tarsius/paren-face" :fetcher github :old-names
              (parenface)
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "bf741a6038a2554abf98d31e658421c33f8bf7a4"))
 (pulsar :source "lockfile" :date
         (25525 47896 909858 613000)
         :recipe
         (:package "pulsar" :host github :repo "emacs-straight/pulsar" :protocol https :remotes "origin" :inherit t :depth 1 :ref "57010e2c6cdee14acfd87b4c2bd75c796f04a75e" :files
          (:defaults)))
 (xterm-color :source "lockfile" :date
              (25525 47896 907047 635000)
              :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "1a4012854c69a5cdaeb5a73d2ad705011892fca3"))
 (aggressive-indent :source "lockfile" :date
                    (25525 47896 904735 808000)
                    :recipe
                    (:package "aggressive-indent" :repo "Malabarba/aggressive-indent-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :remotes "origin" :inherit t :depth 1 :ref "f376cdc25de5c0f8c330f1e053557d95ca47a540"))
 (apheleia :source "lockfile" :date
           (25525 47896 902263 317000)
           :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "5ebd6bf5819fbf2adfa18162f270825e6ca4379c"))
 (drag-stuff :source "lockfile" :date
             (25525 47896 899788 231000)
             :recipe
             (:package "drag-stuff" :repo "rejeep/drag-stuff.el" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "6d06d846cd37c052d79acd0f372c13006aa7e7c8"))
 (emmet-mode :source "lockfile" :date
             (25525 47896 896956 120000)
             :recipe
             (:package "emmet-mode" :fetcher github :repo "smihica/emmet-mode" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "63b6932603184956b5ea8919036d2b307b48d7fd"))
 (evil-cleverparens :source "lockfile" :date
                    (25525 47896 894191 259000)
                    :recipe
                    (:package "evil-cleverparens" :fetcher github :repo "emacs-evil/evil-cleverparens" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :remotes "origin" :inherit t :depth 1 :ref "22aa03d0f50aa70ae08fbe8765a88f5020afa635"))
 (evil-matchit :source "lockfile" :date
               (25525 47896 891472 525000)
               :recipe
               (:package "evil-matchit" :fetcher github :repo "redguardtoo/evil-matchit" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "ec3dd819983b2d824142efddd46ef29b46a7c454"))
 (evil-nerd-commenter :source "lockfile" :date
                      (25525 47896 889223 646000)
                      :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo "redguardtoo/evil-nerd-commenter" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :remotes "origin" :inherit t :depth 1 :ref "8c0f23d46a3927b9f83c1c2c4590be53d0b740db"))
 (evil-smartparens :source "lockfile" :date
                   (25525 47896 886954 797000)
                   :recipe
                   (:package "evil-smartparens" :fetcher github :repo "expez/evil-smartparens" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :remotes "origin" :inherit t :depth 1 :ref "026d4a3cfce415a4dfae1457f871b385386e61d3"))
 (evil-surround :source "lockfile" :date
                (25525 47896 884656 20000)
                :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher github :old-names
                 (surround)
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "c9e1449bf3f740b5e9b99e7820df4eca7fc7cf02"))
 (flycheck :source "lockfile" :date
           (25525 47896 882439 236000)
           :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "15f0759602f9a31aff134c44d001ab058fbe747c"))
 (flyspell-correct :source "lockfile" :date
                   (25525 47896 880079 524000)
                   :recipe
                   (:package "flyspell-correct" :repo "d12frosted/flyspell-correct" :fetcher github :files
                    ("flyspell-correct.el" "flyspell-correct-ido.el")
                    :protocol https :remotes "origin" :inherit t :depth 1 :ref "7d7b6b01188bd28e20a13736ac9f36c3367bd16e"))
 (quickrun :source "lockfile" :date
           (25525 47896 877793 320000)
           :recipe
           (:package "quickrun" :repo "emacsorphanage/quickrun" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "7a89313c07a21eae9cd69a1a98e2a134d559e04f"))
 (paredit :source "lockfile" :date
          (25525 47896 875531 400000)
          :recipe
          (:package "paredit" :fetcher git :url "https://mumble.net/~campbell/git/paredit.git" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "emacsmirror/paredit" :ref "009c95980e52cc4d736fa1404cf17c86fe97fd7d"))
 (smartparens :source "lockfile" :date
              (25525 47896 873311 519000)
              :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "0a23136dd6b1f326419c5828f4197ecfd820b204"))
 (undo-fu :source "lockfile" :date
          (25525 47896 871017 669000)
          :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "601fed8e4bbed041dea5969600d985c0c17759ad"))
 (cargo :source "lockfile" :date
        (25525 47896 868727 889000)
        :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "d2720c8dc7ac3b18ce112a886d3b8696797d01cb"))
 (cider :source "lockfile" :date
        (25525 47896 866269 137000)
        :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider" :files
         ("*.el"
          (:exclude ".dir-locals.el"))
         :old-names
         (nrepl)
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "17743001467e0045ecd6639aad45d21e89d6b9a2"))
 (clj-refactor :source "lockfile" :date
               (25525 47896 863994 76000)
               :recipe
               (:package "clj-refactor" :fetcher github :repo "clojure-emacs/clj-refactor.el" :files
                (:defaults "CHANGELOG.md")
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "8300d5cab861668f313fbbbb3e2926e3e5130e86"))
 (clojure-mode :source "lockfile" :date
               (25525 47896 861629 872000)
               :recipe
               (:package "clojure-mode" :repo "clojure-emacs/clojure-mode" :fetcher github :files
                ("clojure-mode.el")
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "3453cd229b412227aaffd1dc2870fa8fa213c5b1"))
 (csv-mode :source "lockfile" :date
           (25525 47896 859382 153000)
           :recipe
           (:package "csv-mode" :host github :repo "emacs-straight/csv-mode" :protocol https :remotes "origin" :inherit t :depth 1 :ref "58d1b74e5ecdff748f314bf701f5048ad35984b3" :files
            (:defaults)))
 (dockerfile-mode :source "lockfile" :date
                  (25525 47896 857143 218000)
                  :recipe
                  (:package "dockerfile-mode" :fetcher github :repo "spotify/dockerfile-mode" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :remotes "origin" :inherit t :depth 1 :ref "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c"))
 (flycheck-clj-kondo :source "lockfile" :date
                     (25525 47896 854815 811000)
                     :recipe
                     (:package "flycheck-clj-kondo" :fetcher github :repo "borkdude/flycheck-clj-kondo" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :remotes "origin" :inherit t :depth 1 :ref "ff7bed2315755cfe02ef471edf522e27b78cd5ca"))
 (flycheck-ledger :source "lockfile" :date
                  (25525 47896 852572 667000)
                  :recipe
                  (:package "flycheck-ledger" :fetcher github :repo "purcell/flycheck-ledger" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :remotes "origin" :inherit t :depth 1 :ref "628e25ba66604946085571652a94a54f4d1ad96f"))
 (flycheck-rust :source "lockfile" :date
                (25525 47896 850258 582000)
                :recipe
                (:package "flycheck-rust" :repo "flycheck/flycheck-rust" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "a139cd53c5062697e9ed94ad80b803c37d999600"))
 (geiser :source "lockfile" :date
         (25525 47896 848035 557000)
         :recipe
         (:package "geiser" :fetcher gitlab :repo "emacs-geiser/geiser" :files
          ("elisp/*.el" "doc/dir" "doc/geiser.texi")
          :protocol https :remotes "origin" :inherit t :depth 1 :ref "bfc9cce54b7ac1cb036911965198b5cbe2f43f4c"))
 (git-modes :source "lockfile" :date
            (25525 47896 845925 348000)
            :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes" :old-names
             (gitattributes-mode gitconfig-mode gitignore-mode)
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "be96ef14fab6a2d76cca3ebf9a15b462a695923d"))
 (gnuplot :source "lockfile" :date
          (25525 47896 843621 453000)
          :recipe
          (:package "gnuplot" :repo "emacs-gnuplot/gnuplot" :fetcher github :files
           ("gnuplot.el" "gnuplot-gui.el" "gnuplot-context.el")
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "fe7ce76d797b34214178ac8e470f2fa9a63b2520"))
 (graphql-mode :source "lockfile" :date
               (25525 47896 841225 420000)
               :recipe
               (:package "graphql-mode" :repo "davazp/graphql-mode" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "1437b790060f6ce4a8dc57df2023443645b899e5"))
 (groovy-mode :source "lockfile" :date
              (25525 47896 838853 674000)
              :recipe
              (:package "groovy-mode" :fetcher github :repo "Groovy-Emacs-Modes/groovy-emacs-modes" :files
               ("*groovy*.el")
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "c612ac1e9f742856914ad6e8eb9e9dc169f489ab"))
 (haskell-mode :source "lockfile" :date
               (25525 47896 836609 716000)
               :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher github :files
                (:defaults "NEWS" "logo.svg")
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "a34ccdc54be15043ff0d253c3c20087524255491"))
 (kbd-mode :source "lockfile" :date
           (25525 47896 834272 843000)
           :recipe
           (:protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "kmonad/kbd-mode" :ref "96178a43d3c9ea3167362513fe4c3fdeb7074e9f" :package "kbd-mode" :files
            (:defaults)))
 (kotlin-mode :source "lockfile" :date
              (25525 47896 832056 849000)
              :recipe
              (:package "kotlin-mode" :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode" :fetcher github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "55eed95033a59d7448a4b2bc11879e62c05e361b"))
 (ledger-mode :source "lockfile" :date
              (25525 47896 829774 166000)
              :recipe
              (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode" :files
               ("ledger*.el")
               :old-names
               (ldg-mode)
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "8bad528d43007e0310b5e72e6e021b502b30495c"))
 (lua-mode :source "lockfile" :date
           (25525 47896 827661 739000)
           :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
            (:defaults
             (:exclude "init-tryout.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "ad639c62e38a110d8d822c4f914af3e20b40ccc4"))
 (markdown-mode :source "lockfile" :date
                (25525 47896 825347 823000)
                :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "d95107f5b77d6c010e89259e05adfcd79a21f26a"))
 (nix-mode :source "lockfile" :date
           (25525 47896 822884 561000)
           :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
            (:defaults
             (:exclude "nix-company.el" "nix-mode-mmm.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "54e5626829168e22126b233e079f04dff3c71b90"))
 (php-mode :source "lockfile" :date
           (25525 47896 820291 769000)
           :recipe
           (:package "php-mode" :repo "emacs-php/php-mode" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "d01cfc9cd51706e076bf7e5cbf0cfa7ee885efb4"))
 (plantuml-mode :source "lockfile" :date
                (25525 47896 818147 872000)
                :recipe
                (:package "plantuml-mode" :fetcher github :repo "skuro/plantuml-mode" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "ea45a13707abd2a70df183f1aec6447197fc9ccc"))
 (rust-mode :source "lockfile" :date
            (25525 47896 816055 589000)
            :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "0431b10d2520918f3f250fdf4dc96e8d2eb7ea76"))
 (terraform-mode :source "lockfile" :date
                 (25525 47896 813784 501000)
                 :recipe
                 (:package "terraform-mode" :repo "emacsorphanage/terraform-mode" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :remotes "origin" :inherit t :depth 1 :ref "e67459fefc871fdbf20e27be8f85b98b10b97b1b"))
 (tree-sitter :source "lockfile" :date
              (25525 47896 811367 290000)
              :recipe
              (:package "tree-sitter" :repo "emacs-tree-sitter/elisp-tree-sitter" :fetcher github :branch "release" :files
               (:defaults
                (:exclude "lisp/tree-sitter-tests.el"))
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (tree-sitter-langs :source "lockfile" :date
                    (25525 47896 809371 497000)
                    :recipe
                    (:package "tree-sitter-langs" :repo "emacs-tree-sitter/tree-sitter-langs" :fetcher github :branch "release" :files
                     (:defaults "queries")
                     :protocol https :remotes "origin" :inherit t :depth 1 :ref "bf125472c185f098136b26b30d5e74332b4ee46b"))
 (tsi :source "lockfile" :date
      (25525 47896 806730 208000)
      :recipe
      (:protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "orzechowskid/tsi.el" :package "tsi" :files
       (:defaults)
       :ref "eb26ee20437576eefc62a61616bfdc5bda25caae"))
 (typescript-mode :source "lockfile" :date
                  (25525 47896 804603 190000)
                  :recipe
                  (:package "typescript-mode" :fetcher github :repo "emacs-typescript/typescript.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :remotes "origin" :inherit t :depth 1 :ref "c7004fc5a85591a795524bd920618e5e467746af"))
 (vimrc-mode :source "lockfile" :date
             (25525 47896 802225 405000)
             :recipe
             (:package "vimrc-mode" :fetcher github :repo "mcandre/vimrc-mode" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "13bc150a870d5d4a95f1111e4740e2b22813c30e"))
 (web-mode :source "lockfile" :date
           (25525 47896 800019 732000)
           :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "53bed1e6a8554da877c27ffad6bd65113dc758e3"))
 (yaml-mode :source "lockfile" :date
            (25525 47896 797751 958000)
            :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7"))
 (dap-mode :source "lockfile" :date
           (25525 47896 795435 309000)
           :recipe
           (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher github :files
            (:defaults "icons")
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "512b70bb71b5727bfb155f08f7e9a32f0496f1a6"))
 (lsp-java :source "lockfile" :date
           (25525 47896 792528 777000)
           :recipe
           (:package "lsp-java" :repo "emacs-lsp/lsp-java" :fetcher github :files
            (:defaults "icons")
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "de2d89814fecb9bae825baa7028c5cd8b32b9b8f"))
 (lsp-mode :source "lockfile" :date
           (25525 47896 789811 533000)
           :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
            (:defaults "clients/*.el")
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "7dee0d63fa1b6628be4aaea86b2298244eb3d84e"))
 (lsp-ui :source "lockfile" :date
         (25525 47896 787492 975000)
         :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
          (:defaults "lsp-ui-doc.html" "resources")
          :protocol https :remotes "origin" :inherit t :depth 1 :ref "fb1073013f745bce056811a38e2b0b8b2a4b5ebc"))
 (evil-org :source "lockfile" :date
           (25525 47896 785206 369000)
           :recipe
           (:package "evil-org" :fetcher github :repo "Somelauw/evil-org-mode" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "b1f309726b1326e1a103742524ec331789f2bf94"))
 (htmlize :source "lockfile" :date
          (25525 47896 782930 385000)
          :recipe
          (:package "htmlize" :fetcher github :repo "hniksic/emacs-htmlize" :version-regexp "release/\\(.*\\)" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9"))
 (ob-async :source "lockfile" :date
           (25525 47896 780669 393000)
           :recipe
           (:package "ob-async" :repo "astahlman/ob-async" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "9aac486073f5c356ada20e716571be33a350a982"))
 (org-cliplink :source "lockfile" :date
               (25525 47896 778459 764000)
               :recipe
               (:package "org-cliplink" :repo "rexim/org-cliplink" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "13e0940b65d22bec34e2de4bc8cba1412a7abfbc"))
 (org-download :source "lockfile" :date
               (25525 47896 776048 388000)
               :recipe
               (:package "org-download" :repo "abo-abo/org-download" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
 (org-make-toc :source "lockfile" :date
               (25525 47896 773655 326000)
               :recipe
               (:package "org-make-toc" :fetcher github :repo "alphapapa/org-make-toc" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa"))
 (org-modern :source "lockfile" :date
             (25525 47896 771366 997000)
             :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :host github :ref "010eade723881ca234a12bd94b791e2000cd2a15"))
 (emacsql :source "lockfile" :date
          (25525 47896 769130 945000)
          :recipe
          (:package "emacsql" :fetcher github :repo "skeeto/emacsql" :files
           ("emacsql.el" "emacsql-compiler.el" "README.md")
           :protocol https :remotes "origin" :inherit t :depth 1 :host github :ref "6b2e65bdf785364cf7c34c31fea5812e1e58c657"))
 (emacsql-sqlite :source "lockfile" :date
                 (25525 47896 767536 64000)
                 :recipe
                 (:package "emacsql-sqlite" :fetcher github :repo "skeeto/emacsql" :files
                  ("emacsql-sqlite.el" "sqlite")
                  :protocol https :remotes "origin" :inherit t :depth 1 :host github :ref "6b2e65bdf785364cf7c34c31fea5812e1e58c657"))
 (org-roam :source "lockfile" :date
           (25525 47896 765079 863000)
           :recipe
           (:package "org-roam" :fetcher github :repo "org-roam/org-roam" :files
            (:defaults "extensions/*")
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "45a6863e074f049489a2b9fa94f14635bcce4788"))
 (ace-window :source "lockfile" :date
             (25525 47896 762719 546000)
             :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :rev "77115afc1b0b9f633084cf7479c767988106c196" :ref "77115afc1b0b9f633084cf7479c767988106c196"))
 (browse-at-remote :source "lockfile" :date
                   (25525 47896 760157 753000)
                   :recipe
                   (:package "browse-at-remote" :repo "rmuslimov/browse-at-remote" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :remotes "origin" :inherit t :depth 1 :ref "ec6a9d5e8229067518579c4ea992c6881f2543dc"))
 (command-log-mode :source "lockfile" :date
                   (25525 47896 758357 759000)
                   :recipe
                   (:package "command-log-mode" :fetcher github :repo "lewang/command-log-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :remotes "origin" :inherit t :depth 1 :ref "af600e6b4129c8115f464af576505ea8e789db27"))
 (consult-dir :source "lockfile" :date
              (25525 47896 755790 741000)
              :recipe
              (:package "consult-dir" :fetcher github :repo "karthink/consult-dir" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb"))
 (daemons :source "lockfile" :date
          (25525 47896 753431 787000)
          :recipe
          (:package "daemons" :fetcher github :repo "cbowdon/daemons.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "e18e84ccc13101f1609c213029cf011ae0ad1178"))
 (deadgrep :source "lockfile" :date
           (25525 47896 750855 784000)
           :recipe
           (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "9da7183e60c75bacefd44025fc5e5335b7c5862a"))
 (detached :source "lockfile" :date
           (25525 47896 748575 216000)
           :recipe
           (:package "detached" :fetcher sourcehut :repo "niklaseklund/detached.el" :old-names
            (dtache)
            :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "6b64d4d8064cee781e071e825857b442ea96c3d9"))
 (devdocs-lookup :source "lockfile" :date
                 (25525 47896 746288 795000)
                 :recipe
                 (:protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "skeeto/devdocs-lookup" :ref "233b9a2bac3c86a7c3d403d85848273086b4c453" :package "devdocs-lookup" :files
                  (:defaults)))
 (dired-du :source "lockfile" :date
           (25525 47896 744103 649000)
           :recipe
           (:package "dired-du" :host github :repo "emacs-straight/dired-du" :protocol https :remotes "origin" :inherit t :depth 1 :ref "e5a2aa64849aae14fd6d1973919ec7e13ed76dd0" :files
            (:defaults)))
 (explain-pause-mode :source "lockfile" :date
                     (25525 47896 741670 773000)
                     :recipe
                     (:protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "lastquestion/explain-pause-mode" :ref "2356c8c3639cbeeb9751744dbe737267849b4b51" :package "explain-pause-mode" :files
                      (:defaults)))
 (hnreader :source "lockfile" :date
           (25525 47896 739242 143000)
           :recipe
           (:package "hnreader" :fetcher github :repo "thanhvg/emacs-hnreader" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "8444e177035e236e991f9ea73074c053a45426ad"))
 (openwith :source "lockfile" :date
           (25525 47896 736582 576000)
           :recipe
           (:package "openwith" :fetcher github :repo "jpkotta/openwith" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "1dc89670822966fab6e656f6519fdd7f01e8301a"))
 (pdf-tools :source "lockfile" :date
            (25525 47896 734062 709000)
            :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
             (:defaults "README"
              ("build" "Makefile")
              ("build" "server"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "b8079e4ebc2936f9772657332d50936350a65825"))
 (popper :source "lockfile" :date
         (25525 47896 731712 364000)
         :recipe
         (:package "popper" :fetcher github :repo "karthink/popper" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :remotes "origin" :inherit t :depth 1 :ref "d7560f18350faaee8362aee16481268de3cc6457"))
 (speed-type :source "lockfile" :date
             (25525 47896 729346 484000)
             :recipe
             (:package "speed-type" :fetcher github :repo "dakra/speed-type" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "11a8bd33711711fb5e22d93ac2ed950e4a2e76fc"))
 (sudo-edit :source "lockfile" :date
            (25525 47896 726775 371000)
            :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "74eb1e6986461baed9a9269566ff838530b4379b"))
 (svg-clock :source "lockfile" :date
            (25525 47896 724588 20000)
            :recipe
            (:package "svg-clock" :host github :repo "emacs-straight/svg-clock" :protocol https :remotes "origin" :inherit t :depth 1 :ref "0b92fed41aa65238ae7f9716c59cdec583463933" :files
             (:defaults)))
 (synosaurus :source "lockfile" :date
             (25525 47896 722359 542000)
             :recipe
             (:package "synosaurus" :repo "hpdeifel/synosaurus" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "14d34fc92a77c3a916b4d58400424c44ae99cd81"))
 (timer-revert :source "lockfile" :date
               (25525 47896 720099 596000)
               :recipe
               (:package "timer-revert" :repo "yyr/timer-revert" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "615c91dec8b440d2b9b7c725dd733d7432564e45"))
 (trashed :source "lockfile" :date
          (25525 47896 717552 10000)
          :recipe
          (:package "trashed" :repo "shingo256/trashed" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "ddf5830730544435a068f2dc9ac75a81ea69df1d"))
 (visual-fill-column :source "lockfile" :date
                     (25525 47896 715618 237000)
                     :recipe
                     (:package "visual-fill-column" :fetcher codeberg :repo "joostkremers/visual-fill-column" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :remotes "origin" :inherit t :depth 1 :ref "453d698d7fc243a547665f8ba43c55eee574e0db"))
 (vterm :source "lockfile" :date
        (25525 47896 713078 442000)
        :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
         ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h")
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "f14d113ee4618f052879509ec378feb9766b871b"))
 (wgrep :source "lockfile" :date
        (25525 47896 710558 699000)
        :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
         ("wgrep.el")
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "f9687c28bbc2e84f87a479b6ce04407bb97cfb23"))
 (wordnut :source "lockfile" :date
          (25525 47896 708096 623000)
          :recipe
          (:package "wordnut" :repo "gromnitsky/wordnut" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "feac531404041855312c1a046bde7ea18c674915"))
 (bind-key :source "lockfile" :date
           (25525 47896 705233 374000)
           :recipe
           (:package "bind-key" :fetcher github :repo "jwiegley/use-package" :files
            ("bind-key.el")
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "bcf0984cf55b70fe6896c6a15f61df92b24f8ffd"))
 (goto-chg :source "lockfile" :date
           (25525 47896 702767 897000)
           :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "278cd3e6d5107693aa2bb33189ca503f22f227d0"))
 (annalist :source "lockfile" :date
           (25525 47896 700239 288000)
           :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "134fa3f0fb91a636a1c005c483516d4b64905a6d"))
 (compat :source "lockfile" :date
         (25525 47896 697661 409000)
         :recipe
         (:package "compat" :host github :repo "emacs-straight/compat" :protocol https :remotes "origin" :inherit t :depth 1 :files
          (:defaults)
          :ref "7ca7d300d1d256f674f83932d2918d8e70cd28f6"))
 (git-commit :source "lockfile" :date
             (25525 47896 695422 717000)
             :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
              ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
              :old-names
              (git-commit-mode)
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "010fec9cdedb2cbe40fc92b0385823e9a21f9842"))
 (magit-section :source "lockfile" :date
                (25525 47896 693824 884000)
                :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                 ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi")
                 :protocol https :remotes "origin" :inherit t :depth 1 :ref "010fec9cdedb2cbe40fc92b0385823e9a21f9842"))
 (transient :source "lockfile" :date
            (25525 47896 691610 573000)
            :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "c6cf2f2705ab56cd89d807e723ce45b9fcdfb9e1"))
 (with-editor :source "lockfile" :date
   (25525 47896 688235 8000)
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
    :protocol https :remotes "origin" :inherit t :depth 1 :ref "4da109748da0828b79198701eb641d5b724153ce"))
 (elisp-refs :source "lockfile" :date
             (25525 47896 685868 921000)
             :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
              (:defaults
               (:exclude "elisp-refs-bench.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "af73739084637c8ebadad337a8fe58ff4f1d2ec1"))
 (svg-lib :source "lockfile" :date
          (25525 47896 683251 37000)
          :recipe
          (:package "svg-lib" :host github :repo "emacs-straight/svg-lib" :protocol https :remotes "origin" :inherit t :depth 1 :files
           (:defaults)
           :ref "da72b81d8589d045731140a836cfbc2891e4ebf3"))
 (pkg-info :source "lockfile" :date
           (25525 47896 680971 983000)
           :recipe
           (:package "pkg-info" :repo "emacsorphanage/pkg-info" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "76ba7415480687d05a4353b27fea2ae02b8d9d61"))
 (let-alist :source "lockfile" :date
            (25525 47896 678389 599000)
            :recipe
            (:package "let-alist" :host github :repo "emacs-straight/let-alist" :protocol https :remotes "origin" :inherit t :depth 1 :files
             (:defaults)
             :ref "592553db5929b54db40af0df90c5add0aaca045b"))
 (epl :source "lockfile" :date
      (25525 47896 675924 876000)
      :recipe
      (:package "epl" :repo "cask/epl" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "78ab7a85c08222cd15582a298a364774e3282ce6"))
 (ht :source "lockfile" :date
     (25525 47896 672619 589000)
     :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
      :protocol https :remotes "origin" :inherit t :depth 1 :ref "e83fdb8bc0a3cc8cd2687a947e2610b20b68b7d3"))
 (parseedn :source "lockfile" :date
           (25525 47896 670404 611000)
           :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "a09686fbb9113b8b1b4f20c9e1dc0d6fea01a64f"))
 (queue :source "lockfile" :date
        (25525 47896 668016 205000)
        :recipe
        (:package "queue" :host github :repo "emacs-straight/queue" :protocol https :remotes "origin" :inherit t :depth 1 :files
         (:defaults)
         :ref "130c2d656cd5d7376552272fab9e50a7c37d0c4a"))
 (spinner :source "lockfile" :date
          (25525 47896 665655 171000)
          :recipe
          (:package "spinner" :host github :repo "emacs-straight/spinner" :protocol https :remotes "origin" :inherit t :depth 1 :files
           (:defaults)
           :ref "634529bb3173e09b37499f636de70abf29d9fa8a"))
 (sesman :source "lockfile" :date
         (25525 47896 663422 87000)
         :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :protocol https :remotes "origin" :inherit t :depth 1 :ref "e0f555f963c9f02f8e4a50e06fc353eb4c15ee77"))
 (parseclj :source "lockfile" :date
           (25525 47896 660974 749000)
           :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "4d0e780e00f1828b00c43099e6eebc6582998f72"))
 (yasnippet :source "lockfile" :date
            (25525 47896 658652 503000)
            :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
             ("yasnippet.el" "snippets")
             :protocol https :remotes "origin" :inherit t :depth 1 :ref "5cbdbf0d2015540c59ed8ee0fcf4788effdf75b6"))
 (multiple-cursors :source "lockfile" :date
                   (25525 47896 656152 417000)
                   :recipe
                   (:package "multiple-cursors" :fetcher github :repo "magnars/multiple-cursors.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :remotes "origin" :inherit t :depth 1 :ref "7f255ce69603de084d25f615b8556c093cce906b"))
 (inflections :source "lockfile" :date
              (25525 47896 653680 720000)
              :recipe
              (:package "inflections" :repo "eschulte/jump.el" :fetcher github :files
               ("inflections.el")
               :protocol https :remotes "origin" :inherit t :depth 1 :ref "55caa66a7cc6e0b1a76143fd40eff38416928941"))
 (hydra :source "lockfile" :date
        (25525 47896 651084 412000)
        :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
         (:defaults
          (:exclude "lv.el"))
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (lv :source "lockfile" :date
     (25525 47896 649473 720000)
     :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
      ("lv.el")
      :protocol https :remotes "origin" :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (project :source "lockfile" :date
          (25525 47896 647147 587000)
          :recipe
          (:package "project" :host github :repo "emacs-straight/project" :protocol https :remotes "origin" :inherit t :depth 1 :files
           (:defaults)
           :ref "53d1784ca2dda1a2da9b8f2f168a9706f6b36ccf"))
 (xref :source "lockfile" :date
       (25525 47896 644726 370000)
       :recipe
       (:package "xref" :host github :repo "emacs-straight/xref" :protocol https :remotes "origin" :inherit t :depth 1 :files
        (:defaults)
        :ref "0dc81218b59e7f199265704d1af37219af86d381"))
 (hcl-mode :source "lockfile" :date
           (25525 47896 641850 804000)
           :recipe
           (:package "hcl-mode" :repo "purcell/emacs-hcl-mode" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "e4d9eef631e8a386341ae8f94f7c2579586e65b5"))
 (tsc :source "lockfile" :date
      (25525 47896 639504 165000)
      :recipe
      (:package "tsc" :fetcher github :repo "emacs-tree-sitter/elisp-tree-sitter" :branch "release" :files
       ("core/*.el" "core/Cargo.toml" "core/Cargo.lock" "core/src")
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (bui :source "lockfile" :date
      (25525 47896 636733 920000)
      :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "f3a137628e112a91910fd33c0cff0948fa58d470"))
 (lsp-treemacs :source "lockfile" :date
               (25525 47896 633919 152000)
               :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher github :files
                (:defaults "icons")
                :protocol https :remotes "origin" :inherit t :depth 1 :ref "a48763ba5d1c024426e237ce65926db849d3ae6f"))
 (lsp-docker :source "lockfile" :date
             (25525 47896 631723 818000)
             :recipe
             (:package "lsp-docker" :repo "emacs-lsp/lsp-docker" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :remotes "origin" :inherit t :depth 1 :ref "1e1f33ed729c220485c16e6597738d8e416f31b7"))
 (eldoc :source "lockfile" :date
        (25525 47896 629148 641000)
        :recipe
        (:package "eldoc" :host github :repo "emacs-straight/eldoc" :protocol https :remotes "origin" :inherit t :depth 1 :files
         (:defaults)
         :ref "192bcd5571a84e4b4084a840565f40fbec0b0abc"))
 (treemacs :source "lockfile" :date
           (25525 47896 626574 640000)
           :recipe
           (:package "treemacs" :fetcher github :repo "Alexander-Miller/treemacs" :files
            (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py"
             (:exclude "src/extra/*"))
            :protocol https :remotes "origin" :inherit t :depth 1 :ref "0caed0b69b67fa21d949ecd8639053e82423a5e1"))
 (pfuture :source "lockfile" :date
          (25525 47896 623535 807000)
          :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (cfrs :source "lockfile" :date
       (25525 47896 621106 706000)
       :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :remotes "origin" :inherit t :depth 1 :ref "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (avy :source "lockfile" :date
      (25525 47896 618313 955000)
      :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "955c8dedd68c74f3cf692c1249513f048518c4c9"))
 (yaml :source "lockfile" :date
       (25525 47896 616017 223000)
       :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
        :protocol https :remotes "origin" :inherit t :depth 1 :ref "73fde9d8fbbaf2596449285df9eb412ae9dd74d9"))
 (async :source "lockfile" :date
        (25525 47896 613043 225000)
        :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
         :protocol https :remotes "origin" :inherit t :depth 1 :ref "c4772bec684776e93f1b8d845b452dc850ee2315"))
 (tablist :source "lockfile" :date
          (25525 47896 610350 625000)
          :recipe
          (:package "tablist" :fetcher github :repo "politza/tablist" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
           :protocol https :remotes "origin" :inherit t :depth 1 :ref "faab7a035ef2258cc4ea2182f67e3aedab7e2af9"))
 (svg :source "lockfile" :date
      (25525 47896 607206 345000)
      :recipe
      (:package "svg" :host github :repo "emacs-straight/svg" :protocol https :remotes "origin" :inherit t :depth 1 :files
       (:defaults)
       :ref "d36c65e63f142b20de4345ff30793618ce23153e")))
