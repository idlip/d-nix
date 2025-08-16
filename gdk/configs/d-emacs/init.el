;; -*- lexical-binding: t; -*-

(setopt
 display-time-24hr-format t
 display-time-default-load-average nil
 display-time-format "%H:%M")
(display-time-mode 1)

(setopt battery-load-low '40 battery-load-critical '29)
(display-battery-mode 1)

(bind-keys ("C-z") ("C-x C-z") ("M-o" . other-window) ("M-j" . duplicate-dwim) )

(setopt
 inhibit-startup-screen t
 initial-scratch-message (format "\n\n")

 tab-always-indent 'complete
 tab-width 4
 reb-re-syntax 'string
 fill-column 80

 window-combination-resize t
 history-delete-duplicates t

 sentence-end-double-space nil
 sentence-end "[.?!,;-]"
 read-process-output-max (* 1024 1024)
 initial-major-mode 'org-mode
 )

(delete-selection-mode 1) (indent-tabs-mode -1)
(global-so-long-mode 1)
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))

(setopt
 kill-ring-max 30000
 kill-do-not-save-duplicates t
 set-mark-command-repeat-pop t
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 async-shell-command-buffer 'new-buffer
 async-shell-command-display-buffer nil
 grep-use-headings t
 save-interprogram-paste-before-kill t
 )
;; (global-hl-line-mode 1)
(save-place-mode 1)
(global-visual-line-mode 1)

(setopt display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(auto-save-visited-mode 1)

(setopt
 save-silently t
 confirm-kill-emacs 'yes-or-no-p
 view-read-only t
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 safe-local-variable-directories
 '("/home/idlip/d-sync/notes/")
 create-lockfiles nil
 backup-directory-alist '(("." . "~/.config/emacs/backups"))
 version-control t 
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 2
 )

(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :custom (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind
  (("C-x u" . vundo) ("C-z" . undo-only) ("C-S-z" . undo-redo) ("C-M-r" . undo-redo))
  :custom
  (vundo-compact-display t) (vundo-glyph-alist vundo-unicode-symbols))

(setopt global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setopt
 history-length 2000
 save-place-limit nil
 savehist-additional-variables
 '(kill-ring command-history
             set-variable-value-history custom-variable-history
             query-replace-history read-expression-history
             minibuffer-history read-char-history face-name-history
             bookmark-history file-name-history
             mark-ring global-mark-ring search-ring regexp-search-ring register-alist extended-command-history))
(savehist-mode 1)

(global-set-key (kbd "C-x C-r") #'recentf)
(setopt recentf-max-menu-items 1000
        recentf-max-saved-items 1000)
(recentf-mode 1)

(file-name-shadow-mode 1)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

(setopt
 dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$"
 dired-guess-shell-alist-user
 '(("\\.\\(png\\|jpe?g\\|tiff\\)" "swayimg")
   ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv")
   ("\\.\\(pdf\\|cbz\\)" "sioyek")
   (".*" "d-stuff"))
 delete-by-moving-to-trash t
 dired-dwim-target t
 dired-kill-when-opening-new-dired-buffer nil
 )

(setopt wdired-allow-to-change-permissions t
        wdired-create-parent-directories t)

(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)
(setopt hippie-expand-verbose t
        hippie-expand-dabbrev-skip-space t)

(use-package vertico :init (vertico-mode)
  :bind ((:map vertico-map ("C-v" . vertico-scroll-up) ("M-v" . vertico-scroll-down)
               ("DEL" . vertico-directory-delete-char) ("M-DEL" . vertico-directory-delete-word) ))
  :custom ;;(vertico-count 5)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico-multiform :init (vertico-multiform-mode)
  :custom
  (vertico-multiform-commands
   '((load-theme grid) (consult-theme grid)
     (dired-goto-file flat)
     (consult-recoll buffer) (consult-dff unobtrusive)
     (embark-act grid)
	 (org-set-tags-command grid)
     ))

  (vertico-multiform-categories
   '((consult-grep buffer)
     (jinx grid) (embark-bindings grid) (embark-keybinding grid)
     (command flat) (file grid)
     (buffer flat (vertico-cycle . t)))))

(use-package consult :defer t
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history) ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-x" . consult-mode-command)
   ("C-x C-b" . ibuffer) ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x r b" . consult-bookmark) ("C-x p b" . consult-project-buffer)
   ("M-y" . consult-yank-pop) ;; Other custom bindings
   ;; M-g bindings (goto-map)
   ("M-g f" . consult-flycheck)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline) ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark) ("M-g i" . consult-imenu)
   ("M-g s" . consult-eglot-symbols)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-fd)
   ("M-s g" . consult-ripgrep) ("M-s m" . consult-man)
   ("M-s r" . consult-ripgrep)
   ("M-s i" . consult-info) ("M-s l" . consult-line)

   ("M-s e" . consult-isearch-history)
   ("M-s a" . consult-org-agenda)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history) ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line) ("M-s L" . consult-line-multi)
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history) ("M-r" . consult-history)

   :map org-mode-map
   ("M-g o" . consult-org-heading)
   ("M-g a" . consult-org-agenda))

  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (imenu-max-item-length nil)
  (consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number") )

(setopt completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))
        completion-category-defaults nil)

(use-package embark :defer t
  :bind
  (("C-." . embark-act) ("C-;" . embark-act-all)
   ("M-." . embark-dwim) ("C-h B" . embark-bindings)
   ("C-h b" . embark-bindings) ("C-h M" . embark-bindings-in-keymap)

   (:map embark-identifier-map
         ("!" . shell-command-on-region)
         ("ch" . color-name-to-hex)
         ("cr" . color-name-to-rgb)
         ("(" . insert-parentheses)
         ("[" . insert-pair-map) )
   (:map embark-url-map
         ("b" . browse-url-generic)
         ("e" . eww-open-in-new-buffer) )
   (:map embark-file-map
         ("b" . browse-url-of-dired-file))
   (:map embark-expression-map
         ("(" . insert-parentheses)
         ("[" . insert-pair-map))
   (:map embark-region-map
         ("(" . insert-parentheses)
         ("[" . insert-pair-map)
         ("=" . quick-calc))
   )

  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-help-key "?") (embark-confirm-act-all nil)
  )
(add-to-list 'display-buffer-alist `("\\*\\(Embark\\|Completions\\).*\\*"
									 nil (window-parameters (mode-line-format . none))))

(use-package embark-consult :defer t :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia :init (marginalia-mode))

(use-package corfu :init (global-corfu-mode)
  :bind (:map corfu-map ("SPC" . corfu-insert-separator))
  :config (corfu-history-mode) (corfu-echo-mode) (corfu-popupinfo-mode)
  (eldoc-add-command #'corfu-insert))

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  ("M-i" . cape-prefix-map)  ("C-'" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-abbrev))

(use-package tempel :hook (prog-mode . tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete) ("M-*" . tempel-insert)))

;; Refer: `tempo-define-template' for doc.
;;  • (s NAME) Inserts a named field.
;;  • (p/r PROMPT <NAME> <NOINSERT>) Insert an optionally named field with a prompt. The PROMPT is
;;  displayed directly in the buffer as default value. If NOINSERT is non-nil, no field is inserted.
;;  Then the minibuffer is used for prompting and the value is bound to NAME.

nix-mode
nix-ts-mode

(buildphase > "buildPhase= ''" n (p "Build Instructions") n " '';")
(checkPhase > "checkPhase= ''" n (p "") n " '';")
(configurephase > "configurePhase= ''" n (p "") n " '';")
(fixupphase > "fixupPhase= ''" n (p "") n " '';")
(distphase > "distPhase= ''" n (p "") n " '';")
(patchphase > "patchPhase= ''" n (p "") n " '';")
(unpackphase > "unpackPhase= ''" n (p "") n " '';")
(installCheckPhasephase > "installCheckPhasePhase= ''" n (p "") n " '';")
(installphase > "installphase= ''" n p " mkdir -p $out/bin" n> "for f in $(find . -executable -type f);" n> "do" n> "cp $f $out/bin" n> "done}" n> " '';")

(hmpkgs "{" n> "home.packages = with pkgs; [" n> (r "pkgnames") n> n> "];" n> "}")

(gitpackage "{ lib" n ", stdenv" n ", fetchFromGitHub" n ", " (p "inputs") n ", " (p "inputs") n "}:" n n>
            "stdenv.mkDerivation rec {" n> "pname = \"" (p "" pkgn nil) "\";" n> "version = \"" p "\";" n n>
      "src = fetchFromGitHub {" n> "owner = \"" (p "" own) "\";" n> "repo = \"" (s pkgn) "\";" n>
      "rev = \"" "v${version}" "\";" n> "sha256 = \"" "${lib.fakeSha256}" "\";" n> "};" n n>
      "nativeBuildInputs = [ " (p "makeWrapper") " ];" n n> "BuildInputs = [ " (p) " ];" n n>
      "meta = with lib; {" n>
      "homepage = \"" "https://github.com/" (s own) "/" (s pkgn) "\";" n>
      "description = \"" (p) "\";" n>
      "license = licenses." (p (completing-read "License: " '("agpl3" "asl20" "bsd1" "bsd2" "bsd3" "free" "gpl2" "gpl2Only" "gpl2Plus" "gpl3" "gpl3Only" "gpl3Plus" "isc" "lgpl21Only" "lgpl21Plus" "lgpl2Only" "lgpl2Plus" "lgpl3" "lgpl3Only" "mit" "mpl20" "ofl" "unfree"))) ";" n>
      "maintainers = with maintainers; [ " (s own) " ];" n>
      "platforms = platforms."
      (p (completing-read "Platform: " '("all" "allBut" "arm" "cygwin" "darwin" "freebsd" "gnu" "i686" "illumos" "linux" "mesaPlatforms" "mips" "netbsd" "none" "openbsd" "unix" "x86"))) ";" n> q "};" n> "}"
      )

(shellnix "with import <nixpkgs> {};" n>
          "pkgs.mkShell {" n n>
          "nativeBuildInputs = [ pkgs.bashInteractive ];" n n>
          "# EnvVars = The thung" n
          "# NIX_LD_LIBRARY_PATH = lib.makeLibraryPath [" n>
          "# pkgs" n> "# ];" n>
          "# NIX_LD = lib.fileContents \"${stdenv.cc}/nix-support/dynamic-linker\";" n n>
          "buildInputs = with pkgs; [" n>
          (p "pkgs names") n n>
          "];" n n>
          "shellHook = ''" n>
          (p "Command to Run") n>
          "'';" n
          "}"
          )

org-mode

(hugosite ":PROPERTIES:"  n ":EXPORT_FILE_NAME: " (p "simple-name") n ":EXPORT_DATE: " (format-time-string "%Y-%m-%d") n ":EXPORT_HUGO_DRAFT: false" n ":END:")
(readonly ":tangle-mode (identity #o444) :mkdirp yes" n)
(variablweb "  #+name: " (p "noweb-ref") n "#+begin_src " p n> r> n> "#+end_src" :post (org-edit-src-code))
(gitcollapse  "*** " p n "#+begin_html" n "<details>" n "<summary> " (p "heading")  " </summary>" n "#+end_html" n (p "link or any comments") n n "#+begin_html" n "</details>" n "#+end_html" n n)

(eval n> "#+name: " (p "name" fname) n> "#+begin_src " (p "C++" lang) n> r n> "#+end_src" :post (org-edit-src-code))

(pyeval n "  #+name: " (p "name" fname) n "#+begin_src python :tangle ./codes/python/"
        (s fname) ".py " n> r> n> "#+end_src" :post (org-edit-src-code))

(reval n "  #+name: " (p "name" fname) n "#+begin_src R :tangle ./codes/rstats/"
       (s fname) ".R " n> r> n> "#+end_src" :post (org-edit-src-code))

(jleval n "  #+name: " (p "name" fname) n "#+begin_src julia :tangle ./codes/"
        (s fname) ".jl " n> r> n> "#+end_src" :post (org-edit-src-code))

(labmanual "* " p n
           "** Introduction" n p n
           "** Aim and Objective" n n
           "** Procedure" n n
           "** Results" n n
           "** Conclusion" n n
           )

(latimg n "#+CAPTION: " p n
        "#+ATTR_LATEX: :height :float nil" n
        :post (org-insert-link))

(htmlimg n "#+caption: " p n
         "#+attr_html: :width 70%" n
         :post (org-insert-link))

(elisp "  #+name: " (p "name") n "#+begin_src emacs-lisp" n n "#+end_src" :post (org-edit-src-code))

(blogorg "#+title: " (p "title") n
         "#+date: [" (format-time-string "%Y-%m-%d %a") "]")

(contact
 (r "Person Name") n
 ":PROPERTIES: " n
 ":EMAIL: " p n
 ":URL: " p n
 ":MOBILE: " p n
 ":WORKPHONE: " n
 ":COMPANY: " n
 ":CITY: " p n
 ":BIRTHDAY: " p n
 ":ICON:" n
 ":ADDRESS:" n
 ":ITOLDTHEM_EMAIL: " n
 ":ITOLDTHEM_ADDRESS: " n
 ":ITOLDTHEM_PHONE: " n
 ":IGNORE: " n
 ":CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") n
 ":END:" n
 )

(readlist
 (r "Book/Content") n
 ":PROPERTIES: " n
 ":AUTHOR: " n
 ":YEAR: " n
 ":PROS: " n
 ":LINK: " p n
 ":ID: " (org-id-new) n
 ":END:" n
  )

(pdforg
 "#+title: " p n
 "#+author: " p n
 "#+options: toc:nil num:nil" n
 "#+date: " (format-time-string "%Y-%m-%d") nnn
 "* Introduction" n p)


(shbin
 "#+begin_src sh :shebang \"#!/usr/bin/env bash\" :tangle bin/" p
 n n
 "#+end_src"
 :post (org-edit-src-code))

(revjs n
"#+title: " p n
"#+author: " p n
"#+reveal_miscinfo: " p n n
"#+REVEAL_ROOT: /home/idlip/learn/revjs/" n
"#+OPTIONS: reveal_width:1920 reveal_height:1200" n
"#+OPTIONS: toc:nil num:nil" n
"#+REVEAL_MIN_SCALE: 0.2" n
"#+REVEAL_MAX_SCALE: 1.0" n
"#+OPTIONS: reveal_progress:nil" n
"#+REVEAL_EXTRA_OPTIONS: navigationMode: 'linear', controls: false" n
"#+REVEAL_MARGIN: 0.1" n
"#+REVEAL_THEME: white" n
"#+REVEAL_TRANS: none" n
"#+reveal_single_file: t" n
"#+EXCLUDE_TAGS: noexport" n
"#+reveal_extra_css: ./local.scss" n n p
)

markdown-mode

(gitcollapse "## " (p "Heading") n n "<details>" n n
       "<summary>" (p "Sub Heading")  "</summary>" n n
       (r "Insert Link or comments") n n "</details>")
(bolditalics "***" p "***")

(androidfoss "* [**" (p "pname") "**](" (p "Git") ") <sup>**[[F-Droid](" (p "Fdroid") ")]**</sup>")

(srcblock (call-interactively #'markdown-insert-gfm-code-block))
(src "'" p "'")
(unorderlist "- " (p "First") n> "- " (p "Second") n> "- " (p "Third"))
(orderlist "1. " (p "First") n> "2. " (p "Second") n> "3. " (p "Third"))
(insertimage (call-interactively #'markdown-insert-image))
(insertlink (call-interactively #'markdown-insert-link))
(hugotitle "+++" n "title = " (p "title") n "date = " (format-time-string "%Y-%m-%d") n "tags = [ " (p "tag1, tag2 ") "]" n "draft = false" n "+++")
(h1 "# " p " #")
(h2 "## " p " ##")
(h3 "### " p " ###")
(h4 "#### " p " ####")
(inserttable (call-interactively #'markdown-insert-table))

emacs-lisp-mode

(modconfig ";;; " (c-get-current-file) ".el --- " (p "description") " -*- lexical-binding: t -*-" n
           ";;; Commentary:" n n ";;; Code:" n n n r> n n "(provide '" (c-get-current-file) ".el)" n ";;; " (c-get-current-file) ".el ends here" )
(autoload ";;;###autoload")
(lambda "(lambda (" p ")" n> r> ")")
(defvar "(defvar " p "\n  \"" p "\")")
(defvar-local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(defface "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(defgroup "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(defmacro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(defalias "(defalias '" p " '" p ")")
(defun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(defcustom "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(if-let "(if-let (" p ")" n> r> ")")
(when-let "(when-let (" p ")" n> r> ")")
(if-let* "(if-let* (" p ")" n> r> ")")
(when-let* "(when-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " p n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(obsolete-fun "(define-obsolete-function-alias" n> p n> p n> "\"" p "\")")
(obsolete-var "(define-obsolete-function-alias" n> p n> p n> "\"" p "\")")

(feeds "(\"" (p "label") "\" \"" (p "url") "\" starttime)")

python-ts-mode python-mode

(if "if " p ":" p % "else:" n> p)

;; https://issues.genenetwork.org/topics/using-pdb-to-troubleshoot.html
(post-mortem "try:" n> "return " p n> "except Exception:" n> "import pdb; pdb.post_mortem()")

;; subprocess
;; https://realpython.com/python-subprocess/
(subprocess-su "subprocess.run([\"ls\", \"-l\", \"/dev/null\"], capture_output=True)")
(subprocess-sl "subprocess.run([" p "])")
(subprocess-sc "completed_process = subprocess.run(" n> "[" p "]," n> "check=True" n ")")
(subprocess-so "subprocess.run(["python", "timer.py", "5"], timeout=1)")
(subprocess-st "subprocess.run([\"ls /usr/bin | grep pycode\"], shell=True)")

;; json
(json-w "with open(\"data_file.json\", \"w\") as write_file:" n> "json.dump(data, write_file)")
(json-r "with open(\"data_file.json\", \"r\") as read_file:" n> "data = json.load(read_file)")
(json-get-loads "response = requests.get(\"https://jsonplaceholder.typicode.com/todos\")"
    n
    "todos = json.loads(response.text)")

;; pathlib
(pc "pathlib.Path.cwd()")
(ph "pathlib.Path.home() / 'python' / 'scripts' / 'test.py'")

(type "type(" p ")")
(list "list(" p ")")
(len "len(" p ")")
(str "str(" p ")")
(dir "dir(" p ")")

;; files
(withopen-read "with open('data.txt', 'r') as f:" n> "data = f.read()")
(withopen-write "with open('data.txt', 'w') as f:" n> "data = 'some data to be written to the file'" n> "f.write(data)")

(init "def __init__(self, " p "):")

(al "__all__ = [" n> p n "]")

(pa "parser.add_argument(")

;; types
(typing-union " Union[" p ", " p "]:")
(typing-optional " -> Optional[" p "]:" n>)
(typing-sequence " Sequence[" p "]")
(typing-string " -> str:" n>)
(typing-int " -> int:" n>)
(typing-typevar "T = TypeVar('T')" n)

(asserts "assert " p)

(shebang "#!/usr/bin/env python")

(classmethod "@classmethod" n "def " p "(cls, " p "):" n>)

(class "class " p ":" n>)
(class-pass "class " p ":" n> "pass")
(class-employee "class Employee:" n> "def __init__(self, id, name):" n> "self.id = id" n> "self.name = name")
(class-rectangle "class Rectangle:" n>
                 "def __init__(self, length, height):" n>
                 "self._length = length" n> "self._height = height" n> n
                 "@property" n>
                 "def area(self):" n>
                 "return self._length * self._height" n> n
                 "def resize(self, new_length, new_height):" n>
                 "self._length = new_length" n>
                 "self._height = new_height" n>)


(dataclass "@dataclass" n> "class " p ":" n>)


(main "if __name__ == '__main__':" n> "main()")

(lambda "lambda " p ": "  p)

;; comprehensions

(list-comprehension-fruits "fruits = []"
    n
    "newlist = [x for x in fruits if "a" in x]")

(list-comprehension-in "[ " p "for " p " in " p "]")
(list-comprehension-squares "squares = [i * i for i in range(" p ")]")
(list-comprehension-sentence "sentence = 'the rocket came back from mars'"
    n
    "vowels = [i for i in sentence if i in 'aeiou']")
(lp "original_prices = [1.25, -9.45, 10.22, 3.78, -5.92, 1.16]"
    n
    "prices = [i if i > 0 else 0 for i in original_prices]")

(ld "quote = 'life, uh, finds a way'"
    n
    "unique_vowels = {i for i in quote if i in 'aeiou'}")
(lds "squares = {i: i * i for i in range(10)}")

;; debugging
(lo "logger = logging.getLogger(" p ")")
(b "breakpoint()")
(in "import code; code.interact(local=locals())")
(pu "from pudb import set_trace; set_trace()")

;; is this one really useful?
(r "return " p)
(s "self." p)
(se "self." p " = " p)

;; docstrings
(docstring-google "\"\"\"" p n "Args:" n> p "Returns:" n "\"\"\"")
(docstring-module "\"\"\"" p n "\"\"\"")

;; regex
(regex-split-word "re.split(r'\W+', " p)
(regex-split-word-one "re.split(r'\W+', " p ", 1)")
(regex-split-ignorecase "re.split('[a-f]+', " p "flags=re.IGNORECASE)")

;; isinstance
(isinstance-bytes "if isinstance(line, bytes):" n>)

;; dicts
(dicts-print "dictionary = {\"raj\": 2, \"striver\": 3, \"vikram\": 4}"
    n
    "print(dictionary.values())")

;; pytest
(pytest-fixture "@pytest.fixture(params=TERMS_LIST)" n
     "def poly(request):" n>
     "return Polynomial(request.param)")

;; mocking
(mock-return "mock = Mock()" n "mock.__str__ = Mock(return_value='wheeeeee')")

;; numpy

;; csv

(csv "import csv" n
     "with open('eggs.csv', newline='') as csvfile:" n>
     "spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')" n>
     "for row in spamreader:" n>
     "print(', '.join(row))")

;; sets

;; scipy

;; urllib
(ul "from urllib.request import urlopen" n
    "with urlopen('http://worldtimeapi.org/api/timezone/etc/UTC.txt') as response:" n>
    "for line in response:" n>
    "line = line.decode()             # Convert bytes to a str" n>
    "if line.startswith('datetime'):" n>
    "print(line.rstrip()) ")

;; datetime
(now "now = date.today()")
(nstf "now.strftime(\"%m-%d-%y. %d %b %Y is a %A on the %d day of %B.\")")

;; sqlite
(sqlite-connect "con = sqlite3.connect('example.db')")
(sqlite-create "cur = con.cursor()" n
     "cur.execute('''CREATE TABLE stocks (date text, trans text, symbol text, qty real, price real)''')" n
     "cur.execute(\"INSERT INTO stocks VALUES ('2006-01-05','BUY','RHAT',100,35.14)\")" n
     "con.commit()" n
     "con.close()" n)


;; tempfile
(tw "fp = tempfile.TemporaryFile()" n
    "fp.write(b'Hello world!')" n
    "fp.seek(0)" n
    "fp.read()" n
    "fp.close()")

(tc "with tempfile.TemporaryDirectory() as tmpdirname:" n>
    "print('created temporary directory', tmpdirname)")

;; https://docs.python.org/3/library/glob.html
(gr "glob.glob('**/*.txt', recursive=True)")

;; time
(time "from time import gmtime, strftime" n
      "strftime(\"%a, %d %b %Y %H:%M:%S +0000\", gmtime())")

;; timeit
(teit "import timeit"
     "timeit.timeit('\"-\".join(str(n) for n in range(100))', number=10000)")

;; ast
(ast "print(ast.dump(ast.parse('123', mode='eval'), indent=4))")

;; icecream
(icecream "ic(" p ")")
(icecream-install "from icecream import install" n "install()")
(icecream-import "from icecream import ic; ic(" p ")")

;; sys
(sys-getsizeof "print(sys.getsizeof(" p "))")

;; https://stackoverflow.com/questions/6579496/using-print-statements-only-to-debug
(logging "import logging, sys" n
     "logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)" n
     "logging.debug('A debug message!')" n
     "logging.info('We processed %d records', len(processed_records))")

;; https://github.com/AbhijithAJ/clrprint
(clrprint "from clrprint import *" n
     "clrprint('ERROR:', information,clr=['r','y'], debug=True)")

;; what am I doing with this?
(csl "clr = clr.strip().lower()")

(fixtures "@pytest.fixture" n
          "def example_fixture():" n> "return 1" n n
          "def test_with_fixture(example_fixture):" n>
          "assert example_fixture == 1")

fundamental-mode

(datime (format-time-string "%Y-%m-%d %A %d %B %Y"))
(24time (format-time-string "%H:%M - "))
(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rebuild "nixos-rebuild switch --flake ~/d-git/d-nix#host --sudo")
(gc "git clone --depth=1")


bash-ts-mode conf-mode
(func "function " (p "fname") "() {" n> p n> n "}")

(comm "######" (make-string (length str) ?#) "######" n
     "##### " (s str)                       " #####" n
     "######" (make-string (length str) ?#) "######")

(hinfo "${c_yellow}" p "${c_reset}" n p)

css-mode css-ts-mode html-mode html-ts-mode

(var "var(--" p ")")

(tag "<" (p "name" tg) ">" n>
     r> n> "</" (s tg) ">")

(use-package ibuffer :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-set-filter-groups-by-mode))

(setopt uniquify-buffer-name-style 'forward)

(setopt isearch-lazy-count t
        search-whitespace-regexp ".*?")

(context-menu-mode 1)
(setopt mouse-autoselect-window t)

(winner-mode 1)

(setopt
 scroll-step 3
 scroll-margin 0
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(1 ((control) . 1)  ((shift) . 2) ((meta) . 3))
 scroll-conservatively 101
 scroll-preserve-screen-position t
 pixel-scroll-precision-interpolate-page t)

(pixel-scroll-precision-mode 1)

;; (bind-keys ("C-v" . View-scroll-half-page-forward) ("M-v" . View-scroll-half-page-backward))
(bind-keys ("C-v" . pixel-scroll-interpolate-down) ("M-v" . pixel-scroll-interpolate-up))

(setq repeat-exit-timeout 2) (put 'other-window 'repeat-map nil)
(repeat-mode 1)

(with-eval-after-load 'zone
  (zone-when-idle (* 60 5)))

(use-package info :ensure nil
  :config
  (add-to-list 'Info-additional-directory-list "~/learn/info-manuals/"))

(use-package magit :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package ediff :ensure nil
  :custom (ediff-window-setup-function 'ediff-setup-windows-plain "Do actions from single frame"))

(use-package diff-mode :ensure nil
  :custom
  (diff-default-read-only t)
  (diff-font-lock-syntax 'hunk-also))

(use-package envrc :defer 2
  :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

(use-package esh-mode :ensure nil
  :hook (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)))
  :custom
  (eshell-hist-ignoredups t)
  (eshell-kill-processes-on-exit 'ask)
  (eshell-aliases-file (expand-file-name "eshell/alias" user-emacs-directory)))

(use-package em-hist :ensure nil
  :bind ((:map eshell-hist-mode-map
               ("M-s" . nil)
               ("M-s r" . consult-ripgrep)
               ("M-s s" . consult-history)))
  :custom
  (eshell-buffer-maximum-lines 10000) (eshell-history-size 10000))

(add-to-list
 'display-buffer-alist
 '("\\*\\(shell\\|term\\|.*eshell\\|.*eat\\|help\\|compilation\\|Async Shell Command\\|Occur\\|xref\\).*\\*"
   (display-buffer-reuse-window display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (post-command-select-window . t)
   (window-height . 0.3)))

(setq comint-pager "cat")
(setenv "MANPAGER" "cat")
(setopt xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package flycheck :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 3)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-display-errors-delay 0.25))

(use-package reformatter
  :hook
  (python-ts-mode . ruff-format-on-save-mode)
  (nix-mode . alejandra-format-on-save-mode)
  (ess-r-mode . styler-format-on-save-mode)
  (bash-ts-mode . shell-format-on-save-mode)
  ;; (nix-ts-mode . nixfmt-rfc-format-on-save-mode)

  :config
  (reformatter-define ruff-check-fix :program "ruff"
    :args (list "check" "--fix" "--stdin-filename" input-file "-"))
  (reformatter-define ruff-format :program "ruff"
    :args (list "format" "--stdin-filename" input-file "-"))

  (reformatter-define pyblack-format :program "black"
    :args (list "black" "-"))

  (reformatter-define shell-format :program "shfmt" )

  (reformatter-define nixfmt-rfc :program "nixfmt")

  (reformatter-define eslint-format :program "eslint"
	:args (list "--fix-dry-run" "--stdin" "--stdin-filename" buffer-file-name)
	:stdin t :stdout nil )

  (reformatter-define prettier-format :program "prettier"
	:args (list "--stdin-filepath" buffer-file-name))

  (reformatter-define jinja-format :program "djlint"
	:args '("--reformat" "--quiet" "-"))
  )

(use-package eglot :defer t :ensure nil :unless d/on-droid
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  ;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
  ;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  )

(setopt xref-search-program 'ripgrep
        grep-command "rg ")

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind ("M-#" . compile) ; M-! M-# M-& M-\
  :custom
  (shell-command-switch "-c") ;; -i
  (compilation-scroll-output t)
  )

(use-package project :ensure nil
  :custom (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-switch-use-entire-map t))

(use-package treesit :ensure nil
  :mode
  (("\\.tsx\\'" . tsx-ts-mode)
   ("\\.yaml\\'" . yaml-ts-mode) ("\\.toml\\'" . toml-ts-mode) ("\\.jsonrc\\'" . json-ts-mode)
    ("\\.json\\'" .  json-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode)
   ("\\.Dockerfile\\'" . dockerfile-ts-mode)
   ("\\.sh\\'" . bash-ts-mode))

  :custom
  (treesit-font-lock-level 4)
  (standard-indent 2)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode) (c++-mode . c++-ts-mode) (nix-mode . nix-ts-mode)
     (csharp-mode . csharp-ts-mode) (css-mode . css-ts-mode)
     (java-mode . java-ts-mode) (js-mode . js-ts-mode) (html-mode . mhtml-ts-mode)
     (js-json-mode . json-ts-mode) ;; (org-mode . org-ts-mode) ;; not mature yet
     (python-mode . python-ts-mode) (julia-mode . ess-julia-mode)
     (typescript-mode . typescript-ts-mode) (sh-mode . bash-ts-mode) (shell-script-mode . bash-ts-mode)
     (ruby-mode . ruby-ts-mode) (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode) (yaml-mode . yaml-ts-mode))))

(use-package devdocs-browser
  :bind ("C-c d v" . devdocs-browser-open-in)
  :custom
  (devdocs-browser-major-mode-docs-alist
   '((c++-ts-mode "cpp")
     (c-ts-mode "c")
     (go-ts-mode "go")
     (python-ts-mode "Python")
     (emacs-lisp-mode "elisp")
     (rust-ts-mode "rust")
     (cmake-mode "CMake")))

  (devdocs-browser-highlight-lang-mode-alist
   '(("c" . c-ts-mode)
     ("cpp" . c++-ts-mode)
     ("py" . python-ts-mode)
     ("bash" . bash-ts-mode)
     ("shell" . bash-ts-mode)
     ("python" . python-ts-mode)))

  (devdocs-browser-data-directory (expand-file-name "var/devdocs" user-emacs-directory)))

(use-package prog-mode :ensure nil :hook (prog-mode . hs-minor-mode) (prog-mode . outline-minor-mode)
  :custom (tab-width 4))

(use-package hi-lock
  :hook (prog-mode . highlight-marker-mode)
  :config
  (define-minor-mode highlight-marker-mode
    "A minor mode that toggles a chunk of functionality."
    :init-value nil
    (let ((regexps
           `((, (rx (or "FIXME:" "fixme:")) . hi-red-b)   ; fixme
             (, (rx (or "NOTE:" "note:")) . hi-blue)      ; note
             (, (rx (or "TODO:" "todo:")) . hi-green)     ; todo
             (, (rx (group (repeat 8 digit)) "T") . org-date) ; date 20250228T  (denote style)
             (, (rx "T" (group (repeat 6 digit))) . org-modeline-clock)))) ; time T201020

      (if highlight-marker-mode
          (dolist (regexp regexps)
            (highlight-regexp (car regexp) (cdr regexp)) )
        (dolist (regexp regexps)
          (unhighlight-regexp (car regexp)))))
    )
  )

(electric-pair-mode 1)

;; credits to oantolin's config
(bind-keys :prefix-map insert-pair-map
           :prefix "M-["
           ([t] . insert-pair))

(define-advice insert-pair (:filter-args (args) numeric-prefix)
  (cons (prefix-numeric-value (car args)) (cdr args)))

(use-package paren :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-highlight-openparen t) (show-paren-context-when-offscreen t))

(use-package colorful-mode :unless d/on-droid
  :config (global-colorful-mode))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :custom
  (proced-enable-color-flag t)
  (proced-format '(user start time pcpu pmem rss args))
  (proced-sort 'pmem)
  (proced-auto-update-flag t))

(use-package python :ensure nil
  :bind (:map python-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom
  ;; (python-forward-sexp-function nil)
  (python-indent-guess-indent-offset-verbose nil))

;; hacky way to run python tools in any dir without envrc or anything
(defun d/dev-uvx-command ()
  "Prompt for package and command, to run dev environment."
  (interactive)
  (let ((pkg (read-string "Enter package: "))
        (cmd (read-string "Enter Command Args: ")))
    (setq d/dev-uvx-command
          (append '("uvx" "--from") (list pkg) (split-string cmd)))
    (message "Set d/dev-uvx-command to: %S" d/dev-uvx-command)))

(use-package ess :defer t :unless d/on-droid
  :custom
  (ess-use-company nil)
  (ess-ask-for-ess-directory t)
  (ess-style 'RStudio)
  (ess-eldoc-show-on-symbol t)

  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t) (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t) (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t) (ess-R-fl-keyword:%op% . t)
     (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t)))

  (inferior-R-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t) (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t) (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:messages . t) (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t) (ess-fl-keyword:matrix-labels . t)
     (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t))))

(use-package ess-r-mode :unless d/on-droid
  ;; :hook (ess-r-mode . (lambda () (flycheck-mode 0)))
  :bind (
         (:map ess-mode-map ("C-;" . ess-insert-assign))
         (:map inferior-ess-r-mode-map ("C-;" . ess-insert-assign)))
  :custom
  (ess-indent-with-fancy-comments nil))

(unless d/on-droid (use-package nix-ts-mode) )

(use-package js :ensure nil :mode ("\\.jsx\\'" . js-jsx-mode)
  ("\\.vue\\'" . js-ts-mode))

(use-package verb :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package ess-julia :unless d/on-droid
  :hook (ess-julia-mode . (lambda () (setq-local devdocs-browser-active-docs '("Julia"))))
  :bind (:map ess-julia-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom (inferior-julia-args "--color=yes" "You get color in julia inferior process"))

;; access phone storage as default
;; Better is to symlink file to ~/ itself

;;(setq default-directory "/storage/emulated/0/")

(when d/on-droid
  (custom-set-variables
   '(touch-screen-precision-scroll t)
   '(touch-screen-display-keyboard t)
   '(browse-url-android-share t)
   '(touch-screen-enable-hscroll nil "Avoid horizontal scroll that stutters"))

  ;; credits to https://github.com/danijelcamdzic/dotemacs/
  (setq display-buffer-alist
        '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))
  )

(use-package doc-view :ensure nil
  :bind ((:map doc-view-mode-map
               ("M-g M-g" . doc-view-goto-page)
               ("<f8>" . doc-view-presentation)
               ("j" . doc-view-next-line-or-next-page)
               ("k" . doc-view-previous-line-or-previous-page)
               ("C-v" . doc-view-scroll-up-or-next-page)
               ("M-v" . doc-view-scroll-down-or-previous-page)
               ("I" . d/doc-view-theme)
               ))
  :hook (doc-view-mode . (lambda () (setq-local pixel-scroll-precision-mode nil)))
  :custom-face (doc-view-svg-face ((t (:background "#000000" :foreground "#ffffff"))))
  :custom
  (doc-view-continuous t)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 10))

(defun d/doc-view-theme ()
  "Toggle between dark and reading mode in Doc-view buffer."
  (interactive)
  (let ((choice (completing-read "theme Color: " '("black" "reader" "white" "tokyonight") nil t)))
    (cond
     ((string= "black" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#050505" :foreground "#ffffff"))
     ((string= "reader" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#edd1b0" :foreground "#000000"))
     ((string= "tokyonight" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#24283b" :foreground "#fefefe"))
     ((string= "white" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#fefefe" :foreground "#000000"))))
  (doc-view-next-page) (doc-view-previous-page))

(unless d/on-droid
  (use-package reader :demand t
    :load-path "~/learn/emacs-reader"
    :config (reader-global-dark-mode 1)
    (require 'reader-saveplace)
    (require 'reader-bookmark)
    (require 'reader-outline)
    ))

(use-package nov :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . nov-imenu-setup)
  :custom
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))))

(use-package gnus
  :hook
  (gnus-group-mode . gnus-topic-mode)
  (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
  :bind (("C-c d g" . gnus))
  :custom
  (gnus-home-directory (expand-file-name "feeds/gnews" user-emacs-directory))
  (gnus-directory (expand-file-name "news" gnus-home-directory))
  (gnus-cache-directory (nnheader-concat gnus-directory "cache/"))
  (message-directory (expand-file-name "mail" gnus-home-directory))
  (gnus-startup-file (expand-file-name "newsrc" gnus-home-directory))
  (gnus-message-archive-group '((format-time-string "sent.%Y")))
  (gnus-article-save-directory (expand-file-name "saved" gnus-home-directory))
  (gnus-widen-article-window t)
  (gnus-completion-styles completion-styles)

  (gnus-select-method
   '(nnnil ""))

  (gnus-secondary-select-methods
   '((nntp "feedbase"
           (nntp-open-connection-function nntp-open-tls-stream) ; feedbase does not do STARTTLS (yet?)
					 (nntp-connection-timeout 5)
           (nntp-port-number 563) (nntp-address "feedbase.org") )
     ;; (nntp "gwene" (nntp-address "news.gwene.org"))
		 ;;  		 (nntp-open-connection-function nntp-open-network-stream) (nntp-connection-timeout 5) )
     ;; (nntp "news.gmane.io"
		 ;;  		 (nntp-open-connection-function nntp-open-network-stream)
		 ;;  		 (nntp-connection-timeout 5))
	   (nntp "yhetil" (nntp-address "news.yhetil.org"))
     (nnrss "")
     ))

  ;; refer: https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-score))
  (gnus-use-cache t)
  (gnus-thread-hide-subtree t)
  ;; (gnus-activate-level 2)

  (gnus-auto-center-summary nil)
  (gnus-asynchronous t)

  ;;; credits - https://libreddit.kavin.rocks/r/emacs/comments/1cfv84p/tipps_on_gnus_summary_formatting/ - u/ballfresno
  ;; (gnus-summary-line-format "%1{%U%R%O %4k%} %3{%&user-date;%*%ud│%}%I%(%-16,16f%) %4{%s%}\n")
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . " %k:%M")
     ((+ (gnus-seconds-today) (* 24 3600)) . " %l %p")
     (604800 . " %a")
     (31536000 . "%e %b")
     (t . " %Y")))

  (gnus-summary-line-format
   (concat
    "%0{%U%R%z%}"
    "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
    "  "
    "%4{%-20,20f%}"               ;; name
    "  "
    "%3{│%}"
    " "
    "%1{%B%}"
    "%s\n"))

  (gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")

  (gnus-topic-line-format "%i[ %(%{%n%}%) -- %g | %A ]%v\n")

  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

      ;;; credits - https://github.com/jbranso/.emacs.d/blob/master/lisp/init-gnus.org
  (gnus-sum--tree-indent " ")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-false-root "○ ")
  (gnus-sum-thread-tree-single-indent "◎ ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")


  ;; Yay (seen here: `https://github.com/cofi/dotfiles/blob/master/gnus.el')
  ;; (gnus-cached-mark ?󰃨)
  ;; (gnus-canceled-mark ?󱞐)
  ;; (gnus-del-mark ?󰆴)
  ;; gnus-dormant-mark ?⚐
  ;; (gnus-expirable-mark ?♻)
  ;; (gnus-forwarded-mark ?)
  ;; gnus-killed-mark ?☠
  ;; gnus-process-mark ?⚙
  ;; (gnus-read-mark ?󰑇)
  ;; (gnus-recent-mark ?✩)
  ;; (gnus-replied-mark ?↺)
  ;; (gnus-unread-mark ?)
  ;; gnus-unseen-mark ?★
  ;; gnus-ticked-mark ?⚑

  :config
  (setq nnrss-group-alist '( ("manga" "https://nyaa.si/?page=rss&c=3_1&f=0") ) )
  (gnus-demon-add-handler 'gnus-group-save-newsrc 5 t) ;; minutes
  (gnus-demon-init)
  )

(use-package gnus-group
  :bind
  (:map gnus-group-mode-map
        ("M-g" . goto-map)
        ("M-&") ("M-n") ("M-p") ; Gnus taking over useful keybindings
        ("C-&" . gnus-group-universal-argument)))

(use-package gnus-art
  :bind
  (:map gnus-article-mode-map
        ("C-h b") ; come on Gnus, that key binding is sacred!
        ("M-&")   ; also pretty important
        ("C-&" . gnus-summary-universal-argument)
        ("M-g" . goto-map)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)))

(use-package gnus-sum
  :bind
  (:map gnus-summary-mode-map
        ("-" . gnus-summary-hide-thread)
        ("+" . gnus-summary-show-thread)
        ("M-&")   ; also pretty important
        ("C-&" . gnus-summary-universal-argument)
        ("M-g" . goto-map) ; rescan is also on Z G, and I use that prefix a lot!
        ("M-a" . gnus-symbolic-argument)))

(use-package gnus-srvr
  :bind
  (:map gnus-server-mode-map
        ("q" . quit-window)))

(setopt user-mail-address "zororg@tilde.green" ;; you can mail me to discuss anything on emacs ;)
        user-full-name "Zororg")

(setq fast-read-process-output nil)
(setq gnus-search-use-imap t)


(use-package gnus :disabled t
  :unless d/on-droid
  :config
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "protonmail"
                        (nnimap-stream plain)
                        (nnimap-address "127.0.0.1") ;; hydroxide
                        (nnimap-server-port 1143)))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "tilde-green"
                        (nnimap-stream plain)
                        (nnimap-address "imap.tilde.green")
          ))
  )

(use-package smtpmail :unless d/on-droid
  ;; :after gnus
  :custom
  (smtpmail-default-smtp-server "smtp.tilde.green")
  (smtpmail-smtp-server "smtp.tilde.green")
  (smtpmail-smtp-service 465)
  (starttls-use-gnutls t)
  (send-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (mail-from-style 'angles)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t))

(setopt message-server-alist '(("zororg@tilde.green"
                                . "smtp smtp.tilde.green 465 zororg")))

(use-package url :ensure nil
  :custom (url-privacy-level 'high) ;; reddit/SO does not like it 'paranoid
  :config (url-setup-privacy-info))

(use-package shr :ensure nil :demand t
  :custom (shr-bullet "⦿ ") (shr-width 100))

(use-package eww :ensure nil :demand t
  :hook
  (eww-mode . variable-pitch-mode)
  (eww-after-render . (lambda () (d/eww-readable) (setq-local line-spacing '0.4)))
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://leta.mullvad.net/search?engine=brave&q=")
  :config
  (defun d/eww-readable ()
    "Use more opinionated `eww-readable'.

Set width is set to `current-fill-column'.  Adjust size of
images."
    (interactive)
    (let ((shr-width (current-fill-column))
          (shr-max-image-proportion 0.35))
      (eww-readable)))

  (setq d/search-engines
        '(
          ("go google" . "https://google.com/search?q=%s")
          ("ddg duckduckgo" . "https://duckduckgo.com/?q=%s")
          ("yt invidious" . "https://yewtu.be/search?q=%s")
          ("sxng searxng" . "https://searxng.world/search?q=%s")
          ("mg marginalia rss" . "https://marginalia-search.com/search?query=")
          ("feedle rss" . "https://feedle.world/search?query=")
          ("lists gnu mail" . "https://yhetil.org/")
          ))

  (defun d/search-eww (term)
    "Search for a term using an engine."
    (interactive "MTerm: ")
    (let* ((url
            (cdr (assoc (completing-read "Engine: " d/search-engines) d/search-engines))))
      (if (equal url nil) (message "Error: search engine unknown.")
        (eww (format url (url-hexify-string term))))))
  )

(use-package browse-url :ensure nil :unless d/on-droid
  :config ;; browser script
  (setopt browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff"
          browse-url-secondary-browser-function 'browse-url-default-browser))

(use-package ox-hugo :unless d/on-droid :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
  See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("w" "Website Organize"))
  (add-to-list 'org-capture-templates
               '("wt" "website Todo" entry (file+headline "~/d-git/d-site/README.org" "Ideas - TODO")
                 "* TODO %?\n  SCHEDULED:%T\n " :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("ww" "website work"
                 entry
                 (file+olp "~/d-git/d-site/org-mode/source.org" "Posts")
                 ;; (function org-hugo-new-subtree-post-capture-template)
                 "* TODO %^{Post Title}
:PROPERTIES:
:EXPORT_FILE_NAME: %^{File name}
:EXPORT_DATE: %(org-timestamp-inactive)
:END:
%?\n\n" )))

(use-package mpc
  :bind (("C-c d m" . mpc)
         (:map mpc-mode-map
               ("SPC" . mpc-playlist-add)
               ("RET" . mpc-select)
               ("C-k" . mpc-playlist-delete)
               ("m" . mpc-select-dwim)
               ("C-r" . mpc-songs-search)
               ("f" . mpc-ffwd)
               ("b" . mpc-rewind)
               ))
  :custom (mpc-browser-tags '(Title))
  :config
  (defun mpc-select-dwim ()
    (interactive)
    (mpc-select-toggle)
    (next-line)
    ))

(defcustom d/font-size (if d/on-droid 170 120)
  "Default font size based on the system.")

;; Dont worry about the font name, I use fork of Iosevka font

;; Set reusable font name variables
(defcustom d/fixed-pitch-font (if d/on-droid "IBM Plex Mono" "Maple Mono NF")
  "The font to use for monospaced (fixed width) text.")

(defcustom d/variable-pitch-font (if d/on-droid "IBM Plex Serif" "Inter")
  "The font to use for variable-pitch (documents) text.")

(use-package faces :ensure nil
  :custom-face
  (variable-pitch ((t (:family ,d/variable-pitch-font :height 1.1 :weight normal))))
  (fixed-pitch ((t (:family ,d/fixed-pitch-font :weight normal))))
  (default ((t (:family ,d/fixed-pitch-font :height ,d/font-size :weight normal)))))

(global-font-lock-mode 1)

(use-package modus-themes :ensure nil :demand t
  :init (require-theme 'modus-themes)
  :custom
  (modus-themes-italic-constructs t) (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)

  (modus-themes-headings
   '((1 . (variable-pitch 1.5))
     (2 . (1.3))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.1))))

  (modus-vivendi-palette-overrides
   '(
     (bg-main     "#000000") (bg-dim      "#111111")
     (bg-active   "#222222") (bg-inactive "#333333")

     (fg-main     "#ffffff")
     (fg-dim      "#b4aeae")

     (cursor      "#00ffff")

     (fg-heading-1  "#ab82ff")
     (fg-heading-2  "#fab387")
     (mail-subject  "#6ae4b9")
 
     (bg-completion "#2e8b57") (bg-region     bg-completion) (fg-region unspecified)

     (bg-tab-bar bg-main) (bg-tab-current bg-active) (bg-tab-other bg-dim)
     (fringe unspecified)
     (bg-mode-line-active bg-dim)
     (bg-line-number-active  bg-main) (bg-line-number-inactive  bg-main)
     (fg-line-number-active fg-dim) (fg-line-number-inactive border)
     (border-mode-line-active unspecified) (border-mode-line-inactive unspecified)
     ))

  :config
  (load-theme 'modus-vivendi t))

(setq-default
 d/mode-line-format mode-line-format
 ;; mode-line-format nil
 )

;; credits minad in reddit
(defmacro +diminish (mode)
  `(cl-callf2 assq-delete-all ',mode minor-mode-alist))

;; (+diminish abbrev-mode)

(use-package d/hide-mode-line :ensure nil :no-require t
  :bind ([f9] . d/hide-mode-line-mode)
  :init
  (define-minor-mode d/hide-mode-line-mode
    "The void space to hide mode-line."
    :lighter "Vanish" :init-value nil
    (setq mode-line-format
          (if d/hide-mode-line-mode
              nil
			;; d/mode-line-format
			(default-value 'mode-line-format)
			))
    (redraw-display))
  :hook (help-mode nov-mode)
  )

(use-package olivetti :defer t :custom (olivetti-body-width 100)
  :hook (org-mode text-mode Info-mode helpful-mode ement-room-mode gnus-group-mode eww-mode
                  gnus-article-mode sdcv-mode nov-mode elfeed-show-mode markdown-mode))

(use-package org :ensure nil :defer t
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)

  :bind (
         ("C-c t i" . org-timer-set-timer)

         (:map org-mode-map
               ("C-c l" . org-store-link)
               ("M-n" . org-shiftdown)
               ("M-p" . org-shiftup)
               ))

  :custom
  (org-ellipsis " ...")
  (org-use-sub-superscripts '{})
  (org-log-done 'note) (orgl-log-reschedule 'note)
  (org-log-into-drawer t)
  (org-export-exclude-tags '("noexport" "ignore") "excludes these tagged heading from export")
  (org-latex-compiler "lualatex" "Lualatex is fast and gets custom font too") ;; i moved to typst
  (org-todo-keywords
   '((sequence "TODO(t)" "ONGO(o)" "WAIT(w)" "|" "DONE(d)" "SKIP(s)") ))
  (org-todo-keyword-faces
   '(("ONGO" . org-agenda-clocking) ("WAIT" . org-sexp-date) ("SKIP" . org-agenda-dimmed-todo-face)))

  (org-priority-highest 1) (org-priority-lowest  5) (org-priority-default 3)

  (org-clock-in-switch-to-state "STARTED")

  (org-refile-targets '( (org-default-notes-file :maxlevel . 5) (nil :maxlevel . 6) ))

  (org-directory "~/d-sync/notes/")
  (org-default-notes-file (concat org-directory "d-brain.org")) ;; my second brain in OBTF
  (org-pretty-entities t)
  (org-list-allow-alphabetical t)

  (org-fontify-whole-heading-line t) (org-fontify-quote-and-verse-blocks t)

  (org-hierarchical-todo-statistics nil)

  (org-special-ctrl-k t) (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  (org-image-actual-width nil)

  :config
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)  )

(defun d/org-activity()
  "Make temp activity buffer for org tags."
  (interactive)
  (with-current-buffer "d-brain.org"
    (let ((org-export-select-tags (completing-read-multiple "tags: " (org-get-buffer-tags))))
      (org-export-to-buffer 'org (format "export %s.org" org-export-select-tags)))
    (org-mode) (delete-other-windows)
    ))

(global-set-key (kbd "C-x C-a C-o") #'d/org-activity)

(use-package org-agenda :ensure nil :demand t
  :init (org-agenda nil "a") ;; open agenda as dashboard
  :bind ( ("C-c a" . org-agenda)
         (:map org-agenda-mode-map
               ("C-x C-k" . org-agenda-exit)))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'only-window)

  (org-agenda-custom-commands
   '(("n" "Next tasks" ((todo "STARTED") (todo "NEXT") (todo "PROJ")))
     ("a" "Agenda and all TODOs" ((agenda "") (alltodo "")))))

  (org-agenda-files
   '("~/d-sync/notes/d-brain.org"
     "~/d-sync/notes/inbox.org"
     "~/d-git/d-nix/d-setup.org"
     ;; "~/d-git/d-site/README.org"
     )))

(use-package org-capture :ensure nil :after org
  :bind ("C-c c" . org-capture)
  :custom
  (org-capture-templates
   `(
     ("c" "Contacts" entry (file "contacts.org")
      "* %(tempel-insert 'contact)")

     ("l" "Link" item
      (file+headline "bookmarks.org" "gnus") "%a\n")

     ("d" "D Second Brain" entry
      (file+olp+datetree "d-brain.org")
      "* %<%H:%M> - %? %^G
:PROPERTIES:
:ID:       %(org-id-new)
:FROM:     %a
:END:
%i"
      ;; :clock-in t :clock-resume t
      :empty-lines 1 :empty-lines-after 3)

     ("t" "Tasks for the Day" checkitem
      (file+olp+datetree "d-brain.org")
      "[ ] %?\n"
      )

     ("i" "Inbox Rough Notes" entry
      (file "inbox.org")
      "** %?  :z@seed:\n %U\n %i %a\n - ")

     )))

(use-package org-src :ensure nil :after org
    :bind ((:map org-mode-map ("C-c ;" . d/org-babel-edit)))
    :custom (org-src-window-setup 'current-window)
    :config ;; advice to get treesit modes in org src buffer/blocks
    (advice-add 'org-src-get-lang-mode :filter-return
                (lambda (mode)
                  (pcase (assoc mode major-mode-remap-alist)
                    (`(,mode . ,ts-mode) ts-mode)
                    (_ mode)))))

(defun d/org-babel-edit ()
  "Edit any src block with lsp support by tangling the block and
then setting the org-edit-special buffer-file-name to the
absolute path. Finally load eglot."
  (interactive)
  (setq d/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))
  (org-babel-tangle '(4)) (org-edit-special)
  (setq-local buffer-file-name d/tangled-file-name)
  (eglot-ensure) )

(use-package org-id
  :hook (org-insert-heading . org-id-get-create)
  :custom
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; denote inspired heading id as identifier
  (org-id-ts-format "%Y%m%dT%H%M%S"))

(use-package ob-core :ensure nil :after org
  :custom ;; Don't ask every time when I run a code block
  (org-confirm-babel-evaluate t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (calc . t)
     (latex . t) (C . t)
     (R . t) (shell . t) (python . t)
     (julia . t))))

(use-package org-re-reveal :after ox :unless d/on-droid
  :custom
  (add-to-list 'org-export-backends 're-reveal)
  (org-re-reveal-title-slide
   "<h1 class=\"title\">%t</h1> <br> <br> <h2 class=\"subtitle\">%s</h2> <br> <h4 class=\"misc\">%m</h4> <h3 class=\"misc\">%A</h3> <br> <h2 class=\"author\">%a</h2>"))

(use-package org-ql :after org
  :bind (
         ("M-s q" . org-ql-find)
         ("M-s n" . org-ql-find-in-org-directory)
         (:map org-mode-map
               ("C-c q f" . org-ql-find) ("C-c q /" . org-ql-sparse-tree)
               ("C-c q s" . org-ql-search)
               ("C-c q l" . org-ql-open-link)
               ("C-c q v" . org-ql-view))) )

(use-package org-super-agenda :after org
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package org-super-links :unless d/on-droid :after org
  :bind ((:map org-mode-map
               ("C-c s s" . org-super-links-link)
               ("C-c s l" . org-super-links-store-link)
               ("C-c s C-l" . org-super-links-insert-link)
               ("C-c s d" . org-super-links-quick-insert-drawer-link)
               ("C-c s i" . org-super-links-quick-insert-inline-link)
               ("C-c s C-d" . org-super-links-delete-link)))
  :config
  (setopt org-super-links-related-into-drawer "REFERENCE"
          org-super-links-link-prefix 'org-super-links-link-prefix-timestamp
          org-super-links-backlink-into-drawer "CITATION"))

(use-package appt :ensure nil :demand t
  :custom
  (appt-disp-window-function #'appt-org-notify)
  (appt-message-warning-time (* 2 30))
  :config
  (define-advice appt-activate (:after (&optional _arg) hold-your-horses)
    "`appt-activate' is too eager, rein it in."
    (remove-hook 'write-file-functions #'appt-update-list)
    (when (timerp appt-timer)
      (timer-set-time appt-timer (current-time) 600)))

  (define-advice appt-check (:before (&optional _force) from-org-agenda)
    "Read events from Org agenda if possible."
    (and (featurep 'org-agenda)
         (ignore-errors
           (let ((inhibit-message t))
             (org-agenda-to-appt t)))))

  (appt-activate))

(use-package notifications :ensure nil :config
  (defun appt-org-notify (remaining new-time msg)
    (let ((notif (if d/on-droid 'android-notifications-notify 'notifications-notify)))
      (funcall notif
               :body (format "In %s minutes" remaining)
               :title msg
               :urgency 'critical))))

(use-package ox :after org
  :custom (org-export-backends '(org odt md man latex icalendar html ascii)))

(defun d/org-export-clean-brain()
  "Function to retain only level 4 heading in `datetree' single big org file export."
  (interactive)
  (if (y-or-n-p "Do you want to only have 4th headings and flush rest?")
      (progn (beginning-of-buffer)
        (flush-lines "^\\* ")
        (flush-lines "^\\*\\* ")
        (flush-lines "^\\*\\*\\* ")
        (query-replace "****" "*"))
    ))

(use-package org-crypt :after org :ensure nil
  :config (org-crypt-use-before-save-magic)
  :custom
  (org-tags-exclude-from-inheritance '("crypt"))
  (org-crypt-key nil)
  (epg-pinentry-mode 'loopback)
  )

(use-package remember :ensure nil
  :bind ("C-c r r" . remember) ("C-c r n" . remember-notes)
  :custom
  (initial-buffer-choice 'remember-notes)
	(remember-data-file (expand-file-name "inbox.org" org-directory))
	(remember-notes-initial-major-mode 'org-mode))

(use-package calendar :bind ("C-c d d" . calendar))

(use-package markdown-mode :defer t
  :mode "\\.md\\'" "\\.Rmd\\'"
  :hook (markdown-mode . variable-pitch-mode))

(use-package ox-typst :unless d/on-droid 
  :vc (:url "https://github.com/jmpunkt/ox-typst")
  :config (defalias 'typst-mode #'typst-ts-mode))

(use-package jinx :unless d/on-droid
  :init (global-jinx-mode)
  :hook org-mode
  :bind ("M-$". jinx-correct))
