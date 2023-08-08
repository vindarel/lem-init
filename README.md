# My Lem init file
> For Lem 2.0

Written in literate programing with [erudite](https://github.com/mmontone/erudite) (see below).

* new website: https://lem-project.github.io/lem-page/
* all keys: https://lem-project.github.io/lem-page/usage/keybindings/ (missing some modes, like Lisp mode)

Impressions:
* Lem has a shitload of features! paredit, tabs, treeview, tetris…
* very nice to discover commands in Lem itself.
* very nice for CL, only a few keys missing. Advanced debugger.

Installation:
copy or symlink to ~/.lem/init.lisp

I start Lem like this:
```
$ cd lem/
 CL_SOURCE_REGISTRY=$(pwd)// ros -s lem-sdl2 -e '(lem:lem)'
```

## The keys I miss so far

* M-j newline with comment (if inside comment)
* M-h select paragraph
* imenu functionality => see very poor but useful snippet below.

In Lisp mode:
 * C-~ sync file package with REPL
 * C-c C-y call function at point in the REPL, with package prefix.
 * REPL: C-c C-p go to previous prompt => done in main (end of June, 2023)
 * Inspecting literal objects => done in main (end of June, 2023)
 * marks (m <letter> and '<letter> (quote))
 * little issue with highlighting of parens in vi insert mode, on the last paren.

In vi-mode:
* OK now, some keys sent upstream.

Issues in Lem 2.0:
* I can't type backslash or any key with Alt Gr (right Alt key) => fixed upstream
* typing an accentuated letter (french é) ppriints sa spacee after it and sorta repaeat keys O_o

## Things than Lem does better than Emacs

* default auto-completion UI (but not the algorithm yet)
* ease of development for CL, commands discovery
* sorting files by name, mtime AND by size (with human-readable format) in directory-mode

## Load more CL libraries
We want some more CL libraries.

```lisp
(pushnew "/home/vince/projets/cl-str/" asdf:*central-registry* :test #'equal)

```
## Starting up
Start this config in Lem's CL package

```lisp
(in-package :lem-user)

```
Evaluating Lisp in M-: we want to be in Lem' package.

```lisp
(lem-lisp-mode/internal::lisp-set-package "LEM")

```
Start in vi-mode

```lisp
(lem-vi-mode:vi-mode)

```
Start the Lisp REPL in vi insert mode (commit 23-8-8)

```lisp
(add-hook lem-lisp-mode:*lisp-repl-mode-hook* 'lem-vi-mode/commands:vi-insert)

```

## Some helper functions, bound to keys below.
I want quick movement functions to go to the previous or next definition (function, or anything really).
Lem has beginning- and end-of-defun, but a repetitive call doesn't go outside the function definition.

```lisp
(define-command beginning-of-defun-on-function (n) ("p")
  "Go to the beginning of defun, the point on the function name."
  ; if n < 0, go to end of defun.
  (previous-line)
  (lem/language-mode::beginning-of-defun-1 n)
  (lem-vi-mode/word:forward-word-end (current-point) n t)
  (skip-whitespace-forward (current-point) t))

(define-key lem-vi-mode:*command-keymap*
  "g a"
  'beginning-of-defun-on-function)
```
'lem/detective:detective-search)  ;; <2023-07-05 Wed> detective doesn't currently work with define-command and define-key definitions.

```lisp

(define-command end-of-defun-on-function (n) ("p")
  "Go to the next defun, the point on the function name."
  (lem/language-mode::end-of-defun n)
  (search-forward-regexp (current-point) "^\\(")
  (lem-vi-mode/word:forward-word-end (current-point) n t)
  (skip-whitespace-forward (current-point) t))

(define-key lem-vi-mode:*command-keymap*
  "g e"
  'end-of-defun-on-function)

```
<2023-05-25 Thu>
vi-mode doesn't have "+" and "-" keys => sent upstream.

dev note: we could find the variables and command names easily thanks to Lem's autocompletion.

```lisp
(define-key lem-vi-mode:*command-keymap*
  "-"
  'lem-vi-mode/commands:vi-previous-line)

(define-key lem-vi-mode:*command-keymap*
  "+"
  'lem-vi-mode/commands:vi-next-line)

(define-key lem-vi-mode:*command-keymap*
  "q"
  'kill-buffer)

(define-key lem-vi-mode:*command-keymap*
  "Space"
  'lem-vi-mode/commands:vi-forward-char)

(define-key lem-vi-mode:*command-keymap*
  "("
  'backward-paragraph)

```
sent upstream.

```lisp
(define-key lem-vi-mode:*command-keymap*
  ")"
  'forward-paragraph)

```
## Abbrev completion

TIL Lem has completion with abbrev.
C-p is bound to abbrev
As suggested by its example, we define C-n to display a list of suggestions.
It is originally bound to next-line, in vi-mode too.

```lisp
(define-key lem-vi-mode:*insert-keymap* "C-n" 'lem/abbrev:abbrev-with-pop-up-window)
(define-key *global-keymap* "C-n" 'lem/abbrev:abbrev-with-pop-up-window)

```

## More keys of my liking (bépo keybboard)

Most make sense for a bépo keyboard only.

```lisp
(define-key lem-vi-mode:*command-keymap*
  "' b"
  'select-buffer)

```
switch buffers

```lisp
(define-key lem-vi-mode:*command-keymap*
  "' «"
  'previous-buffer)

(define-key lem-vi-mode:*command-keymap*
  "' »"
  'next-buffer)

```
go to end of buffer

```lisp
(define-key lem-vi-mode:*command-keymap*
  ">"
  'move-to-end-of-buffer)

(define-key lem-vi-mode:*command-keymap*
  "<"
  'move-to-beginning-of-buffer)

(define-key lem-vi-mode:*command-keymap*
  "M-»"
  'move-to-end-of-buffer)

(define-key lem-vi-mode:*command-keymap*
  "M-«"
  'move-to-beginning-of-buffer)

(define-key *global-keymap*
  "C-x \""
  'lem-core/commands/window:delete-other-windows)

(define-key *global-keymap*
  "C-x »"
  'lem-core/commands/window:split-active-window-horizontally)

(define-key *global-keymap*
  "C-x «"
  'lem-core/commands/window:split-active-window-vertically)

```
vi visual mode

```lisp
(define-key lem-vi-mode/visual::*visual-keymap*
  "x"
  'lem:kill-region)

```

## Global keymap

```lisp
(define-key *global-keymap*
  "F6"
  'lem/language-mode::comment-or-uncomment-region)

(define-key *global-keymap*
  "M-s"
  'previous-line)
(define-key *global-keymap*
  "M-t"
  'next-line)

```

## Project

Project-aware commands (M-x project-find-file) sent upstream!
Currently not bound to a key. The Filer (C-x d) opens the root though.

```lisp
(define-key *global-keymap*
  "C-x p f"
  'lem-core/commands/project:project-find-file)

```
Undo:
this doesn't work, it understands C-space.

```lisp
(define-key *global-keymap*
  "C-_"
  'undo)

```
## Dev

```lisp
(define-command find-directory-buffer () ()
  (let ((name (buffer-filename)))
    (and
     (switch-to-buffer
      (find-file-buffer (lem-core/commands/file::directory-for-file-or-lose (buffer-directory))))
```
check buffer name exists as a file.

```lisp
     (when name
       (search-forward (current-point) name)
       (window-recenter (current-window))
       (backward-char (length name))))))

(define-key *global-keymap*
  "C-x C-j"
  'find-directory-buffer)

```
### imenu
A very poor man's imenu.

```lisp
(defun buffer-headings (txt)
  (loop for line in (str:lines txt)
      for parts = (str:split " " line)
      for i = 1 then (incf i)
      if (str:starts-with-p "(def" line)
```
collect (list line i))

```lisp
        collect line))

(defun prompt-for-heading ()
  (let ((candidates (buffer-headings (buffer-text (current-buffer)))))
    (if candidates
        (prompt-for-string "Heading: "
                            :history-symbol '*imenu*
                            :completion-function (lambda (x)
                                                   (completion-strings x candidates))
                            :test-function (lambda (name)
                                             (member name candidates :test #'string=))))))

(define-command imenu () ()
  (let ((candidate (prompt-for-heading)))
    (move-to-beginning-of-buffer)
    (search-forward (current-point) candidate)
    (message "~a" candidate)))

(define-key *global-keymap* "C-x i" 'imenu)

```

## Misc

```lisp
(define-command open-init-file () ()
  ; thx @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-command oif () ()
  (open-init-file))

```
### Open PDF files with an external program

```lisp

(defmethod lem-core/commands/file:execute-find-file :around (executor mode pathname)
  (if (find (pathname-type pathname) '("pdf" "mp4" ".mp4") :test #'equal)
      (open-external-file pathname)
      (call-next-method)))

```
### Transparent background! ^^

```lisp
(sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2::display-window lem-sdl2::*display*) 0.9)

```
I want to see my logs on the terminal output:

```lisp
(log:config :info)

```
Load a utility from another file, too short for a PR:

```lisp
(load "~/dotfiles/lem/time-stamp.lisp")

```
Now you can do M-x time-stamp to print the timestamp of the day, in the org-mode format:
<2023-07-05 Wed>

## Erudite: produce this README.md

The command required to produce the readme is:

```
(erudite:erudite #p"README.md" "lem-init.lisp" :output-type :markdown :syntax :erudite)
```

NB: I had to tweak cl-template's .asd definition to `:cl-template` instead of `#:cl-template`.

## See also

* https://gitlab.com/sasanidas/lem-config/-/blob/master/init.lisp
