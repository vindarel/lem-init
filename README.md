# My Lem init file
> For Lem 2.0

Written in literate programing with [erudite](https://github.com/mmontone/erudite) (see below).

* Lem's new website: https://lem-project.github.io/
* all keys: https://lem-project.github.io/usage/keybindings/ (missing some modes, like Lisp mode)

What's in my config:

* nothing you *need*, Lem has good defaults.
* keybindings for a bépo keyboard.
* starts in vi-mode and other configuration snippets
* a command to insert an org-mode-like timestamp of today
* a couple functions to play a media file and toggle pause/play.
  * `M-x media-player-play` on a file in directory mode, `-toggle` and `-stop`.


Impressions:
* Lem has a shitload of features! paredit, tabs, treeview, tetris…
* very nice to discover commands in Lem itself.
* very nice for CL, only a few keys missing. Advanced debugger.
* **Lem has a new git interface!** that I started developping. See changes, stage a file, the hunk of a diff, interactive rebase for the best scenario… cool but needs loads of work.

Installation:
copy or symlink to ~/.config/lem/init.lisp (since Lem 2.2) or ~/.lem/init.lisp.

I start Lem like this:
```
$ cd lem/
 CL_SOURCE_REGISTRY=$(pwd)// ros -s lem-sdl2 -e '(progn (lem:lem) (uiop:quit))'
```

## The keys I miss so far

* M-j newline with comment (if inside comment)
* M-h select paragraph
* imenu functionality => see very poor but useful snippet below.
* C-x C-j to open directory mode on the current file => added upstream.
* project-aware commands => the basics added upstream.

In Lisp mode:
 * C-c C-y call function at point in the REPL, with package prefix.
 * marks (m <letter> and '<letter> (quote))
 * little issue with highlighting of parens in vi insert mode, on the last paren.
 * C-~ sync file package with REPL => done in main, <2024-07-30>
 * REPL: C-c C-p go to previous prompt => done in main (end of June, 2023)
 * Inspecting literal objects => done in main (end of June, 2023)

In vi-mode:
* OK now, some keys sent upstream.

Issues in Lem 2.0:
* I can't type backslash or any key with Alt Gr (right Alt key) => fixed upstream => un-fixed so it works best for more people. What's my fix now?

## Things than Lem does better than Emacs

* default auto-completion UI (but not the algorithm yet)
* ease of development for CL, commands discovery
* sorting files by name, mtime AND by size (with human-readable format) in directory-mode
* interactive markdown mode with multiple-major-modes (evaluate Lisp or C code inside code blocks), interactive markdown preview in a browser built-in

## Load more CL libraries
We want some more CL libraries.

```lisp
(pushnew "/path/to/lisp/project/" asdf:*central-registry* :test #'equal)

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
(define-command beginning-of-defun-on-function (n) (:universal)
  "Go to the beginning of defun, the point on the function name."
  ; if n < 0, go to end of defun.
  (previous-line)
  (lem/language-mode::beginning-of-defun-1 n)
  (lem-vi-mode/word:forward-word-end (current-point) n t)
  (skip-whitespace-forward (current-point) t))

(define-key lem-vi-mode:*normal-keymap*
  "g a"
  'beginning-of-defun-on-function)
```
'lem/detective:detective-search)  ;; <2023-07-05 Wed> detective doesn't currently work with define-command and define-key definitions.

```lisp

(define-command end-of-defun-on-function (n) (:universal)
  "Go to the next defun, the point on the function name."
  (lem/language-mode::end-of-defun n)
  (search-forward-regexp (current-point) "^\\(")
  (lem-vi-mode/word:forward-word-end (current-point) n t)
  (skip-whitespace-forward (current-point) t))

(define-key lem-vi-mode:*normal-keymap*
  "g e"
  'end-of-defun-on-function)

```
<2023-05-25 Thu>
vi-mode doesn't have "+" and "-" keys => sent upstream.

dev note: we could find the variables and command names easily thanks to Lem's autocompletion.

```lisp
(define-key lem-vi-mode:*normal-keymap*
  "-"
  'lem-vi-mode/commands:vi-previous-line)

(define-key lem-vi-mode:*normal-keymap*
  "+"
  'lem-vi-mode/commands:vi-next-line)

(define-key lem-vi-mode:*normal-keymap*
  "q"
  'kill-buffer)

(define-key lem-vi-mode:*normal-keymap*
  "Space"
  'lem-vi-mode/commands:vi-forward-char)

(define-key lem-vi-mode:*normal-keymap*
  "("
  'backward-paragraph)

```
sent upstream.

```lisp
(define-key lem-vi-mode:*normal-keymap*
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
(define-key lem-vi-mode:*normal-keymap*
  "' b"
  'select-buffer)

```
switch buffers

```lisp
(define-key lem-vi-mode:*normal-keymap*
  "' «"
  'previous-buffer)

(define-key lem-vi-mode:*normal-keymap*
  "' »"
  'next-buffer)

```
go to end of buffer

```lisp
(define-key lem-vi-mode:*normal-keymap*
  ">"
  'move-to-end-of-buffer)

(define-key lem-vi-mode:*normal-keymap*
  "<"
  'move-to-beginning-of-buffer)

(define-key lem-vi-mode:*normal-keymap*
  "M-»"
  'move-to-end-of-buffer)

(define-key lem-vi-mode:*normal-keymap*
  "M-«"
  'move-to-beginning-of-buffer)

```
vi visual mode

```lisp
(define-key lem-vi-mode/visual::*visual-keymap*
  "x"
  'lem:kill-region)

(define-key *global-keymap*
  "C-x \""
  'lem-core/commands/window:delete-other-windows)

(define-key *global-keymap*
  "C-x »"
  'lem-core/commands/window:split-active-window-horizontally)

(define-key *global-keymap*
  "C-x «"
  'lem-core/commands/window:split-active-window-vertically)

(define-key *global-keymap*
  "F6"
  'lem/language-mode::comment-or-uncomment-region)

(define-key *global-keymap*
  "M-s"
  'previous-line)
(define-key *global-keymap*
  "M-t"
  'next-line)

(define-key *global-keymap* "C-PageDown"
  'lem/frame-multiplexer:frame-multiplexer-next)

(define-key *global-keymap* "C-PageUp"
  'lem/frame-multiplexer:frame-multiplexer-prev)



```
Undo:
this doesn't work, it understands C-space.

```lisp
(define-key *global-keymap*
  "C-_"
  'undo)

```
## imenu
A very poor man's imenu.

```lisp
(defun buffer-headings (txt)
  (loop for line in (str:lines txt)
      for parts = (str:split " " line)
      for i = 1 then (incf i)
      if (str:starts-with-p "(def" line)
      ; collect (list line i))
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
;(sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2::display-window lem-sdl2/display::*display*) 0.9)
#+lem-sdl2
(sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2/display::display-window lem-sdl2/display::*display*) 1.0)

```
I want to see my logs on the terminal output:

```lisp
(log:config :info)

```
### Load legit.
It is not loaded by default, waiting for power testers.

```lisp
(ql:quickload "lem/legit")

(setf (config :prompt-for-commit-abort) nil)

```
Fix a slow down for me, see
https://github.com/lem-project/lem/issues/1092
This crashes Lem: (so, keep a patch in my local git…)

```lisp

; (defun my/lem-sdl2--call-with-renderer (function)
;   (uiop:format! t "~%running Lem with my SDL2 slowdown fix, issue 1092…~%")
;   (bt:with-recursive-lock-held ((display-mutex *display*))
;     (funcall function)))

```
(setf (symbol-function 'lem-sdl2::call-with-renderer) #'my/lem-sdl2--call-with-renderer)
### Suspend ncurses Lem (C-z)

```lisp

#+lem-ncurses
(define-command suspend-lem () ()
  ; @fukamachi
  ; https://github.com/lem-project/lem/issues/306
  (when (find-package 'charms/ll)
    (uiop:symbol-call 'charms/ll 'endwin)  ; the package doesn't exist in the SDL version.
    (sb-posix:kill (sb-posix:getpid) sb-posix:sigtstp)))

(define-key *global-keymap* "C-z C-z" 'suspend-lem)

```
### Other settings - timestamps
Load a utility from another file, too short for a PR:

```lisp
(load "~/dotfiles/lem/time-stamp.lisp")

```
Now you can do `M-x time-stamp` to print the timestamp of the day, in the org-mode format:
"<2023-07-05 Wed>"
### Choose the position of the completion prompt (new in May, 2024)

```lisp

(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)

```
and show the completion list directly, without a first press on TAB:

```lisp
(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))

```

## Erudite: produce this README.md

The command required to produce the readme is:

```
(erudite:erudite #p"README.md" "lem-init.lisp" :output-type :markdown :syntax :erudite)
```

NB: I had to tweak cl-template's .asd definition to `:cl-template` instead of `#:cl-template`.

## See also

* https://gitlab.com/sasanidas/lem-config/-/blob/master/init.lisp
* https://github.com/garlic0x1/.lem
