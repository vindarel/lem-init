

```lisp
(play "/path/to/file.mp3")
;; => #<PLAYER {}>

(toggle-play/pause #<PLAYER {}>)
```


## dev

We use `mpv`'s built-in IPC.

https://mpv.io/manual/master/#json-ipc
