
(uiop:define-package :media-player
    (:use :cl)
  (:export
   #:*player*
   #:ensure-top-level-player
   #:player
   #:play
   #:stop
   #:toggle-play/pause))

(in-package :media-player)

;; TODO: .asd
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "str"))

(defvar *player* nil
  "top-level player. We can also use more than one player instance.")

(defun ensure-top-level-player ()
  (unless *player*
    (setf *player* (make-instance 'player))))


;;;
;;; elisp -> CL interface macros.
;;;

;; copy-paste variables from elisp.
;; We should have them with *earmuffs* though.
(defmacro defcustom (name val doc &body rest)
  (declare (ignore rest))
  `(defparameter ,name ,val ,doc))

;;;
;;; parameters
;;;

(defparameter *show-thumbnails* t
  "Show thumbnails by default.")

(defcustom ready-player-show-thumbnail t
  "When non-nil, display file's thumbnail if available."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-autoplay t
  "When non-nil, automatically start playing when media file opens."
  :type 'boolean
  :group 'ready-player)


(defparameter *playback-commands*
  '(("mpv" "--audio-display=no")))

(defcustom ready-player-supported-video
  '("3g2" "3gp" "asf" "asx" "avi" "divx" "drc" "dvb" "evo" "f4p"
    "f4v" "flv" "h264" "h265" "hevc" "m2ts" "m2v" "mkv" "mov" "mp4"
    "mpg" "mpeg" "mts" "mxf" "ogm" "ogv" "qt" "rm" "rmvb" "vob"
    "webm" "wmv")
  "Supported video media."
  :group 'play-mode
  :type '(repeat string))

(defcustom ready-player-supported-audio
  '("aac" "ac3" "aiff" "amr" "ape" "dts" "f4a" "f4b" "flac" "gsm"
    "m4a" "midi" "mlp" "mka" "mp2" "mp3" "oga" "ogg" "opus" "pva"
    "ra" "ram" "raw" "rf64" "spx" "tta" "wav" "wavpack" "wma" "wv")
  "Supported audiomedia."
  :group 'play-mode
  :type '(repeat string))

(defun ready-player-supported-audio-and-video ()
  "Get a list of supported audio and video.

See `ready-player-supported-audio' and `ready-player-supported-video'
for configuration."
  (append ready-player-supported-audio
          ready-player-supported-video))
(defcustom ready-player-supported-media
  #'ready-player-supported-audio-and-video
  "Supported media types."
  :group 'play-mode
  :type '(choice (function :tag "Function")
                 (repeat (string :tag "String"))))

(defparameter *metadata* nil "Metadata as per ffprobe.")

(defparameter *thumbnail* nil "Thumbnail as per ffmpeg.")

(defvar *last-button-focus* 'play-stop
  "Last button focused.

Could be one of:

=next= =previous= =play-stop= =open-externally= =repeat= =shuffle= or
=autoplay.=

Used to remember button position across files in continuous playback.")


;;;
;;; util functions
;;;

(defun is-audio-p (file)
  (find (str:downcase (pathname-type file))
        ready-player-supported-audio
        :test #'equal))

(defun is-video-p (file)
  (find (str:downcase (pathname-type file))
        ready-player-supported-video
        :test #'equal))

;;;
;;; Player representation.
;;;

(defclass player ()
  ((process :initform nil
            :accessor process
            :documentation "The process object, returned by uiop:launch-program.")
   (socket :initform "/tmp/mpvsocket"
           :accessor socket
           :documentation "The IPC socket file to which send commands. It should be different for different player objects, if we want to control more than one at the same time."))
  (:documentation "We control the mpv player with its built-in IPC mechanism."))


;;;
;;; Control player
;;;

#+(or)
(defparameter *testfile* "/home/vince/zique/01 - Eu e você sempre.MP3")

(defparameter *mpv* "mpv" "mpv binary.")

(defun mpv-input-ipc (&optional player)
  "Mandatory mpv argument in order to control the player. A placeholder to fill with the (socket player) value."
  (format nil "--input-ipc-server=~a" (if player (socket player)
                                          "/tmp/mpvsocket")))

(defparameter *mpv-args* '() "User definable additional arguments to run mpv.")

(defun mpv-command (args)
  "Build up a mpv command to be run with uiop."
  (setf args (uiop:ensure-list args))
  (append (list *mpv*)
          (uiop:ensure-list (mpv-input-ipc))
          *mpv-args* args))

(defun play (file &optional player)
  "Play this file in the background.

  Return the PLAYER instance.

  The PLAYER instance is optional. If no one is given, we create one and return it.

  If this player's process is alive, it is probably reading a media file already, so we send a stop command before playing this file.

  It is possible to read many files at the same time by using different PLAYER objects.

  Usage:

    (play \"/path/to/file.mp3\")  ;; => #<PLAYER {a}>

    (play \"/path/to/file.mp3\" (make-instance 'player))  ;; => #<PLAYER {b}>


  See also:

    TOGGLE-PLAY/PAUSE, STOP, QUIT."
  ;; Ensure we have a player object
  ;; (we can quickly throw "t" or "nil" during development.
  (unless (equal (type-of player) 'player)
    (setf player (make-instance 'player)))

  ;; If we get a pathname, get the file name as string.
  ;; Play!
  (with-accessors ((p process)) player
    ;; but stop the current process if it's alive.
    (when (uiop:process-alive-p p)
      (stop player))
    (setf p (uiop:launch-program (mpv-command (uiop:native-namestring file)))))
  player)

(defvar *debug* nil)

(defun send-command (s &optional player)
  (let ((error-string
          (with-output-to-string (error-output)
            (uiop:run-program (print (format nil "echo ~a | socat - ~a" s (socket player)))
                              :output t
                              :error-output error-output
                              ;; Don't get the debugger on errors. We can read the error output.
                              :ignore-error-status t))))
    (when (string/= error-string "")
      (if *debug*
          (error "~a" error-string)
          error-string))))

(defun toggle-play/pause (player)
  (send-command "'{ \"command\": [\"cycle\", \"pause\"] }'"
                player))

(defun quit (player)
  (send-command "'{ \"command\": [\"quit\"] }'"
                player))

(defun stop (player)
  (send-command "'{ \"command\": [\"stop\"] }'"
                player))
