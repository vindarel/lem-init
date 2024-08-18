(uiop:define-package :lem/media-player
    (:use :cl
          :media-player
          :lem))

(in-package :lem/media-player)


(define-major-mode mediaplayer-mode ()
    (:name "mediaplaer"
     :syntax-table nil
     :keymap *mediaplayer-mode-keymap*)
  ;; config here.
  )

(define-command media-player-play () ()
  "Play the file at point."
  (let ((file (lem/directory-mode::get-pathname (current-point))))
    (ensure-top-level-player)
    (play file *player*)))

(define-command media-player-toggle () ()
  (toggle-play/pause *player*))
