(uiop:define-package :lem/media-player
    (:use :cl
          :media-player
          :lem))

(in-package :lem/media-player)


 (define-major-mode mediaplayer-mode nil
    (:name "mediaplaer"
     :syntax-table nil
     :keymap *mediaplayer-mode-keymap*)

  ;; Add player actions on a file's right click, only for supported media files:
  (setf (buffer-context-menu (current-buffer))
        (make-instance 'lem/context-menu:context-menu
                        :compute-items-function 'compute-context-menu-items))
  )

(define-command media-player-play () ()
  "Play the file at point."
  (let ((file (lem/directory-mode::get-pathname (current-point))))
    (ensure-top-level-player)
    (play file *player*)))

(define-command media-player-toggle () ()
  (toggle-pause *player*))

(define-command media-player-stop () ()
  (stop *player*))

;;;
;;; Context menu functions.
;;; The major mode sets the context menu.
;;;

(defun context-menu-play-file ()
  (lem/context-menu:make-item
   :label "Play file."
   :callback (lambda (&rest args)
               (declare (ignore args))
               (play (lem/directory-mode::get-pathname (current-point))
                     *player*))))

(defun context-menu-toggle-play/pause ()
  (lem/context-menu:make-item
   :label "Toggle play/pause."
   :callback (lambda (&rest args)
               (declare (ignore args))
               (toggle-pause *player*))))

(defun compute-context-menu-items ()
  "Add menus on a right click (play, pause…) when the file at point is a supported media file."
  (let ((file (lem/directory-mode::get-pathname (current-point))))
    (if (and file (find (pathname-type file) (supported-audio-and-video)
                          :test #'equal))
      (list (context-menu-play-file)
            (context-menu-toggle-play/pause))
      ;; We'd like to show the toggle pause menu in all buffers, for a right click everywhere.
      ;; not working?
      (list (context-menu-toggle-play/pause))
      )))
