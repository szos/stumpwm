(in-package :stumpwm)

(defpackage :customizer-gui
  (:use :clim :clim-lisp)
  (:import-from #:stumpwm
                #:write-settings-to-file
                #:customize-variable
                #:call-in-main-thread
                #:*stumpwm-db*
                #:*customize-file*)
  (:export #:app-main))

(in-package :customizer-gui)

(defmacro mkbutton (name fn)
  `(make-pane 'push-button
              :label ,name
              :activate-callback ,fn))

(define-application-frame stumpwm-settings ()
  ((type-filter :initform "" :accessor type-filter)
   (active-filter :initform "" :accessor active-filter :initarg :filter))
  (:panes (main-display :application
                        :scroll-bars nil
                        :incremental-redisplay nil
                        :display-function 'display-main)
          (interactor :interactor)
          (filters :application
                   :incremental-redisplay nil
                   :display-function 'display-filters
                   :scroll-bars nil))
  (:layouts
   (default
    (labelling (:label "Customize StumpWM Settings"
                :text-style (make-text-style :serif :roman :huge))
      (vertically (:equalize-width t)
        (horizontally ()
          (mkbutton "Filter Names"
                    (lambda (gadget)
                      (let ((frame (gadget-client gadget)))
                        (execute-frame-command frame '(com-filter-name)))))
          (mkbutton "Filter Types"
                    (lambda (gadget)
                      (let ((frame (gadget-client gadget)))
                        (execute-frame-command frame '(com-filter-type)))))
          (mkbutton "Clear Filters"
                    (lambda (gadget)
                      (let* ((frame (gadget-client gadget)))
                        (execute-frame-command frame '(com-clear-filters)))))
          +fill+
          (mkbutton "Save Settings"
                    (lambda (gadget)
                      (let ((frame (gadget-client gadget)))
                        (write-settings-to-file)
                        (notify-user frame
                                     (format nil "Settings saved to ~A"
                                             *customize-file*)
                                     :exit-boxes '((:ok "OK"))))))
          (mkbutton "Quit"
                    (lambda (gadget)
                      (let ((frame (gadget-client gadget)))
                        (frame-exit frame)))))
        (20 filters)
        (:fill (scrolling () main-display))
        (1/4 interactor))))))

(defmacro defc (name-and-options arguments &body body)
  `(define-stumpwm-settings-command ,name-and-options ,arguments ,@body))

(defc (com-quit :name t) ()
  (frame-exit *application-frame*))

(defc (com-redisplay :name t) ()
  (redisplay-frame-panes *application-frame* :force-p t))

(define-presentation-method present
    (config-info (type defconfig::config-info) stream view &key)
  (format stream
          ;; "~&Setting: ~S~%~4TCurrent Value: ~S~%~4TDefault Value: ~S~%~4TType Description: ~S~%"
          "~&~S~%~4TCurrent Value: ~S~%~4TDefault Value: ~S~%~4TType Description: ~S~%"
          (defconfig:config-info-place config-info)
          (symbol-value (defconfig:config-info-place config-info))
          (defconfig:config-info-default-value config-info)
          (or (defconfig:config-info-typespec config-info)
              (defconfig:config-info-valid-values-description config-info))))

(defc (com-customize :name t) ((symbol symbol) (v t))
  (case (customize-variable symbol v)
    (:invalid
     (notify-user *application-frame*
                  (format nil "~A isn't a valid value for ~A" v symbol)))))

(defun acceptable-type-p (ts)
  (cond ((not (and (listp ts)
                   (member (car ts) '(member or and))))
         nil)
        ((eq ts 'xlib:color) nil)
        (t t)))

(define-presentation-to-command-translator customize-setting
    (defconfig::config-info com-customize stumpwm-settings
     :priority 1)
    (info)
  (list (defconfig:config-info-place info)
        (let* ((ts (defconfig:config-info-typespec info))
               (acceptable-type (acceptable-type-p ts)))
          (if acceptable-type
              (accept acceptable-type)
              (read-from-string (accept 'string))))))

(defc (com-reset-setting :name t) ((symbol symbol))
  (call-in-main-thread
   (lambda () 
     (defconfig:reset-computed-place symbol :db *stumpwm-db*))))

(define-presentation-to-command-translator reset-setting
    (defconfig::config-info com-reset-setting stumpwm-settings
     :priority 0)
    (info)
  (list info))

(defc (com-filter-name :name t) ((string string))
  (setf (active-filter *application-frame*) string))

(defc (com-filter-type :name t) ((string string))
  (setf (type-filter *application-frame*) string))

(defc (com-clear-filters :name t) ()
  (setf (active-filter *application-frame*) ""
        (type-filter *application-frame*)   ""))

(defun satisfy-filter (filter setting)
  (if filter 
      (let ((sym-string
              (string-upcase (symbol-name (defconfig:config-info-place setting))))
            (match-against (string-upcase filter)))
        (cl-ppcre:all-matches match-against sym-string))
      t))

(defun satisfy-type-filter (filter setting)
  (if filter
      (let ((match-against (string-upcase filter))
            (ts (string-upcase
                 (format nil "~A" (defconfig:config-info-typespec setting)))))
        (cl-ppcre:all-matches match-against ts))
      t))

(defun display-main (frame pane)
  (with-end-of-line-action (pane :allow)
    (with-end-of-page-action (pane :allow)
      (loop for setting being the hash-value of (cdr *stumpwm-db*)
            with filter = (string-trim '(#\space) (active-filter frame))
            with tfilter = (string-trim '(#\space) (type-filter frame))
            when (and (satisfy-filter filter setting)
                      (satisfy-type-filter tfilter setting))
              do (present setting 'defconfig::config-info :stream pane
                                                          :single-box t)))))

(defun display-filters (frame pane)
  (let ((name (active-filter frame))
        (type (type-filter frame)))
    (when (and name type) 
      (slim:with-table (pane)
        (slim:row
          (slim:cell (format pane "Name Filter: "))
          (slim:cell (format pane "~S" name))
          (slim:cell (format pane "Type Filter: "))
          (slim:cell (format pane "~S" type)))))))

(defun app-main (&optional filter)
  (let ((frame (make-application-frame 'stumpwm-settings
                                       :filter (or filter ""))))
    (handler-case 
        (handler-bind ((simple-error
                         (lambda (c)
                           (declare (ignore c))
                           (let ((r (find-restart 'clim-clx::use-localhost)))
                             (when r
                               (invoke-restart 'clim-clx::use-localhost))))))
          (run-frame-top-level frame)
          (return-from app-main nil))
      (error (c)
        (notify-user nil
                     (format nil "Encountered error ~A and quit" c))
        (frame-exit frame)))))

(in-package :stumpwm)

(defvar *customize-thread* nil)

(defcommand customize (&optional filter) ((:rest))
  (if (and (typep *customize-thread* 'sb-thread:thread)
           (sb-thread:thread-alive-p *customize-thread*))
      (message "Customizer thread already running")
      (setf *customize-thread*
            (sb-thread:make-thread
             (lambda ()
               (customizer-gui:app-main filter))))))

