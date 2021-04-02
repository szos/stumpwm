(in-package :stumpwm)

(defpackage :customize
  (:use :clim :clim-lisp))

(in-package :customize)

(defun acceptable-type-p (ts)
  (not (and (listp ts)
            (member (car ts) '(member or and)))))

(defmacro mkbutton (name fn)
  `(make-pane 'push-button
              :label ,name
              :activate-callback ,fn))

(define-application-frame stumpwm-settings-inspector ()
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
                      (let* ((frame (gadget-client gadget))
                             (redisp (and (string= (active-filter frame) "")
                                          (string= (type-filter frame) ""))))
                        (execute-frame-command frame '(com-clear-filters)))))
          +fill+
          (mkbutton "Save Settings"
                    (lambda (gadget)
                      (let ((frame (gadget-client gadget)))
                        (stumpwm::write-settings-to-file)
                        (notify-user frame "Settings saved to ~/.stumpwm.d/customize-variables.lisp" :exit-boxes '((:ok "OK"))))))
          (mkbutton "Quit"
                    (lambda (gadget)
                      (let ((frame (gadget-client gadget)))
                        (frame-exit frame)))))
        (20 filters)
        (:fill (scrolling () main-display))
        (1/4 interactor))))))

(defmacro defc (name-and-options arguments &body body)
  `(define-stumpwm-settings-inspector-command ,name-and-options ,arguments ,@body))

(defc (com-quit :name t) ()
  (frame-exit *application-frame*))

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
  (let ((valid (stumpwm::customize symbol v)))
    (when (eq valid :invalid)
      (notify-user *application-frame*
                   (format nil "~A isn't a valid value for ~A" v symbol)))))

(define-presentation-to-command-translator customize-setting
    (defconfig::config-info com-customize stumpwm-settings-inspector
     :priority 1)
    (info)
  (list (defconfig:config-info-place info)
        (let* ((ts (defconfig:config-info-typespec info))
               (acceptable-type (acceptable-type-p ts)))
          (if acceptable-type
              (accept ts)
              (read-from-string (accept 'string))))))

(defc (com-reset-setting :name t) ((symbol symbol))
  (stumpwm:call-in-main-thread
   (lambda () 
     (defconfig:reset-computed-place symbol :db stumpwm::*stumpwm-db*))))

(define-presentation-to-command-translator reset-setting
    (defconfig::config-info com-reset-setting stumpwm-settings-inspector
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
      (loop for setting being the hash-value of (cdr stumpwm::*stumpwm-db*)
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
  (let ((frame (make-application-frame 'stumpwm-settings-inspector
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
               (customize::app-main filter))))))
