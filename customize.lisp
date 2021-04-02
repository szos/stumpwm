(in-package :stumpwm)

(defun load-customized ()
  (when (uiop:file-exists-p *customize-file*)
    (load *customize-file*)))

(defun customize (symbol value)
  (let* ((config (car (defconfig:search-configurable-objects symbol :stumpwm)))
         (validated? (and config
                          (funcall (defconfig:config-info-predicate config)
                                   value))))
    (if validated?
        (progn (stumpwm:call-in-main-thread
                (lambda ()
                  (setf (symbol-value symbol) value)))
               :valid)
        :invalid)))

(defun reset (symbol)
  (stumpwm:call-in-main-thread
   (lambda ()
     (defconfig:reset-computed-place symbol :db (defconfig:get-db :stumpwm)))))

(defun generate-set-string (symbol)
  (let ((value (symbol-value symbol)))
    (if (eq t value)
        (format nil "T")
        (typecase value
          (string (format nil "~S" value))
          (number value)
          (null "NIL")
          (keyword (format nil ":~A" value))
          (symbol (format nil "'~A" value))
          (cons (format nil "'~S" value))))))

(defun gen-final (place list)
  (cond ((eql place 'stumpwm:*colors*)
         (cons "(update-color-map (current-screen))" list))
        (t list)))

(defun write-settings-to-file ()
  (let ((finalized nil))
    (with-open-file (file *customize-file*
                          :direction :output
                          :element-type 'character
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file ";;;; This file, customize-variables.lisp, was automatically generated~%;;;; and should not be edited by hand. If you do, your changes may be overwritten.~%~%")
      (format file "(in-package :stumpwm)~%~%(defconfig:setv~%")
      (loop for setting being the hash-value of (cdr *stumpwm-db*)
            for place = (defconfig:config-info-place setting)
            unless (equal (symbol-value place)
                          (defconfig:config-info-default-value setting))
              do (format file "~& ~A ~A~&"
                         place (generate-set-string place))
                 (setf finalized (gen-final place finalized)))
      (format file " :db *stumpwm-db*)~%")
      (loop for final in finalized
            do (format file "~%~A~&" final)))))
