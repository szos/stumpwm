(in-package :stumpwm)

(defvar *customize-file* "~/.stumpwm.d/customize-variables.lisp")

;; (handler-bind ((defconfig:database-already-exists-error
;;                  (lambda (c)
;;                    (let ((r (find-restart 'defconfig::continue c)))
;;                      (when r
;;                        (invoke-restart r))))))
;;   (defconfig:define-defconfig-db *stumpwm-db* :stumpwm
;;     :doc "defconfig database for stumpwm"))

(declaim (special *stumpwm-db*))

(defconfig:define-defconfig-db *stumpwm-db* :stumpwm
    :doc "defconfig database for stumpwm")

;; (defparameter *stumpwm-db* (defconfig::make-config-database))
;; (defconfig::add-db-to-plist :stumpwm '*stumpwm-db*)

(defmacro define-pre-existing-setting (place &rest keys)
  `(defconfig:define-variable-config ,place ,place
     :db *stumpwm-db*
     :regen-config t
     ,@keys))

(define-pre-existing-setting *debug-level*
  :typespec '(integer 0))

(defmacro defsetting (place value docstring &rest keys)
  (declare (ignorable keys))
  ;; `(defvar ,place ,value ,docstring)
  (with-gensyms (hold)
    `(let ((,hold ,value))
       (defvar ,place ,hold ,docstring)
       (defconfig:define-variable-config ,place ,hold
         :documentation ,docstring
         :db *stumpwm-db*
         ,@keys)
       (declaim (special ,place)))
    ;; (eval-when (:compile-toplevel :load-toplevel :execute)
    ;;    )
    ))

;; (defmacro defsetting (place value docstring &rest keys)
;;   (with-gensyms (hold)
;;     `(let ((,hold ,value))
;;        (defvar ,place ,hold ,docstring)
;;        (defconfig:define-variable-config ,place ,hold
;;          :documentation ,docstring
;;          :db *stumpwm-db*
;;          :regen-config t
;;          ,@keys))))
       
