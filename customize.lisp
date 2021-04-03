(in-package :stumpwm)

(defmacro defsetting (place &rest keys)
  `(defconfig:define-variable-config ,place ,place
     :db *stumpwm-db*
     :regen-config t
     ,@keys))

(defmacro with-typespec (typespec &body variables)
  (let ((forms (loop for variable in variables
                     collect `(defsetting ,variable :typespec ,typespec))))
    `(progn ,@forms)))

(defconfig:define-defconfig-db *stumpwm-db* :stumpwm
  :if-exists :redefine
  :doc "database for stumpwm related defconfig settings")

(declaim (special *stumpwm-db*))

(defvar *customize-file* "~/.stumpwm.d/customized-settings.lisp")

(defun load-customized ()
  (when (uiop:file-exists-p *customize-file*)
    (load *customize-file*)))

(defun customize-variable (symbol value)
  (let* ((config (car (defconfig:search-configurable-objects symbol :stumpwm)))
         (validated? (and config
                          (funcall (defconfig:config-info-predicate config)
                                   value))))
    (cond (validated?
           (stumpwm:call-in-main-thread
            (lambda ()
              (setf (symbol-value symbol) value)))
           :valid)
          ((and config (defconfig:config-info-coercer config))
           (let ((coerced (funcall (defconfig:config-info-coercer config) value)))
             (if (funcall (defconfig:config-info-predicate config) coerced)
                 (progn (call-in-main-thread
                         (lambda () (setf (symbol-value symbol) coerced)))
                        :valid)
                 :invalid)))
          (t :invalid))))

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
  (with-open-file (file *customize-file*
                        :direction :output
                        :element-type 'character
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file ";;;; This file was automatically generated and should not be edited by hand.~%;;;; If you do, your changes may be overwritten.~%~%")
    (unless (= (hash-table-count (cdr *stumpwm-db*)) 0)
      (let ((finalized nil))
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
              do (format file "~%~A~&" final))))))

;;;; Setting Declarations

;;; Primitives.lisp

(with-typespec '(integer 0)
  *maximum-completions*
  *timeout-wait*
  *timeout-frame-indicator-wait*
  *maxsize-border-width*
  *transient-border-width*
  *normal-border-width*
  *message-window-padding*
  *message-window-y-padding*
  *max-last-message-size*
  *min-frame-width*
  *min-frame-height*
  *debug-level*
  *grab-pointer-character*
  *grab-pointer-character-mask*)

;; (defun parse-color-string (string)
;;   (when (stringp string)
;;     (let ((r (parse-integer string :start 0 :end 2 :radix 16))
;;           (g (parse-integer string :start 2 :end 4 :radix 16))
;;           (b (parse-integer string :start 4 :end 6 :radix 16)))
;;       (xlib:make-color :red (/ r 255) :green (/ g 255) :blue (/ b 255)))))

;; (defsetting *grab-pointer-foreground*
;;   :validator (lambda (x)
;;                (typep x 'xlib:color))
;;   :coercer (lambda (x)
;;              (handler-case (parse-color-string x)
;;                (error () x))))

(with-typespec 'boolean
  *suppress-frame-indicator*
  *suppress-window-placement-indicator*
  *list-hidden-groups*
  *run-or-raise-all-groups*
  *run-or-raise-all-screens*
  *resize-hides-windows*
  *root-click-focuses-frame*)

(with-typespec 'string
  *frame-indicator-text*
  *shell-program*
  *text-color*
  *frame-number-map*
  *window-format*
  *group-format*
  *default-group-name*)

(with-typespec '(member :top-left :top-right :bottom-left :bottom-right :center
                 :top :left :right :bottom)
  *message-window-gravity*
  *message-window-input-gravity*
  *input-window-gravity*)

(defsetting *top-level-error-action*
  :typespec '(member :abort :message :break))

(defsetting *window-name-source*
  :typespec '(member :title :class :resource-name))

(defsetting *new-frame-action*
  :typespec '(member :empty :last-window))

(defsetting *new-window-preferred-frame*
  :typespec '(or (member :focused :last :empty :unfocused) cons function))

(defsetting *startup-message*
  :typespec '(or string null))

(defsetting *default-package*
  :typespec 'package)

(defsetting *mouse-focus-policy*
  :typespec '(member :click :ignore :sloppy))

(defsetting *banish-pointer-to*
  :typespec '(member :screen :head :frame :window))

(defsetting *window-border-style*
  :typespec '(member :thick :thin :tight :none))

;;; input.lisp

(defsetting *input-completion-style*
  :typespec '(or input-completion-style-cyclic
                input-completion-style-unambiguous))

;;; tile-window.lisp

(defsetting *ignore-wm-inc-hints*
  :typespec 'boolean)

;;; message-window.lisp

(defsetting *queue-messages-p*
  :typespec '(or (member :new-on-bottom) null t))

;;; selection.lisp

(defsetting *default-selections*
  :typespec '(or (member :primary :clipboard) cons))

;;; iresize.lisp

(defsetting *resize-increment*
  :typespec '(integer 1))

;;; help.lisp

(with-typespec '(integer 1)
  *message-max-width*
  *help-max-height*)

;;; time.lisp

(with-typespec 'string
  *time-format-string-default*
  *time-modeline-string*)

;;; mode-line.lisp

(defsetting *mode-line-position*
  :typespec '(member :top :bottom))

(with-typespec '(integer 0)
  *mode-line-border-width*
  *mode-line-pad-x*
  *mode-line-pad-y*)

(with-typespec 'string
  *mode-line-background-color*
  *mode-line-foreground-color*
  *mode-line-border-color*)

(defsetting *mode-line-timeout*
  :typespec '(integer 1))

(defsetting *screen-mode-line-format*
  :typespec '(or list string))

;;; mode-line-formatters.lisp

(with-typespec 'string
  *hidden-window-color*
  *mode-line-highlight-template*)

;;; color.lisp

(defsetting *colors*
  :validator (lambda (x) (every 'stringp x)))

;;; remap-keys.lisp

(defsetting *remapped-keys-enabled-p*
  :typespec 'boolean)

;;; customize.lisp

(defsetting *customize-file*
  :typespec 'string)
