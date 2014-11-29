;;;; +----------------------------------------------------------------+
;;;; | Google Cloud Messaging (GCM)                                   |
;;;; +----------------------------------------------------------------+

(defpackage #:gcm
  (:use #:cl)
  (:import-from #:com.gigamonkeys.json #:to-json #:json #:parse-json)
  (:import-from #:drakma #:http-request)
  (:import-from #:babel #:octets-to-string)
  (:export #:gcm-error
           #:message
           #:message-payload
           #:message-delay-while-idle-p
           #:message-collapse-key
           #:message-time-to-live
           #:raw-json
           #:response
           #:response-multicast-id
           #:response-success
           #:response-failure
           #:response-canonical-ids
           #:response-results
           #:result
           #:result-message-id
           #:result-error-string
           #:config
           #:config-api-key
           #:config-dry-run-p
           #:config-send-endpoint
           #:config-timeout
           #:*config*
           #:configure
           #:config-required
           #:store-api-key
           #:*registration-ids*
           #:set-registration-ids
           #:send-failure
           #:send-failure-code
           #:send-failure-response
           #:send))

(in-package #:gcm)

;;; Miscellaneous

(define-condition gcm-error (error)
  ()
  (:report "GCM error"))

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defun json-bool (x)
  (if x :true :false))

;;; Message

(defclass message ()
  ((payload :initarg :payload :reader message-payload)
   (delay-while-idle :initarg :delay-while-idle :reader message-delay-while-idle-p)
   (collapse-key :initarg :collapse-key :reader message-collapse-key)
   (time-to-live :initarg :time-to-live :reader message-time-to-live)))

(defmethod initialize-instance :after ((message message) &key)
  (if (slot-boundp message 'payload)
      (check-type (slot-value message 'payload) list))
  (if (slot-boundp message 'collapse-key)
      (check-type (slot-value message 'collapse-key) string))
  (if (slot-boundp message 'time-to-live)
      (check-type (slot-value message 'time-to-live) integer)))

(defmethod to-json ((message message))
  (append (if (slot-boundp message 'delay-while-idle)
              (list "delay_while_idle" (json-bool (slot-value message 'delay-while-idle))))
          (if (slot-boundp message 'collapse-key)
              (list "collapse_key" (slot-value message 'collapse-key)))
          (if (slot-boundp message 'time-to-live)
              (list "time_to_live" (slot-value message 'time-to-live)))
          (if (slot-boundp message 'payload)
              (list "data" (slot-value message 'payload)))))

;;; Response

(defgeneric raw-json (object))

(defclass response ()
  ((multicast-id :initform nil :reader response-multicast-id)
   (success :initform 0 :reader response-success)
   (failure :initform 0 :reader response-failure)
   (canonical-ids :initform 0 :reader response-canonical-ids)
   (results :initform #() :reader response-results)
   (raw-json :initarg :raw-json :reader raw-json)))

(defun parse-response (json)
  (let ((response (make-instance 'response :raw-json json)))
    (do ((plist json (cddr plist)))
        ((null plist))
      (cond ((equal (car plist) "multicast_id")
             (setf (slot-value response 'multicast-id) (cadr plist)))
            ((equal (car plist) "success")
             (setf (slot-value response 'success) (cadr plist)))
            ((equal (car plist) "failure")
             (setf (slot-value response 'failure) (cadr plist)))
            ((equal (car plist) "canonical_ids")
             (setf (slot-value response 'canonical-ids) (cadr plist)))
            ((equal (car plist) "results")
             (setf (slot-value response 'results)
                   (map 'vector #'parse-result (cadr plist))))
            (t
             (warn "Unrecognized key '~A' in reponse." (car plist)))))
    response))

(defclass result ()
  ((message-id :initform nil :reader result-message-id)
   (error-string :initform nil :reader result-error-string)
   (raw-json :initarg :raw-json :reader raw-json)))

(defun parse-result (json)
  (let ((result (make-instance 'result :raw-json json)))
    (do ((plist json (cddr plist)))
        ((null plist))
      (cond ((equal (car plist) "message_id")
             (setf (slot-value result 'message-id) (cadr plist)))
            ((equal (car plist) "error")
             (setf (slot-value result 'error-string) (cadr plist)))
            (t
             (warn "Unrecognized key '~A' in result." (car plist)))))
    result))

;;; Configuration

(defclass config ()
  ((api-key :initarg :api-key :reader config-api-key)
   (dry-run :initarg :dry-run :reader config-dry-run-p)
   (send-endpoint :initarg :send-endpoint :reader config-send-endpoint)
   (timeout :initarg :timeout :reader config-timeout))
  (:default-initargs
   :dry-run nil
   :send-endpoint "https://android.googleapis.com/gcm/send"
   :timeout 20))

(defvar *config* nil)

(defun configure (api-key &rest options)
  (check-type api-key string)
  (setf *config*
        (apply #'make-instance 'config
               :api-key api-key
               options))
  (values))

(define-condition config-required (gcm-error)
  ()
  (:report "GCM configuration is required."))

(defun require-config (config)
  (if (and config (slot-boundp config 'api-key))
      config
      (restart-case
          (error 'config-required)
        (store-api-key (api-key)
          :report "Provide an API key."
          :interactive prompt-for-api-key
          (cond (config
                 (setf (slot-value config 'api-key) api-key)
                 config)
                (t
                 (make-instance 'config :api-key api-key)))))))

(defun prompt-for-api-key ()
  (format t "Enter the API key: ")
  (list (read-line)))

;;; Registration IDs

(defvar *registration-ids* nil)

(defun set-registration-ids (registration-ids)
  (setf *registration-ids* (coerce registration-ids 'vector))
  (values))

;;; Sending messages

(define-condition send-failure (gcm-error)
  ((code :initarg :code :reader send-failure-code)
   (response :initarg :response :reader send-failure-response))
  (:report report-send-failure))

(defun report-send-failure (condition stream)
  (format stream "Failed to send GCM message, with code ~A."
          (send-failure-code condition)))

(defun send (message &key (config *config*) (registration-ids *registration-ids*))
  (setf config (require-config config))
  (multiple-value-bind (response code)
      (http-request (config-send-endpoint config)
                    :method :post
                    :additional-headers
                    (list (cons "Authorization" (cat "key=" (config-api-key config))))
                    :connection-timeout (config-timeout config)
                    :force-binary t
                    :content-type "application/json"
                    :content (json (send-body message registration-ids config)))
      (setf response (octets-to-string response))
      (when (/= code 200)
        (error 'send-failure :code code :response response))
      (parse-response (parse-json response))))

(defun send-body (message registration-ids config)
  (setf registration-ids (coerce registration-ids 'vector))
  (append (to-json message)
          (list "registration_ids" registration-ids)
          (if (config-dry-run-p config)
              (list "dry_run" :true))))
