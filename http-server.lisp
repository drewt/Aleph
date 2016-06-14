;;
;; Copyright 2016 Drew Thoreson
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, version 2 of the
;; License.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.
;;

;; http-server.lisp
;;
;; This package defines the HTTP API for interacting with the aggregator.
(in-package :http-server)

(defparameter *port*          (config:get-value '(:server :port)))
(defparameter *document-root* (config:get-value '(:server :document-root)))

(defvar server (make-instance 'hunchentoot:easy-acceptor
                              :port *port*
                              :document-root *document-root*))

(let ((show-errors (config:get-value '(:server :show-lisp-errors)))
      (access-log  (config:get-value '(:server :access-log-file)))
      (message-log (config:get-value '(:server :message-log-file))))
  (when (eq :true show-errors)
    (setf *show-lisp-errors-p* t))
  (unless (eq :default access-log)
    (setf (acceptor-access-log-destination server) access-log))
  (unless (eq :default message-log)
    (setf (acceptor-message-log-destination server) message-log)))

;; Hunchentoot apparently doesn't provide any way to check this, so we keep
;; track of it ourselves.
(defvar server-started nil)

(defun start ()
  "Start the Aleph HTTP server."
  (if server-started
    (warn "Server already running")
    (progn
      (hunchentoot:start server)
      (setf server-started t))))

(defun stop ()
  "Stop the Aleph HTTP server."
  (if server-started
    (progn
      (hunchentoot:stop server)
      (setf server-started nil))
    (warn "Server is not started")))

(defun make-response (obj)
  "Respond to a request by sending OBJ to the client, encoded as JSON."
  (setf (content-type*) "application/json; charset=utf-8")
  (json:encode-json-to-string obj))

(defun not-found ()
  "Respond with HTTP code 404."
  (setf (return-code*) +HTTP-NOT-FOUND+)
  nil)

(defmacro method-case (&body cases)
  `(case (request-method*)
     ,@cases
     (otherwise
       (setf (return-code*) +HTTP-METHOD-NOT-ALLOWED+)
       nil)))

(define-easy-handler (feeds :uri "/feeds")
                     ((since  :parameter-type 'integer))
  (feed-store:with-connection
    (make-response
      (feed-store:query-feeds
        :since (if since
                 (+ since 2208988800)
                 nil)))))

(define-easy-handler (items :uri "/items")
                     ((limit  :init-form 100
                              :parameter-type 'integer)
                      (feed   :parameter-type 'integer)
                      (unread :parameter-type 'boolean)
                      format)
  (feed-store:with-connection
    (let ((items (feed-store:query-items :limit limit :feed feed :unread unread)))
      (make-response (cond ((string= format "raw") items)
                           (t (mapcar #'curator:curate items)))))))

(define-easy-handler (add-feed :uri "/add-feed")
                     ((name :init-form "Untitled")
                      (fetcher :init-form "http")
                      (parser :init-form "auto")
                      (schedule :init-form "periodic")
                      (scheduleparameter :init-form "30")
                      source
                      (tags :init-form '()))
  (feed-store:with-connection
    (let ((feed (controller:add-feed name source
                                     :loc-type fetcher
                                     :feed-type parser
                                     :schedule schedule
                                     :schedule-parameter scheduleparameter
                                     :tags tags)))
      ; TODO: update synchronously
      (redirect (format nil "/feeds/~a" (feed-store:feed-id feed))))))

;; TODO: refactor to consolidate code for extracting and validating ID from URL

(defun feed-handler ()
  "Serve the feed at /feeds/<id>."
  (feed-store:with-connection
    (let* ((id (parse-integer (subseq (request-uri*) 7)
                              :junk-allowed t))
          (feed (feed-store:get-feed id))
          (fmt (get-parameter "format")))
      (if feed
        (method-case
          ((:GET)
            (make-response (cond ((string= fmt "raw") feed)
                                 (t (curator:curate feed)))))
          ((:DELETE)
            (feed-store:rm-feed (feed-store:feed-id feed))
            nil))
        (not-found)))))

(defun feed-items-handler ()
  "Serve items for the feed at /feeds/<id>."
  (feed-store:with-connection
    (let* ((id (parse-integer (subseq (request-uri*) 7)
                              :junk-allowed t))
           (fmt (get-parameter "format")))
      (if (feed-store:get-feed id)
        (let ((items (feed-store:get-items id)))
          (method-case
            ((:GET)
              (make-response (cond ((string= fmt "raw"))
                                   (t (mapcar #'curator:curate items)))))
            ((:DELETE)
              ;TODO
              nil)))
        (not-found)))))

(defun feed-update-handler ()
  "Update the feed at /feeds/<id>"
  (feed-store:with-connection
    (let ((id (parse-integer (subseq (request-uri*) 7)
                             :junk-allowed t)))
      (if (feed-store:get-feed id)
        (method-case
          ((:POST)
            (controller:update-feed id)
            nil))
        (not-found)))))

(defun feed-mark-read-handler ()
  "Mark all items read for the feed at /feeds/<id>."
  (feed-store:with-connection
    (let ((id (parse-integer (subseq (request-uri*) 7)
                             :junk-allowed t)))
      (if (feed-store:get-feed id)
        (method-case
          ((:POST)
            (feed-store:mark-feed-read id)
            nil))
        (not-found)))))

(defun mark-all-read-handler ()
  "Mark all items read."
  (feed-store:with-connection
    (method-case
      ((:POST)
        (feed-store:mark-all-read)
        nil))))

(defun update-all-handler ()
  "Update all feeds."
  (feed-store:with-connection
    (controller:update-all)))

(defun item-mark-read-handler ()
  "Mark the item read at /items/<id>."
  (feed-store:with-connection
    (let ((id (parse-integer (subseq (request-uri*) 7)
                             :junk-allowed t)))
      (if (feed-store:get-item id)
        (method-case
          ((:POST)
            (feed-store:mark-item-read id)
            nil))
        (not-found)))))

(defun item-handler ()
  "Serve the item at /items/<id>."
  (feed-store:with-connection
    (let* ((id (parse-integer (subseq (request-uri*) 7)
                              :junk-allowed t))
           (item (feed-store:get-item id)))
      (if item
        (method-case
          ((:GET)
            (setf (content-type*) "application/json; charset=utf-8")
            (json:encode-json-to-string item)))
        (not-found)))))

(defun index-handler ()
  "Serve <path>/index.html when <path>/ is requested."
  (handle-static-file
    ; FIXME: join pathnames portably
    (format nil "~a~aindex.html"
                *document-root*
                (puri:uri-path (puri:parse-uri (request-uri*))))))

(defun no-extension-handler ()
  "Serve <page>.html when <page> is requested."
  (handle-static-file
    ; FIXME: join pathnames portably
    (format nil "~a~a.html"
            *document-root*
            (puri:uri-path (puri:parse-uri (request-uri*))))))

(setf *dispatch-table*
      (list (create-regex-dispatcher "^/feeds/\\d+/items/?$"     #'feed-items-handler)
            (create-regex-dispatcher "^/feeds/\\d+/update/?$"    #'feed-update-handler)
            (create-regex-dispatcher "^/feeds/\\d+/mark-read/?$" #'feed-mark-read-handler)
            (create-regex-dispatcher "^/feeds/\\d+/?$"           #'feed-handler)
            (create-regex-dispatcher "^/feeds/mark-read/?$"      #'mark-all-read-handler)
            (create-regex-dispatcher "^/feeds/update/?$"         #'update-all-handler)
            (create-regex-dispatcher "^/items/\\d+/mark-read/?$" #'item-mark-read-handler)
            (create-regex-dispatcher "^/items/\\d+/?$"           #'item-handler)
            (create-regex-dispatcher "^/items/mark-read/?$"      #'mark-all-read-handler)
            'hunchentoot:dispatch-easy-handlers
            (create-regex-dispatcher "/$"                        #'index-handler)
            (create-regex-dispatcher "/[^\\./]*$"                #'no-extension-handler)))
