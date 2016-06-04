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

;; controller.lisp
;;
;; This package includes feed update logic.
(in-package :controller)

(defun add-feed (name source &key (loc-type "http")
                                  (feed-type "auto")
                                  (schedule "periodic")
                                  (schedule-parameter "30")
                                  (tags '()))
  (scheduler:schedule-feed
    (feed-store:add-feed
      (make-instance 'feed-store:feed
                     :name name
                     :source source
                     :fetcher loc-type
                     :parser feed-type
                     :schedule schedule
                     :schedule-parameter schedule-parameter
                     :tags tags))))

(defun *update-feed (feed)
  (feed-parser:parse (feed-store:feed-parser feed)
                     (feed-source:fetch (feed-store:feed-fetcher feed)
                                        (feed-store:feed-source feed))))

;; Returns the (hopefully) globally-unique identifier for an item.  If the
;; feed publisher had any sense, they provided this themselves in the guid or
;; atom:id tags; otherwise we use the link, title or description tag.  The
;; Feed ID is always prepended to the guid to further prevent collisions.
(defun get-guid (feed item-metadata)
  (format nil "~a-~a"
    (feed-store:object-id feed)
    (fourth
      (cond
        ((assoc "guid" item-metadata :test #'string=))
        ((assoc "atom:id" item-metadata :test #'string=))
        ((assoc "link" item-metadata :test #'string=))
        ((assoc "title" item-metadata :test #'string=))
        ((assoc "description" item-metadata :test #'string=))
        ; XXX
        (t
          (warn (format nil "Unable to find GUID for item in feed ~a: ~a"
                            (feed-store:feed-id feed)
                            item-metadata))
          '("fake-guid" nil nil (format nil "~a" (get-universal-time))))))))

;; Save a list of metadata trees to the database.
(defun save-metadata (metadata object)
  (labels ((do-save-metadata (datum parent-id)
             (let ((datum-obj (make-instance 'feed-store:metadata
                                             :object-id (feed-store:object-id object)
                                             :object-type (feed-store:object-type object)
                                             :key (first datum)
                                             :value (fourth datum)
                                             :parent parent-id
                                             )))
               (feed-store:save-metadata datum-obj)
               ; save attributes
               (dolist (attr (second datum))
                 (feed-store:save-metadata
                   (make-instance 'feed-store:metadata
                                  :object-id (feed-store:object-id object)
                                  :object-type (feed-store:object-type object)
                                  :key (car attr)
                                  :value (cdr attr)
                                  :type feed-store:+metadata-attribute+
                                  :parent (feed-store:object-id datum-obj))))
               ; save child metadata recursively
               (dolist (child (third datum))
                 (do-save-metadata child (feed-store:object-id datum-obj))))))
    (feed-store:delete-metadata object)
    ; Save the root metadata
    (dolist (datum metadata)
      (do-save-metadata datum -1))))

(defun update-item (item item-metadata)
  ; TODO: compare 'updated' date and replace metadata if newer
  (declare (ignore item-metadata))
  item)

(defun get-published (item)
  (let ((published (assoc "published" item :test #'string=)))
    (simple-date:universal-time-to-timestamp
      (if published
        (parse-integer (fourth published))
        (get-universal-time)))))

(defgeneric update-feed (feed))

(defmethod update-feed ((feed feed-store:feed))
  (let ((found-new-items nil)) ; FIXME: yuck
    (multiple-value-bind (feed-data items) (*update-feed feed)
      (save-metadata feed-data feed)
      (dolist (item-metadata items)
        (let* ((guid (get-guid feed item-metadata))
               (item (feed-store:get-item guid)))
          (if item
            (update-item item item-metadata)
            (let ((item (make-instance 'feed-store:item
                                       :feed (feed-store:object-id feed)
                                       :guid guid
                                       :published (get-published item-metadata))))
              (feed-store:add-item item)
              (save-metadata item-metadata item)
              (setf found-new-items t)))))
      ; update timestamps and save feed object
      (let ((now (simple-date:universal-time-to-timestamp (get-universal-time))))
        (setf (feed-store:feed-updated feed) now)
        (when found-new-items
          (setf (feed-store:feed-new-items feed) now)
          (setf (feed-store:feed-unread feed)
                (feed-store:count-feed-unread (feed-store:feed-id feed)))))
      (feed-store:save-feed feed) 
      feed)))

(defmethod update-feed ((id integer))
  (let ((feed (feed-store:get-feed id)))
    (if feed
      (update-feed feed)
      ; TODO: error
      nil)))

(defun update-all ()
  (dolist (feed (feed-store:get-feeds))
    (update-feed feed)))

(defun schedule-all (minutes)
  (dolist (feed (feed-store:get-feeds))
    (sb-ext:schedule-timer
      (sb-ext:make-timer (lambda ()
                           (format t "UPDATING ~a" (feed-store:feed-id feed))
                           (terpri)
                           (update-feed feed)
                           ; TODO: reschedule update
                           )
                         :name (format nil "update-~a" (feed-store:feed-id feed)))
      (* minutes 60))))
