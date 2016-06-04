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

;; feed-store.lisp
;;
;; This package defines the fundamental data types of the aggregator (feeds,
;; items, and metadata) and provides an interface to the database.
(in-package :feed-store)

;(defparameter *db-name* "aggregator")
;(defparameter *db-user* "drew")
;(defparameter *db-pass* "aggregator")
;(defparameter *db-host* "localhost")
(defparameter *db-name* (config:get-value '(:database :name)))
(defparameter *db-user* (config:get-value '(:database :user)))
(defparameter *db-pass* (config:get-value '(:database :pass)))
(defparameter *db-host* (config:get-value '(:database :host)))

(defmacro with-connection (&body body)
  `(postmodern:with-connection (list ,*db-name* ,*db-user* ,*db-pass* ,*db-host* :pooled-p t)
     ,@body))

(if (or (not postmodern:*database*)
        (not (postmodern:connected-p postmodern:*database*)))
  (postmodern:connect-toplevel *db-name* *db-user* *db-pass* *db-host*))

(defun delete-db ()
  (execute "DROP TABLE feed")
  (execute "DROP TABLE item")
  (execute "DROP TABLE metadata")
  (execute "DROP TABLE aleph"))

(defun create-db ()
  (execute (postmodern:dao-table-definition 'feed))
  (execute (postmodern:dao-table-definition 'item))
  (execute (postmodern:dao-table-definition 'metadata))
  (execute
    (sql (:create-table :aleph
           ((:version :type integer)
            (:created :type timestamp)))))
  (execute
    (sql (:insert-into 'aleph
          :set 'version 0
               'created (universal-time-to-timestamp (get-universal-time))))))

(defun reset-db ()
  (delete-db)
  (create-db))

(defun db-created-p ()
  (> (execute
       (sql (:select 'relname :from 'pg_class :where (:= 'relname "aleph"))))
     0))

(defun initialize ()
  (unless (db-created-p)
    (create-db)))

;; The feed object.
(defclass feed ()
  ((id
     :accessor feed-id
     :col-type serial)
   ; User-friendly identifier for the feed
   (name
     :initarg :name
     :initform "Untitled"
     :accessor feed-name
     :col-type string)
   ; Name of the source location type
   (fetcher
     :initarg :fetcher
     :initform "http"
     :accessor feed-fetcher
     :col-type string)
   ; Name of the parser to use for the feed
   (parser
     :initarg :parser
     :initform "auto"
     :accessor feed-parser
     :col-type string)
   ; Source of the feed (URI, file path, etc.)
   (source
     :initarg :source
     :initform (error "Must supply a feed source.")
     :accessor feed-source
     :col-type string)
   ; Date at which the feed was added to the aggregator
   (added
     :initarg :added
     :initform (simple-date:universal-time-to-timestamp (get-universal-time))
     :reader feed-added
     :col-type timestamp)
   ; XXX: Refers to the last time the aggregator checked for updates, NOT the
   ;      last time the feed changed.
   (updated
     :initarg :updated
     :initform (simple-date:universal-time-to-timestamp 0)
     :accessor feed-updated
     :col-type timestamp)
   ; Date at which the aggregator last found new items for the feed
   (new-items
     :initarg :updated
     :initform (simple-date:universal-time-to-timestamp 0)
     :accessor feed-new-items
     :col-type timestamp)
   ; Name of the update scheduling policy to use for the feed
   (schedule
     :initarg :schedule
     :initform "periodic" ; TODO: use "auto" policy that respects feed hints
     :accessor feed-schedule
     :col-type string)
   ; Argument to pass to the update policy
   (schedule-parameter
     :initarg :schedule-parameter
     :initform "30"
     :accessor feed-schedule-parameter
     :col-type string)
   (unread
     :initform 0
     :accessor feed-unread
     :col-type integer)
   ; XXX: not stored in feed record
   (tags
     :initarg :tags
     :initform '()
     :accessor feed-tags)
   (metadata
     :initarg :metadata
     :initform '()
     :accessor feed-metadata))
  (:metaclass postmodern:dao-class)
  (:keys id))

;; A feed item.  Note that Item objects do not directly contain any content;
;; rather, the items contents are stored as metadata.
(defclass item ()
  ((id
     :accessor item-id
     :col-type serial)
   ; ID of the feed to which the item belongs
   (feed
     :initarg :feed
     :initform (error "Must supply a feed for item.")
     :accessor item-feed
     :col-type integer)
   ; Globally Unique Identifier for the item
   (guid
     :initarg :guid
     :initform (error "Must supply an item guid.")
     :accessor item-guid
     :col-type string)
   ; XXX: Refers to the time at which the item was first retrieved, NOT the
   ;      time it was added to the feed.
   (added
     :initform (simple-date:universal-time-to-timestamp (get-universal-time))
     :reader item-added
     :col-type timestamp)
   ; Date at which the item was published, or our best guess.
   ; XXX: We store this in the item object so that we can impose an ordering
   ;      in items within a feed without having to query the metadata table.
   (published
     :initarg :published
     :initform (simple-date:universal-time-to-timestamp (get-universal-time))
     :accessor item-published
     :col-type timestamp)
   ; True if the item has been marked read
   (read
     :initarg :read
     :initform nil
     :accessor item-read
     :col-type boolean)
   ; XXX: not stored in item record
   (tags
     :initarg :tags
     :initform '()
     :accessor item-tags)
   (metadata
     :initarg :metadata
     :initform '()
     :accessor item-metadata))
  (:metaclass postmodern:dao-class)
  (:keys id))

;; Object type constants.  Used to identify which kind of object a tag/metadata
;; refers to.
(defconstant +feed+ 0)
(defconstant +item+ 1)
(defconstant +metadata+ 2)

;; Arbitrary strings may be associated with a feed or item.
(defclass tag ()
  ; ID of the tagged object
  ((object-id
     :initarg :object-id
     :initform (error "Must supply an object ID for tag.")
     :reader tag-object-id
     :col-type integer)
   ; Type of the tagged object
   (object-type
     :initarg :object-type
     :initform (error "Must supply an object type for tag.")
     :reader tag-object-type
     :col-type smallint)
   ; The tag itself
   (name
     :initarg :name
     :initform (error "Must supply a name for tag.")
     :accessor item-tag-name
     :col-type string))
  (:metaclass postmodern:dao-class))

;; Metadata type constants.
(defconstant +metadata-element+ 0)
(defconstant +metadata-attribute+ 1)

;; Arbitrary key-value pairs may be associated with a feed or item.  Metadata
;; may also be organized into a tree structure with the parent slot.  Circular
;; data is NOT supported.
(defclass metadata ()
  ((id
     :accessor metadata-id
     :col-type serial)
   ; ID of the object associated with the metadata
   (object-id
     :initarg :object-id
     :initform (error "Must supply an object ID for metadata.")
     :accessor metadata-object-id
     :col-type integer)
   ; Type of the object associated with the metadata
   (object-type
     :initarg :object-type
     :initform (error "Must supply an object type for metadata.")
     :accessor metadata-object-type
     :col-type smallint)
   ; The name of the metadata
   (key
     :initarg :key
     :initform (error "Must supply a key for metadata.")
     :accessor metadata-key
     :col-type string)
   ; The value of the metadata
   (value
     :initarg :value
     :initform ""
     :accessor metadata-value
     :col-type string)
   ; The type of the metadata.  This may be either +metadata-element+ or
   ; +metadata-attribute+.  These types exist only to provide a simple mapping
   ; for arbitrary XML data.  They are quite meaningless for other kinds of
   ; data.
   (type
     :initarg :type
     :initform +metadata-element+
     :accessor metadata-type
     :col-type smallint)
   ; The ID of the metadata's parent datum in the hierarchy.  -1 means no
   ; parent (i.e. a root node).
   (parent
     :initarg :parent
     :initform -1
     :accessor metadata-parent
     :col-type integer))
  (:metaclass postmodern:dao-class)
  (:keys id))

;; Intermediate representation for metadata.  An element object roughly
;; corresponds to an XML element.  The main difference is that there is a
;; single text node, with no ordering relative to other child nodes.
(defclass element ()
  ((name :initarg :name :accessor element-name)
   (text :initarg :text :accessor element-text)
   (attributes :initarg :attributes :initform nil :accessor element-attributes)
   (children :initarg :children :initform nil :accessor element-children)))

;; Intermediate representation for metadata.  An attribute object is a simple
;; key-value pair, analogous to an XML attribute.
(defclass attribute ()
  ((name :initarg :name :accessor attribute-name)
   (value :initarg :value :accessor attribute-value)))

;; Object-abnostic accessors.  Useful for writing functions which operate
;; on metadata, since these functions generally don't care what kind of
;; object the metadata references.

(defgeneric object-id (object))
(defmethod object-id ((object feed)) (feed-id object))
(defmethod object-id ((object item)) (item-id object))
(defmethod object-id ((object metadata)) (metadata-id object))

(defgeneric object-type (object))
(defmethod object-type ((object feed)) +feed+)
(defmethod object-type ((object item)) +item+)
(defmethod object-type ((object metadata)) +metadata+)

(defgeneric object-metadata (object))
(defmethod object-metadata ((object feed)) (feed-metadata object))
(defmethod object-metadata ((object item)) (item-metadata object))
(defgeneric (setf object-metadata) (data object))
(defmethod (setf object-metadata) (data (object feed)) (setf (feed-metadata object) data))
(defmethod (setf object-metadata) (data (object item)) (setf (item-metadata object) data))

(defun metadata->element (datum)
  (make-instance 'element
                 :name (metadata-key datum)
                 :text (metadata-value datum)))

(defun metadata->attribute (datum)
  (make-instance 'attribute
                 :name (metadata-key datum)
                 :value (metadata-value datum)))

;; Converts a list of metadata objects to a list of element hierarchies.
(defun structure-metadata (data)
  (let ((elements (make-hash-table)))
    ; store elements in hash table by id
    (loop for datum in data
          when (= (metadata-type datum) +metadata-element+) do
      (setf (gethash (metadata-id datum) elements)
            (metadata->element datum)))
    ; construct element hierarchy
    (dolist (datum data)
      (when (> (metadata-parent datum) 0)
        (let ((parent (gethash (metadata-parent datum) elements)))
          (if parent
            (cond
              ((= (metadata-type datum) +metadata-element+)
                (push (gethash (metadata-id datum) elements)
                      (element-children parent)))
              ((= (metadata-type datum) +metadata-attribute+)
                (push (metadata->attribute datum)
                      (element-attributes parent)))
              (t
                (warn (format nil "Unknown metadata type: ~a" (metadata-type datum)))))
            (warn (format nil "Orphaned child in metadata: <~a: ~a> with parent ~a"
                              (metadata-key datum)
                              (metadata-value datum)
                              (metadata-parent datum)))))))
    ; collect the root elements
    (loop for datum in data
          when (and (= (metadata-type datum) +metadata-element+)
                    (= (metadata-parent datum) -1))
          collect (gethash (metadata-id datum) elements))))

(defun get-metadata (object)
  (setf (object-metadata object)
        (structure-metadata
          (select-dao 'metadata (:and (:= 'object-type (object-type object))
                                      (:= 'object-id (object-id object))))))
  object)

(defun add-feed (feed)
  (insert-dao feed))

(defun save-feed (feed)
  (save-dao feed))

(defun save-metadata (datum)
  (insert-dao datum))

(defun delete-metadata (object)
  (execute
    (sql (:delete-from 'metadata
          :where (:and (:= 'object-id (object-id object))
                       (:= 'object-type (object-type object)))))))

(defgeneric rm-feed (feed))

(defmethod rm-feed ((id integer))
  ; delete item metadata
  (dolist (item (select-dao 'item (:= 'feed id)))
    (execute (sql (:delete-from 'metadata
                   :where (:and (:= 'object-type +item+)
                                (:= 'object-id (item-id item)))))))
  ; delete feed metadata
  (execute (sql (:delete-from 'metadata
                 :where (:and (:= 'object-type +feed+)
                              (:= 'object-id id)))))
  ; delete feed and items
  (execute (sql (:delete-from 'item :where (:= 'feed id))))
  (execute (sql (:delete-from 'feed :where (:= 'id id)))))

(defmethod rm-feed ((feed feed))
  (rm-feed (feed-id feed)))

(defun get-feeds ()
  (select-dao 'feed t 'name))

;; Get a feed object, but don't fill out its metadata slots.
(defun *get-feed (id)
  (let ((feed (select-dao 'feed (:= 'id id))))
    (if feed
      (first feed)
      nil)))

;; Get a feed object and fill out its metadata slots.
(defgeneric get-feed (feed))

(defmethod get-feed ((id integer))
  (let ((feed (*get-feed id)))
    (if feed
      (get-metadata feed)
      nil)))

(defmethod get-feed ((feed feed))
  (get-feed (feed-id feed)))

(defun update-feed-unread (id)
  (execute (sql (:update 'feed :set 'unread (:select (:count :*) :from 'item
                                             :where (:and (:= 'read nil)
                                                          (:= 'feed id)))
                 :where (:= 'id id)))))

(defun count-feed-unread (id)
  (caar (query (:select (:count :*) :from 'item
                :where (:and (:= 'read nil)
                             (:= 'feed id))))))

(defun add-items (items)
  (if items
    (with-transaction ()
      (loop for item in items do (insert-dao item))
      (update-feed-unread (item-feed (first items))))))

(defun add-item (item)
  (insert-dao item))

; TODO: options (e.g. unread, added-since, etc.)
(defgeneric get-items (feed))

(defmethod get-items ((id integer))
  (mapcar #'get-metadata (select-dao 'item (:= 'feed id) (:desc 'published))))

(defmethod get-items ((feed feed))
  (get-items (feed-id feed)))

;; Retrieve an item record from the database.
(defgeneric get-item (id))

(defmethod get-item ((id integer))
  (let ((item (select-dao 'item (:= 'id id))))
    (if item
      (first item)
      nil)))

(defmethod get-item ((guid string))
  (let ((item (select-dao 'item (:= 'guid guid))))
    (cond
      ((not item) nil)
      ((> 1 (length item)) nil) ; TODO: guid not unique error
      (t (first item)))))

;;
(defun query-items (&key (limit nil) (unread nil) (feed nil))
  (let ((query (list :select :* :from 'item))
        (constraints
          (append (if unread '((:= read nil))   '())
                  (if feed   `((:= feed ,feed)) '()))))
    (when constraints
      (setf query (append query `(:where (:and ,@constraints)))))
    (when limit
      (setf query (list :limit query limit)))
    (query-dao 'item (sql-compile query))))

;; Mark an item as read and update the unread count on the corresponding feed.
(defgeneric mark-item-read (item))

(defmethod mark-item-read ((item item))
  (mark-item-read (item-id item)))

(defmethod mark-item-read ((id integer))
  (with-transaction ()
    (let ((item (get-item id)))
      (if item
        (unless (item-read item)
          (setf (item-read item) t)
          (update-dao item)
          (update-feed-unread (item-feed item)))
        (warn (format nil "Tried to mark non-existent item as read: ~a" id))))))

(defgeneric mark-feed-read (feed))

(defmethod mark-feed-read ((id integer))
  (with-transaction ()
    (execute (sql (:update 'item :set 'read t :where (:= 'feed id))))
    (execute (sql (:update 'feed :set 'unread 0 :where (:= 'id id))))))

(defmethod mark-feed-read ((feed feed))
  (mark-feed-read (feed-id feed)))

