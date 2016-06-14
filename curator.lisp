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

;; curator.lisp
;;
;; This package implements a "curator" for feeds/items.  It is responsible for
;; providing a clean and consistent set of metadata for clients.
(in-package :curator)

(defun keep-text (element name)
  (list (cons name (feed-store:element-text element))))

(defun strip-html (element name)
  "Remove HTML tags from an element's text."
  `((,name . ,(string-trim '(#\space #\tab #\newline)
                           (sanitize:clean (feed-store:element-text element)
                                           sanitize:+restricted+)))))

(defun strip-scripts (element name)
  ; TODO: remove <script> tags (iframes?)
  (keep-text element name))

(defun parse-time (element name)
  "Compute a UNIX timestamp from an element storing a universal time."
  `((,name . ,(max 0
                   (- (parse-integer (feed-store:element-text element)
                                     :junk-allowed t)
                      2208988800)))))

(defun element->attributes-alist (element)
  "Return the attributes of ELEMENT as an association list."
  (mapcar (lambda (attr)
            (cons (feed-store:attribute-name attr)
                  (feed-store:attribute-value attr)))
          (feed-store:element-attributes element)))

(defun parse-media-data (data)
  (loop for datum in data
        when (let ((name (feed-store:element-name datum)))
               (cond
                 ((string= name "media:title")
                   `("title"     . ,(feed-store:element-text datum)))
                 ((string= name "media:content")
                   `("content"    . ,(element->attributes-alist datum)))
                 ((string= name "media:thumbnail")
                   `("thumbnail" . ,(element->attributes-alist datum)))
                 (t nil)))
        collect it))

(defun parse-media-content (element name)
  "Parse a top-level media:content tag into a media metadata object."
  `((,name ("content" . ,(element->attributes-alist element))
           ,@(parse-media-data (feed-store:element-children element)))))

(defun parse-media-group (element name)
  "Parse a media:group tag into a media metadata object."
  `((,name ,@(parse-media-data (feed-store:element-children element)))))

(defun curate-datum (datum handlers)
  "Compute an association list of metadata from the element DATUM, using
   curator spec HANDLERS."
  (let ((handler (assoc (feed-store:element-name datum) handlers :test #'string=)))
    (if handler
      (apply (third handler) datum (nthcdr 3 handler))
      nil)))

(defun handler-default (handler data curated-data)
  "Compute the default value for the datum specified by HANDLER."
  (if (functionp (second handler))
    (funcall (second handler) data curated-data)
    (second handler)))

(defun curate-metadata (object handlers)
  "Curate OBJECT's metadata by applying the curator spec HANDLERS to its root
   metadata elements."
  (let ((data (loop for datum in (feed-store:object-metadata object)
                    when (curate-datum datum handlers) append it)))
    ; add defaults
    (append data
            (loop for handler in handlers
                  unless (assoc (fourth handler) data :test #'string=)
                  collect (cons (fourth handler)
                                (handler-default handler 
                                                 (feed-store:object-metadata object)
                                                 data))))))

;; Bring order to the chaos of feed/item metadata.  Returns a simple alist.
;; TODO: "ext" option to turn on curator extensions  E.g., "ext=mrss" instructs
;;       the curator to include MRSS data.
(defgeneric curate (object))

(defmethod curate ((feed feed-store:feed))
  "Curate metadata for FEED."
  (append
    `(("id"      . ,(feed-store:feed-id feed))
      ("name"    . ,(feed-store:feed-name feed))
      ("fetcher" . ,(feed-store:feed-fetcher feed))
      ("parser"  . ,(feed-store:feed-parser feed))
      ("source"  . ,(feed-store:feed-source feed))
      ("added"   . ,(feed-store:feed-added feed))
      ("unread"  . ,(feed-store:feed-unread feed))
      ("tags"    . ,(feed-store:feed-tags feed)))
    (curate-metadata feed
      ;  Element Name  Default                      Handler       Metadata Name
      `(("title"       ,(feed-store:feed-name feed) ,#'strip-html "title")
        ("link"        ""                           ,#'keep-text  "link")
        ("description" ""                           ,#'strip-html "description")
        ("published"   nil                          ,#'parse-time "published")
        ("updated"     nil                          ,#'parse-time "updated")
        ))))

(defmethod curate ((item feed-store:item))
  "Curate metadata for ITEM."
  (labels ((default-content (data curated-data)
             (declare (ignore curated-data))
             (let ((desc (find-if (lambda (x)
                                    (string= (feed-store:element-name x)
                                             "description"))
                                  data)))
               (if desc
                 (feed-store:element-text desc)
                 ""))))
    (append
      `(("id"    . ,(feed-store:item-id item))
        ("feed"  . ,(feed-store:item-feed item))
        ("added" . ,(feed-store:item-added item))
        ("read"  . ,(feed-store:item-read item))
        ("tags"  . ,(feed-store:item-tags item)))
      (curate-metadata item
        ;  Element Name  Default            Handler          Metadata Name
        `(("title"       "Untitled"         ,#'strip-html    "title")
          ("link"        ""                 ,#'keep-text     "link")
          ("dc:creator"  ""                 ,#'keep-text     "creator")
          ("description" ""                 ,#'strip-html    "description")
          ("category"    ""                 ,#'keep-text     "category")
          ("content"     ,#'default-content ,#'strip-scripts "content")
          ("published"   ""                 ,#'parse-time    "published")
          ("updated"     ""                 ,#'parse-time    "updated")
          ; Media RSS
          ("media:content" nil ,#'parse-media-content "media")
          ("media:group"   nil ,#'parse-media-group   "media")
          )))))
