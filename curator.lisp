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
  `((,name . ,(string-trim '(#\space #\tab #\newline)
                           (sanitize:clean (feed-store:element-text element)
                                           sanitize:+restricted+)))))

(defun strip-scripts (element name)
  ; TODO: remove <script> tags (iframes?)
  (keep-text element name))

(defun parse-time (element name)
  `((,name . ,(max 0
                   (- (parse-integer (feed-store:element-text element)
                                     :junk-allowed t)
                      2208988800)))))

(defun curate-datum (datum handlers)
  (let ((handler (assoc (feed-store:element-name datum) handlers :test #'string=)))
    (if handler
      (apply (third handler) datum (nthcdr 3 handler))
      nil)))

(defun handler-default (handler data curated-data)
  (if (functionp (second handler))
    (funcall (second handler) data curated-data)
    (second handler)))

;; Curate an object's metadata by applying the given handlers to the root
;; metadata elements.
(defun curate-metadata (object handlers)
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
;; TODO: "format" option to choose different curator behaviour.  E.g.,
;;       "format=mrss" instructs the curator to include MRSS data.
(defgeneric curate (object))

(defmethod curate ((feed feed-store:feed))
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
        ;  Element Name  Default              Handler          Metadata Name
        `(("title"       "Untitled"         ,#'strip-html    "title")
          ("link"        ""                 ,#'keep-text     "link")
          ("dc:creator"  ""                 ,#'keep-text     "creator")
          ("description" ""                 ,#'strip-html    "description")
          ("category"    ""                 ,#'keep-text     "category")
          ("content"     ,#'default-content ,#'strip-scripts "content")
          ("published"   ""                 ,#'parse-time    "published")
          ("updated"     ""                 ,#'parse-time    "updated")
          )))))
