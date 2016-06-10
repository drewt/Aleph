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

(in-package :cl-user)

(defpackage :config
  (:use :cl)
  (:export :merge-config :load-from-file :get-value))

;; Data store, type definitions
(defpackage :feed-store
  (:use :cl)
  (:import-from :postmodern
                :with-transaction
                :insert-dao
                :select-dao
                :save-dao
                :update-dao
                :query-dao
                :query
                :execute)
  (:import-from :s-sql :sql :sql-compile)
  (:import-from :simple-date :universal-time-to-timestamp)
  (:export :with-connection

           :initialize

           :feed
           :feed-id
           :feed-name
           :feed-fetcher
           :feed-parser
           :feed-source
           :feed-added
           :feed-updated
           :feed-new-items
           :feed-schedule
           :feed-schedule-parameter
           :feed-unread
           :feed-tags
           :feed-metadata

           :item
           :item-id
           :item-feed
           :item-added
           :item-read
           :item-tags
           :item-metadata

           :tag

           :metadata
           +metadata-element+
           +metadata-attribute+
           :delete-metadata

           :element
           :element-name
           :element-text
           :element-children
           :element-attributes
           
           :attribute
           :attribute-name
           :attribute-value

           :object-id
           :object-type
           :object-metadata

           :add-feed
           :save-feed
           :save-metadata
           :rm-feed
           :get-feeds
           :get-feed
           :mark-item-read
           :mark-feed-read
           :mark-all-read
           :add-items
           :add-item
           :get-items
           :get-item
           :query-items
           :update-feed-unread
           :count-feed-unread))

;; Fetchers
(defpackage :feed-source
  (:use :cl)
  (:export :fetch))

;; Parser meta-package
(defpackage :feed-parser
  (:use :cl)
  (:export :parse
           :get-parser
           :register-parser))

(defpackage :curator
  (:use :cl)
  (:export :curate))

;; Feed update logic and etc.
(defpackage :controller
  (:use :cl)
  (:export :add-feed
           :update-feed
           :update-all))

;; Feed update scheduling
(defpackage :scheduler
  (:use :cl :split-sequence)
  (:export :schedule-feed
           :schedule-all
           :register-schedule))

;; HTTP server with RESTful API
(defpackage :http-server
  (:use :cl :hunchentoot)
  (:shadow :start :stop)
  (:export :start :stop))
