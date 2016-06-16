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

;; feed-source.lisp
;;
;; This package implements the fetchers for different types of source
;; locations (HTTP(S), file, command).
(in-package :feed-source)

;; Fetcher for HTTP resources.
(defun fetch-http (source)
  "Return a stream for the HTTP resource identified by the URI SOURCE."
  (drakma:http-request source :want-stream t))

;; Fetcher for local files.
(defun fetch-file (source)
  "Return a stream for the file identified by the path SOURCE."
  (open source))

;; This fetcher will run source as a shell command and return the output
;; stream.
;; TODO: option to prevent command feeds from being added remotely.
;; TODO: option to disable command feeds altogether
(defun fetch-command (source)
  "Return a stream for the output of the shell command SOURCE."
  (sb-ext:run-program "sh" (list "-c" source) :output :stream :wait nil))

(defparameter *fetchers*
  (list (cons "http" #'fetch-http)
        (cons "file" #'fetch-file)
        (cons "command" #'fetch-command)))

(defun get-fetcher (name)
  "Return the fetcher with the name NAME."
  (let ((fetcher (assoc name *fetchers* :test #'string=)))
    (if fetcher
      (cdr fetcher)
      nil)))

(defun register-fetcher (name fetcher)
  "Register FETCHER as a fetcher with the name NAME."
  (setf *fetchers* (acons name fetcher *fetchers*)))

(defun fetch (fetcher-name source)
  "Return a stream for SOURCE using the fetcher named by FETCHER-NAME."
  (let ((fetcher (get-fetcher fetcher-name)))
    (if fetcher
      (funcall fetcher source)
      ; TODO: error
      nil)))
