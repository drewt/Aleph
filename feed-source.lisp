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
  (drakma:http-request source :want-stream t))

;; Fetcher for local files.
(defun fetch-file (source)
  (open source))

;; This fetcher will run source as a shell command and return the output
;; stream.
;; TODO: option to prevent command feeds from being added remotely.
;; TODO: option to disable command feeds altogether
(defun fetch-command (source)
  (sb-ext:run-program "sh" (list "-c" source) :output :stream :wait nil))

(defparameter *fetchers*
  (list (cons "http" #'fetch-http)
        (cons "file" #'fetch-file)
        (cons "command" #'fetch-command)))

(defun get-fetcher (name)
  (let ((fetcher (assoc name *fetchers* :test #'string=)))
    (if fetcher
      (cdr fetcher)
      nil)))

(defun register-fetcher (name fetcher)
  (acons name fetcher *fetchers*))

(defun fetch (fetcher-name source)
  (let ((fetcher (get-fetcher fetcher-name)))
    (if fetcher
      (funcall fetcher source)
      ; TODO: error
      nil)))
