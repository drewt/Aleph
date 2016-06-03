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

;; config.lisp
;;
;; This package handles loading, storing and retrieving configuration settings.
(in-package :config)

(defparameter *config*
  `((:database
      (:name . "aleph")
      (:user . "aleph")
      (:pass . "aleph")
      (:host . "localhost"))
    (:server
      (:port          . 8080)
      (:document-root . "www/"))))

;; Merge the given configuration alist with *config*.  Settings not specified
;; in the given alist remain unchanged in *config*.
(defun merge-config (config)
  (labels ((*merge-config (new old)
             (dolist (new-pair new)
               (let ((old-pair (assoc (car new-pair) old)))
                 (cond
                   ((not old-pair)
                     ; append new config value
                     (nconc old (list new-pair)))
                   ((not (eq (listp (cdr old-pair))
                             (listp (cdr new-pair))))
                     (warn (format nil "Skipping config entry '~a': type mismatch"
                                       (car new-pair))))
                   ((listp (cdr old-pair))
                     (*merge-config (cdr new-pair) (cdr old-pair)))
                   (t
                     (setf (cdr old-pair) (cdr new-pair))))))))
    (*merge-config config *config*)))

;; Update *config* with values read from a file.
(defun load-from-file (filename)
  (with-open-file (stream filename :if-does-not-exist nil)
    (if stream
      (merge-config (read stream))
      (warn (format nil "Configuration file '~a' does not exist" filename)))))

;; Get the value of a configuration setting.  'Name' should be a list of
;; keywords identifying the setting, such as '(:database :name).
(defun get-value (name)
  (labels ((*get-value (name val)
             (if name
               (let ((next (assoc (car name) val)))
                 (if next
                   (*get-value (cdr name) (cdr next))
                   nil))
               val)))
    (*get-value name *config*)))
 
(load-from-file "rc.lisp")
