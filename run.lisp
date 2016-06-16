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

(ql:quickload "cl-date-time-parser")
(ql:quickload "cl-json")
(ql:quickload "cxml")
(ql:quickload "drakma")
(ql:quickload "hunchentoot")
(ql:quickload "postmodern")
(ql:quickload "puri")
(ql:quickload "simple-date")
(ql:quickload "sanitize")
(ql:quickload "split-sequence")

;; Configure Drakma to use a proxy if the http_proxy environment variable is
;; set.  Unfortunately Drakma only supports HTTP proxies, so in order to use a
;; SOCKS proxy (e.g. Tor) it is necessary to tunnel through an HTTP proxy first
;; (e.g. Privoxy).
(let ((proxy (sb-ext:posix-getenv "http_proxy")))
  (when proxy
    (let ((uri (puri:parse-uri proxy)))
      (if (and (puri:uri-scheme uri)
               (not (eql (puri:uri-scheme uri) :HTTP)))
        (warn (format nil "Ignoring proxy: ~a; only HTTP proxies are supported" proxy))
        (setf drakma:*default-http-proxy*
          (if (puri:uri-port uri)
            (list (puri:uri-host uri)
                  (puri:uri-port uri))
            (puri:uri-host uri)))))))

(load "package.lisp")
(load "config.lisp")
(load "feed-store.lisp")
(load "feed-source.lisp")
(load "feed-parser.lisp")
(load "curator.lisp")
(load "controller.lisp")
(load "scheduler.lisp")
(load "http-server.lisp")

; Load plugins
(loop for f in (directory "plugins/*.lisp") do
  (load f))

(feed-store:initialize)
(scheduler:schedule-all)
(http-server:start)
