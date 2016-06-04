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

;; scheduler.lisp
;;
;; This package implements feed update scheduling policies.
(in-package :scheduler)

;; TODO: use portable libraries for timers/locks

(defvar *scheduled-updates-lock* (sb-thread:make-mutex :name "*scheduled-updates*"))
(defparameter *scheduled-updates* (make-hash-table))

(defparameter *schedules* nil)

(defun get-schedule (name)
  (assoc name *schedules* :test #'string=))

(defun register-schedule (name schedule validate)
  (push (list name schedule validate) *schedules*))

;; Schedule an update for a feed.  The length of time to wait before updating
;; is determined by the scheduling policy of the feed object.
(defun schedule-feed (feed)
  (let ((schedule (get-schedule (feed-store:feed-schedule feed))))
    (if schedule
      (schedule-update feed (funcall (second schedule)
                                     feed
                                     (feed-store:feed-schedule-parameter feed)))
      (warn (format nil "Unknown schedule for feed ~a (~a): ~a"
                        (feed-store:feed-id feed)
                        (feed-store:feed-name feed)
                        (feed-store:feed-schedule feed)))))
  feed)

;; Schedule an update for a feed after a number of seconds.
(defun schedule-update (feed seconds)
  (let* ((id (feed-store:feed-id feed))
         (timer (sb-ext:make-timer (lambda ()
                                     (feed-store:with-connection
                                       (let ((feed (feed-store:get-feed id)))
                                         (sb-thread:with-mutex (*scheduled-updates-lock*)
                                           (remhash id *scheduled-updates*))
                                         (controller:update-feed feed)
                                         (schedule-feed feed))))
                                   :thread t
                                   :name (format nil "update-~a" id))))
    (sb-thread:with-mutex (*scheduled-updates-lock*)
      (setf (gethash id *scheduled-updates*) timer))
    (sb-ext:schedule-timer timer seconds)))

(defun schedule-all ()
  (dolist (feed (feed-store:get-feeds))
    (unless (gethash (feed-store:feed-id feed) *scheduled-updates*)
      (schedule-feed feed))))

(defun elapsed-since-update (feed)
  (max 0 (- (get-universal-time)
            (simple-date:timestamp-to-universal-time
              (feed-store:feed-updated feed)))))

(defun elapsed-since-new-items (feed)
  (max 0 (- (get-universal-time)
            (simple-date:timestamp-to-universal-time
              (feed-store:feed-new-items feed)))))

(defun seconds-after-last-update (feed seconds)
  (max 0 (- seconds (elapsed-since-update feed))))

;;
;; Validator helpers
;;

(defun positive-integer-p (string)
  (loop for char across string
        always (digit-char-p char)))

;; Parse part of a time string (e.g. "3d") into a number and a unit (e.g.
;; '("3" "d"))
(defun *parse-time-part (part)
  (list (subseq part 0 (1- (length part)))
        (subseq part (1- (length part)))))

(defun time-valid-p (string)
  (labels ((unit-valid-p (unit)
             (member (char unit 0) '(#\s #\m #\h #\d)))
           (time-part-valid-p (part)
             (and (> (length part) 1)
                  (destructuring-bind (n unit) (*parse-time-part part)
                    (and (positive-integer-p n)
                         (unit-valid-p unit))))))
    (loop for val in (split-sequence #\, string)
          always (time-part-valid-p val))))

(defun minutes-or-time-p (string)
  (or (positive-integer-p string)
      (time-valid-p string)))

;;
;; Parameter parsing
;;

(defun parse-params (string)
  (split-sequence #\; string))

(defun parse-time-part (part)
  (destructuring-bind (n unit) (*parse-time-part part)
    (list (parse-integer n)
          (char unit 0))))

;; Parse a string representing a interval of time, such as "3d", "3d,4h", etc.
(defun parse-time (string)
  (loop for val in (split-sequence #\, string)
        with time = 0
        finally (return time)
        do
    (destructuring-bind (n unit) (parse-time-part val)
      (case unit
        ((#\s) (setf time (+ time n)))
        ((#\m) (setf time (+ time (* 60 n))))
        ((#\h) (setf time (+ time (* 60 60 n))))
        ((#\d) (setf time (+ time (* 24 60 60 n))))
        (otherwise
          (warn (format nil "Unknown unit int time string: ~a" (string unit))))))))

(defun parse-minutes-or-time (param)
  (if (positive-integer-p param)
    (* 60 (parse-integer param))
    (parse-time param)))

;;
;; Scheduling policies
;;

;; Schedule an update after a fixed number of minutes.  The parameter must
;; contain a string suitable for passing to parse-integer.
(defun schedule-periodic (feed param)
  (seconds-after-last-update feed (parse-minutes-or-time param)))

(defun schedule-threshold (feed param)
  (destructuring-bind (threshold before after) (parse-params param)
    (let ((elapsed (elapsed-since-new-items feed)))
      (if (>= elapsed (parse-minutes-or-time threshold))
        (seconds-after-last-update feed (parse-minutes-or-time after))
        (seconds-after-last-update feed (parse-minutes-or-time before))))))

(defun threshold-param-valid-p (param)
  (let ((params (parse-params param)))
    (if (= (length params) 3)
      (destructuring-bind (threshold before after) params
        (and (minutes-or-time-p threshold)
             (minutes-or-time-p before)
             (minutes-or-time-p after)))
      nil)))

(register-schedule "periodic"  #'schedule-periodic  #'minutes-or-time-p)
(register-schedule "threshold" #'schedule-threshold #'threshold-param-valid-p)
