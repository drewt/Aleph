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
  "Retrieve the scheduler with the name NAME."
  (assoc name *schedules* :test #'string=))

(defun register-schedule (name schedule validate)
  "Register a new scheduler with the name NAME."
  (push (list name schedule validate) *schedules*))

(defun schedule-feed (feed)
  "Schedule an update for the feed FEED.  The feed's scheduler is invoked to
   determine when the update should occur."
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

(defun schedule-update (feed seconds)
  "Schedule an update for FEED after SECONDS seconds."
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
  "Schedule updates for all feeds."
  (dolist (feed (feed-store:get-feeds))
    (unless (gethash (feed-store:feed-id feed) *scheduled-updates*)
      (schedule-feed feed))))

(defun elapsed-since-update (feed)
  "Compute the number of seconds elapsed since FEED was last updated."
  (max 0 (- (get-universal-time)
            (simple-date:timestamp-to-universal-time
              (feed-store:feed-updated feed)))))

(defun elapsed-since-new-items (feed)
  "Compute the number of seconds elapsed since new items were found for FEED."
  (max 0 (- (get-universal-time)
            (simple-date:timestamp-to-universal-time
              (feed-store:feed-new-items feed)))))

(defun seconds-after-last-update (feed seconds)
  "Compute the number of seconds until SECONDS seconds will have elapsed since
   FEED was last updated."
  (max 0 (- seconds (elapsed-since-update feed))))

;;
;; Validator helpers
;;

(defun positive-integer-p (string)
  "Return T if STRING contains only positive integers."
  (loop for char across string
        always (digit-char-p char)))

(defun *parse-time-part (part)
  "Parse PART, which should be a part of a time string (e.g. \"3d\") into a
   number and a unit (e.g. '(\"3\" \"d\"))."
  (list (subseq part 0 (1- (length part)))
        (subseq part (1- (length part)))))

(defun time-valid-p (string)
  "Return T if STRING is a valid time string (e.g. \"3d,4h,15m\")."
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
  "Return T if STRING is either a time string, or a positive integer."
  (or (positive-integer-p string)
      (time-valid-p string)))

;;
;; Parameter parsing
;;

(defun parse-params (string)
  (split-sequence #\; string))

(defun parse-time-part (part)
  "Parse PART, which should be a part of a time string (e.g. \"3d\") into a
   number and a unit (e.g. '(3 #\d))."
  (destructuring-bind (n unit) (*parse-time-part part)
    (list (parse-integer n)
          (char unit 0))))

(defun parse-time (string)
  "Parse a time string (e.g. \"3d,4h,15m\"), returning the time as a number
   of seconds."
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
  "Parse PARAM, which should be either a time string or a positive integer,
   returning a number of seconds."
  (if (positive-integer-p param)
    (* 60 (parse-integer param))
    (parse-time param)))

;;
;; Scheduling policies
;;

(defun schedule-periodic (feed param)
  "Schedule an update after a fixed number of minutes."
  (seconds-after-last-update feed (parse-minutes-or-time param)))

(defun schedule-threshold (feed param)
  "Schedule an update for one of two fixed numbers of minutes.  If less than
   THRESHOLD minutes have passed since the feed received new items, then the
   update is scheduled for BEFORE minutes; otherwise it is scheduled for
   AFTER minutes."
  (destructuring-bind (threshold before after) (parse-params param)
    (let ((elapsed (elapsed-since-new-items feed)))
      (if (>= elapsed (parse-minutes-or-time threshold))
        (seconds-after-last-update feed (parse-minutes-or-time after))
        (seconds-after-last-update feed (parse-minutes-or-time before))))))

(defun threshold-param-valid-p (param)
  "Return T if PARAM is a list of three values, separated by semicolons, and
   each value is either a time string or a positive integer."
  (let ((params (parse-params param)))
    (if (= (length params) 3)
      (destructuring-bind (threshold before after) params
        (and (minutes-or-time-p threshold)
             (minutes-or-time-p before)
             (minutes-or-time-p after)))
      nil)))

(register-schedule "periodic"  #'schedule-periodic  #'minutes-or-time-p)
(register-schedule "threshold" #'schedule-threshold #'threshold-param-valid-p)
