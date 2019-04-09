;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SPY; Base: 10 -*-

;;; spy-puzzle -- represent and solve a puzzle from the book,
;;; "Algorithmic Puzzles", by Anany Levitin and Maria Levitin, (2011,
;;; Oxford University Press).
;;;
;;; Copyright (c) 2019, Vijay Lakshminarayanan <laksvij@hawk.iit.edu>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; In a computer game, a spy is located on a one-dimensional line. At
;;; time `0`, the spy is at location `a`. With each time interval, the
;;; spy moves `b` units to the right if `b≥0`, and `|b|` units to the
;;; left if `b<0`.  Both `a` and `b` are fixed integers, but they are
;;; unknown to you. Your goal is to identify the spy’s location by
;;; asking at each time interval (starting at time 0) whether the spy
;;; is currently at some location of your choosing. For example, you
;;; can ask whether the spy is currently at location 19, to which you
;;; will receive a truthful yes/no answer. If the answer is “yes,” you
;;; reach your goal; if theanswer is “no,” you can ask the next time
;;; whether the spy is at the same or another location of your
;;; choice. Devise an algorithm that will find the spy after a finite
;;; number questions.

(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(defpackage spy
  (:use :common-lisp :hunchentoot :cl-who))

(in-package :spy)

(defstruct spy
  (a (* (random 100) (if (zerop (random 2)) -1 +1)) :type integer)
  (b (* (random 100) (if (zerop (random 2)) -1 +1)) :type integer)
  (time 0 :type fixnum)
  (game-id "" :type string))

(defun spy-location (spy)
  (+ (spy-a spy) (* (spy-b spy) (spy-time spy))))

(defun clock-tick (spy)
  (incf (spy-time spy)))

(defparameter *spies* (make-hash-table :test #'equal :synchronized t)
  "table of spies")

(defun new-game-id ()
  "Returns a unique game id."
  (loop as game-id = (format nil "~a" (random 1000000000000000000000))
        until (null (gethash game-id *spies*))
        finally (return game-id)))

;;; Some day this should be MVC'd

(defun form-find (stream game-id &optional (value 0))
  (with-html-output (stream)
    ((:form :action "/locate")
     ((:input :type "text" :name "location" :value value))
     ((:input :type "hidden" :name "gameid" :value game-id))
     ((:input :type "submit" :value "Submit")))))

;;; The actual web acceptors

(defvar *root-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))

(define-easy-handler (start-page :uri "/") ()
  (let ((session (start-session))
        (game-id (new-game-id))
        (new-game-message ""))
    (when (session-value 'spy session)
      (setq new-game-message "<div>Starting new game</div>")
      (remhash (spy-game-id (session-value 'spy session)) *spies*))
    (let ((spy (make-spy :game-id game-id)))
      (setf (session-value 'spy session) spy
            (gethash game-id *spies*) spy))
    (format t "new-game-message = ~a~%spy=~a~&"
            new-game-message
            (session-value 'spy session))
    (setf (hunchentoot:content-type*) "text/html"
          (hunchentoot:header-out "x-spy-game-id") game-id)
    (with-html-output-to-string (s)
      (:html
       (:head
        (:title "Catch the spy"))
       (:body
        (:h1 "Catching a spy")
        (:div "
            In a computer game, a spy is located on a one-dimensional
            line. At time 0, the spy is at location " (:code "a")
            ". With each time interval, the spy moves "
            (:code "b") "units to the right if " (:code "b >= 0")
            ", and " (:code "|b|") " units to the left if "
            (:code "b < 0") ".  ")
        (:div "Both " (:code "a") " and " (:code "b") " are fixed
            integers, but they are unknown to you.")
        (:div "Your goal is to identify the spy’s location by asking
            at each time interval (starting at time 0) whether the spy
            is currently at some location of your choosing. For
            example, you can ask whether the spy is currently at
            location 19, to which you will receive a truthful yes/no
            answer. If the answer is “yes,” you reach your goal; if
            the answer is “no,” you can ask the next time whether the
            spy is at the same or another location of your choice.")
        (:div "Devise an algorithm that will find the spy after a
            finite number questions.")
        (:div (fmt new-game-message))
        (:div (form-find s game-id)))))))

(define-easy-handler (find-page :uri "/locate")
    ((location :parameter-type 'integer)
     (game-id :real-name "gameid" :parameter-type 'string))
  (setf (hunchentoot:content-type*) "text/html")
  (let ((spy (gethash game-id *spies*)))
    (with-html-output-to-string (s)
      (if (null spy)
          (htm (:div "Could not find game-id " (fmt game-id)))
          (let* ((spy-location (spy-location spy))
                 (time (spy-time spy))
                 (new-time (clock-tick spy))
                 (given-location location)
                 (spy-found-p (= spy-location given-location))
                 (spy-found (if (= spy-location given-location) "true" "false")))
            (format t "game=~a ~a location=~a guess=~a~%"
                    game-id
                    (format nil "#spy(:a ~d :b ~d :time ~d)"
                            (spy-a spy) (spy-b spy) (spy-time spy))
                    spy-location
                    given-location)
            (setf (hunchentoot:header-out "x-spy-game-id") game-id
                  (hunchentoot:header-out "x-spy-time") new-time
                  (hunchentoot:header-out "x-spy-found") spy-found)
            (htm (:div (fmt "Time = ~d" time)))
            (if spy-found-p
                (progn
                  (htm (:div (fmt "Congrats, you found the spy.")))
                  (remhash game-id *spies*))
                (htm (:div (form-find s game-id location)))))))))

(define-easy-handler (exit :uri "/exit") ()
  (hunchentoot:stop *root-acceptor*))

(hunchentoot:start *root-acceptor*)

;;; thief.lisp ends here
