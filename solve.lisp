;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SOLVE-SPY; Base: 10 -*-

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

(ql:quickload :drakma)

(defpackage solve-spy
  (:use :common-lisp :drakma))

(in-package :solve-spy)

(defun pairs (n)
  (loop :as i :from 0 :to n
     :collect (cons i n)
     :if (/= i n) :collect (cons n i)))

(defun positive-negative-pairs (n)
  (if (zerop n)
      `((0 . 0))
      (loop :as (a . b) :in (pairs n)
         :collect                           (cons    a     b)
         :if (> b 0) :collect               (cons    a  (- b))
         :if (> a 0) :collect               (cons (- a)    b)
         :if (and (> a 0) (> b 0)) :collect (cons (- a) (- b)))))

(defstruct game
  (id "" :type string :read-only 't)
  (finished-p 'nil)
  (time 0 :type fixnum)
  (a nil)
  (b nil))

(defun start-game ()
  (multiple-value-bind (strm1 code headers uri strm2 closep status)
      (http-request "http://localhost:4242/")
    (declare (ignore strm1 code uri strm2 closep status))
    (let ((game-id (cdr (assoc :x-spy-game-id headers))))
      (make-game :id game-id :time 0))))

(defun guess (game a b)
  (let* ((location (+ a (* b (game-time game))))
         (url (format nil "http://localhost:4242/locate?gameid=~a&location=~d"
                      (game-id game) location)))
    (multiple-value-bind (strm1 code headers uri strm2 closep status)
        (http-request url)
      (declare (ignore strm1 code uri strm2 closep status))
      (flet ((header (key) (cdr (assoc key headers))))
        (setf (game-time game) (parse-integer (header :x-spy-time))
              (game-finished-p game) (string-equal (header :x-spy-found) "true")
              (game-a game) a
              (game-b game) b)))))

(defun play ()
  (loop :with game := (start-game)
     :as i :from 0
     :do (loop :as (a . b) :in (positive-negative-pairs i)
            :do (guess game a b)
            :if (game-finished-p game) :return game)
     :if (game-finished-p game) :return game
     :finally (return game)))

(let ((game (play)))
  (format t "Found the spy at time ~d a=~d b=~d~%"
          (game-time game) (game-a game) (game-b game)))

;;; solve.lisp ends here
