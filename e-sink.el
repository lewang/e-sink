;;; e-sink.el --- pipe output of any command to Emacs buffer

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: see top.
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Mon Sep  5 00:01:13 2011 (+0800)
;; Version: 0.2
;; Last-Updated: Mon Apr  1 20:28:42 2013 (+0800)
;;           By: Le Wang
;;     Update #: 68
;; URL: https://github.com/lewang/e-sink
;; Keywords: server shell-integration
;; Compatibility: emacs 23+

;;; Installation:

;; add to your .emacs:
;;
;;   (server-start)
;;   (require 'e-sink)
;;
;; link e-sink.pl to your local bin directory:
;;
;;   $ ln -s e-sink.pl /usr/local/bin
;;

;;; Commentary:

;;; Usage:

;;
;; Pipe diff output to a new buffer named "*| <diff>*"
;;
;;   $ diff old.txt new.txt | e-sink.pl diff
;;
;; For a slower operation, but not needing a temporary file:
;;
;;   $ diff old.txt new.txt | e-sink.pl --cmd diff
;;
;; To "tee" the output:
;;
;;   $ diff old.txt new.txt | e-sink.pl --tee diff
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(provide 'e-sink)

(require 'tty-format)

(defgroup e-sink nil
  "work with e-sink.pl to receive STDOUT piped data"
  :group 'server)

(defface e-sink-marker-face
  '((t (:inherit font-lock-warning-face)))
  "face to display chevron markers"
  :group 'e-sink)


(defvar e-sink-data-alist nil
  "")
(make-variable-buffer-local 'e-sink-data-alist)
(put 'e-sink-data-alist 'permanent-local t)

(defvar s-sink-refresh-rate 2
  "polling interval of temp file")

(defun e-sink-float-duration-to-parts (time)
  "convert `time' in float seconds to a list (days hours minutes seconds_float)"
  (let* ((time-i (truncate time))
         (frac (- time time-i))
         (day-seconds (* 60 60 24))
         (hour-seconds (* 60 60))
         (minute-seconds 60))
    (append (mapcar (lambda (factor)
                       (prog1 (/ time-i factor)
                         (setq time-i (% time-i factor))))
                    (list day-seconds hour-seconds minute-seconds))
            (list (+ time-i frac)))))

(defun e-sink-format-duration (time-parts)
  "convert a float-time parts list into a string"
  (let ((part-names '("d" "h" "m" "s"))
        (zero-so-far t))
    (mapconcat (lambda (part-name)
                 (prog1
                     (if (and (zerop (car time-parts))
                              zero-so-far)
                         ""
                       (setq zero-so-far nil)
                       (format (if (floatp (car time-parts)) "%.3f%s" "%i%s")
                               (car time-parts)
                               part-name))
                   (setq time-parts (cdr time-parts))))
               part-names
               "")))

(defun e-sink-buffer-name-transform (name)
  ""
  (if (zerop (length name))
      "*| <e-sink>*"
    (format "*| %s*" name)))

;;;###autoload
(defun e-sink-start (name &optional temp-file)
  "start a new sink"
  (setq name (e-sink-buffer-name-transform name))
  (pop-to-buffer (get-buffer-create name))
  (with-current-buffer name
    (if (= (point-min) 1)
        (font-lock-mode 1))
    (if (cdr (assq :e-sink-in-progress e-sink-data-alist))
        (error "Buffer '%s' has an active e-sink session.  Choose another." name)
      (push (cons :e-sink-in-progress t) e-sink-data-alist))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert
     (propertize "<<<<<" 'face 'e-sink-marker-face)
     " start: "
     (format-time-string "%Y-%m-%dT%H:%M:%S")
     "\n")
    (push (cons :start-time (current-time)) e-sink-data-alist)
    (push (cons :start-pos (point)) e-sink-data-alist)
    (if temp-file
        (progn
          (setq temp-file (if temp-file (make-temp-file "e-sink")))
          (push (cons :temp-file temp-file) e-sink-data-alist)
          (push (cons :temp-file-pos 0) e-sink-data-alist)
          ;; if we use the intervaled timers, then we could get race
          ;; conditions and all kinds of weirdness.
          ;;
          ;; So: just scheduke one and schedule the next one after it's done.
          (push (cons :timer (run-at-time s-sink-refresh-rate nil 'e-sink-insert-from-temp name)) e-sink-data-alist)
          temp-file)
      "e-sink session started")))

(defun e-sink-receive (name data)
  "receive some data"
  (setq name (e-sink-buffer-name-transform name))
  (with-current-buffer name
    (unless (cdr (assq :e-sink-in-progress e-sink-data-alist))
      (error "Buffer '%s' doesn't have an active e-sink session"))
    (goto-char (point-max))
    (insert data))
  (format "received %i characters." (length data)))

(defun e-sink-insert-from-temp (transformed-buffer-name &optional no-reschedule)
  "read some data from temp-file"
  (with-current-buffer transformed-buffer-name
    (unless (cdr (assq :e-sink-in-progress e-sink-data-alist))
      (error "Buffer '%s' doesn't have an active e-sink session"))
    (let ((pos-cons (assq :temp-file-pos e-sink-data-alist))
          (timer-cons (assq :timer e-sink-data-alist)))
      (goto-char (point-max))
      (setcdr pos-cons
              (+
               (cdr pos-cons)
               (cadr (insert-file-contents (cdr (assq :temp-file e-sink-data-alist)) nil (cdr pos-cons) nil))))
      (goto-char (point-max))
      (setcdr timer-cons (if no-reschedule
                             nil
                           (run-at-time s-sink-refresh-rate
                                        nil
                                        'e-sink-insert-from-temp
                                        transformed-buffer-name))))))

(defun e-sink-finish (name &optional signal)
  "finish e-sink session."
  (setq name (e-sink-buffer-name-transform name))
  (with-current-buffer name
    (let ((timer-cons (assq :timer e-sink-data-alist)))
      (when timer-cons
        (cancel-timer (cdr timer-cons))
        (e-sink-insert-from-temp name 'no-reschedule))
      (unless (bolp) (insert "\n"))
      (let* ((start (cdr (assq :start-pos e-sink-data-alist)))
             (finish (progn
                       (ansi-format-decode start (point))
                       (point)))))
      (push (cons :finish-pos (point)) e-sink-data-alist)
      (insert
       (propertize "<<<<<" 'face 'e-sink-marker-face)
       "   end: "
       (let* ((start-time (cdr (assq :start-time e-sink-data-alist)))
              (duration (time-subtract (current-time) start-time))
              (parts (e-sink-float-duration-to-parts (float-time duration)))
              (str (e-sink-format-duration parts)))
         str)
       (if signal
           (concat " "
                   (propertize (format "{SIG%s}" signal) 'face 'e-sink-marker-face))
         "")
       "\n\n")
      (push (cons :e-sink-in-progress nil) e-sink-data-alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e-sink.el ends heree
