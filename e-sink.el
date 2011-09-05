;;; e-sink.el --- pipe output of any command to Emacs buffer

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: see top.
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Mon Sep  5 00:01:13 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Tue Sep  6 05:39:20 2011 (+0800)
;;           By: Le Wang
;;     Update #: 25
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

;;
;; The following applies to the "--cmd" option, which was the default at the time.
;;
;;   First of all this thing is slow.  So don't use it if you can help it.  Pipe
;;   to a file and use Emacs to view that file instead.
;;
;;   I have made a couple of optimizations to hopefully make it slightly faster:
;;
;;    1. `e-sink.pl' tries to minimize invocations of "emacsclient" by
;;       maximizing the command-line buffer.
;;
;;    2. It also does non-blocking read from the source.  So the data source
;;       doesn't hang there waiting for emacsclient invocations.
;;

;;; Usage:

;;
;; For example to pipe diff output to a new buffer named "*| <diff>*"
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

(defgroup e-sink nil
  "work with e-sink.pl to receive STDOUT piped data"
  :group 'server)

(defface e-sink-marker-face
  '((t (:inherit font-lock-warning-face)))
  "face to display chevron markers"
  :group 'e-sink)


(defvar e-sink-started-alist nil
  "")

(defun e-sink-float-duration-to-parts (time)
  "convert `time' in float seconds to a list (days hours minutes seconds_float)"
  (let* ((time-i (truncate time))
         (frac (- time time-i))
         (day-seconds (* 60 60 24))
         (hour-seconds (* 60 60))
         (minute-seconds 60))
    (append (mapcar '(lambda (factor)
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
(defun e-sink-start (name)
  "start a new sink"
  (setq name (e-sink-buffer-name-transform name))
  (pop-to-buffer (get-buffer-create name))
  (set-buffer name)
  (unless (bolp) (insert "\n"))
  (insert
   (propertize "<<<<<" 'face 'e-sink-marker-face)
   " start: "
   (format-time-string "%Y-%m-%dT%H:%M:%S")
   "\n")
  (setq e-sink-started-alist (cons (cons name (current-time)) e-sink-started-alist))
  "e-sink session started")

(defun e-sink-receive (name data)
  "receive some data"
  (setq name (e-sink-buffer-name-transform name))
  (with-current-buffer name
    (goto-char (point-max))
    (insert data))
  (format "received %i characters." (length data)))

(defun e-sink-finish (name &optional signal)
  ""
  (setq name (e-sink-buffer-name-transform name))
  (with-current-buffer name
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert
     (propertize "<<<<<" 'face 'e-sink-marker-face)
     "   end: "
     (let* ((start-time (cdr (assoc name e-sink-started-alist)))
            (duration (time-subtract (current-time) start-time))
            (parts (e-sink-float-duration-to-parts (float-time duration)))
            (str (e-sink-format-duration parts)))
       str)
     (if signal
         (concat " "
                 (propertize (format "{%s}" signal) 'face 'e-sink-marker-face)
                 " caught")
       "")
     "\n\n"))
  "e-sink session finished.")


(defun e-sink-insert-and-finish (name temp-file &optional signal)
  ""
  (let ((buffer-name (e-sink-buffer-name-transform name)))
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (insert-file temp-file)
      (e-sink-finish name signal))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e-sink.el ends heree
