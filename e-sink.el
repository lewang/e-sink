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
;; Last-Updated: Thu Dec 22 06:55:46 2011 (+0800)
;;           By: Le Wang
;;     Update #: 64
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

(defgroup e-sink nil
  "work with e-sink.pl to receive STDOUT piped data"
  :group 'server)

(defvar e-sink-data-alist nil
  "")
(make-variable-buffer-local 'e-sink-data-alist)
(put 'e-sink-data-alist 'permanent-local t)

(defvar s-sink-startup-time 0.2
  "delay before the very first poll")

(defvar s-sink-refresh-rate 2
  "polling interval of temp file")

(defun e-sink-buffer-name-transform (name)
  "transform name into a suitable name for a buffer"
  (if (zerop (length name))
      "*e-sink*"
    (format "*%s*" name)))

;;;###autoload
(defun e-sink-start (name &optional temp-file)
  "start a new sink"
  (setq name (e-sink-buffer-name-transform name))
  (pop-to-buffer (get-buffer-create name))
  (with-current-buffer name
    (if (cdr (assq :e-sink-in-progress e-sink-data-alist))
        (error "Buffer '%s' has an active e-sink session.  Choose another." name)
      (push (cons :e-sink-in-progress t) e-sink-data-alist))
    (goto-char (point-max))
    (push (cons :start-time (current-time)) e-sink-data-alist)
    (if temp-file
        (progn
          (setq temp-file (if temp-file (make-temp-file "e-sink")))
          (push (cons :temp-file temp-file) e-sink-data-alist)
          (push (cons :temp-file-pos 0) e-sink-data-alist)
          (push (cons :timer (run-at-time s-sink-startup-time s-sink-refresh-rate
					  'e-sink-insert-from-temp name))
		e-sink-data-alist)
          temp-file)
      "e-sink session started")))

(defun e-sink-receive (name data)
  "receive some data"
  (setq name (e-sink-buffer-name-transform name))
  (with-current-buffer name
    (unless (cdr (assq :e-sink-in-progress e-sink-data-alist))
      (error "Buffer '%s' doesn't have an active e-sink session"))
    (save-excursion
      (goto-char (point-max))
      (insert data)))
  (format "received %i characters." (length data)))

(defun e-sink-insert-from-temp (transformed-buffer-name)
  "read some data from temp-file"
  (with-current-buffer transformed-buffer-name
    (unless (cdr (assq :e-sink-in-progress e-sink-data-alist))
      (error "Buffer '%s' doesn't have an active e-sink session"))
    (let ((pos-cons (assq :temp-file-pos e-sink-data-alist))
          (timer-cons (assq :timer e-sink-data-alist)))
      (save-excursion
	(goto-char (point-max))
	(setcdr pos-cons
		(+
		 (cdr pos-cons)
		 (cadr
		  (insert-file-contents (cdr (assq :temp-file e-sink-data-alist))
					nil
					(cdr pos-cons) nil))))))))

(defun e-sink-finish (name &optional signal)
  "finish e-sink session."
  (setq name (e-sink-buffer-name-transform name))
  (with-current-buffer name
    (let ((timer-cons (assq :timer e-sink-data-alist)))
      (when timer-cons
        (cancel-timer (cdr timer-cons))
        (e-sink-insert-from-temp name))
      (push (cons :e-sink-in-progress nil) e-sink-data-alist))))

(provide 'e-sink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e-sink.el ends heree
