;;; swap-regions.el --- Swap two regions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Chunyang Xu

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/swap-regions.el
;; Package-Requires: ((emacs "24"))

;; swap-regions.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; swap-regions.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(defun swap-regions (buf-A reg-A-beg reg-A-end buf-B reg-B-beg reg-B-end)
  "Swap contents in two regions."
  (interactive
   (let ((hint
          (substitute-command-keys
           "Finish `\\[exit-recursive-edit]', abort \
`\\[abort-recursive-edit]'"))
         buf-A-overlay)
     ;; Select the first region
     (unless (use-region-p)
       (let (message-log-max)
         (message "Select the first region (%s)" hint))
       (recursive-edit))
     (setq buf-A (current-buffer)
           reg-A-beg (region-beginning)
           reg-A-end (region-end))
     (deactivate-mark)
     (setq buf-A-overlay (make-overlay reg-A-beg reg-A-end))
     ;; TODO: Make the face customizable
     (overlay-put buf-A-overlay 'face 'region)
     ;; Select the second region
     (let (message-log-max)
       (message "Select the second region (%s)" hint))
     (unwind-protect (recursive-edit)
       (delete-overlay buf-A-overlay))
     (setq buf-B (current-buffer)
           reg-B-beg (region-beginning)
           reg-B-end (region-end))
     (deactivate-mark)
     (list buf-A reg-A-beg reg-A-end buf-B reg-B-beg reg-B-end)))
  ;; Swap these two regions
  (let ((reg-A-str (buffer-substring reg-A-beg reg-A-end))
        (reg-B-str (buffer-substring reg-B-beg reg-B-end)))
    (when (< reg-B-beg reg-A-beg)
      (cl-psetq buf-A buf-B
                reg-A-beg reg-B-beg
                reg-A-end reg-B-end
                reg-A-str reg-B-str
                buf-B buf-A
                reg-B-beg reg-A-beg
                reg-B-end reg-A-end
                reg-B-str reg-A-str))
    (with-current-buffer buf-B
      (delete-region reg-B-beg reg-B-end)
      (goto-char reg-B-beg)
      (insert reg-A-str))
    (with-current-buffer buf-A
      (delete-region reg-A-beg reg-A-end)
      (goto-char reg-A-beg)
      (insert reg-B-str))))

;; For backwards compatibility
(defun swap-regions-mode (&rest _ignore))

(provide 'swap-regions)
;;; swap-regions.el ends here

;; Local Variables:
;; fill-column: #x50
;; indent-tabs-mode: nil
;; End:
