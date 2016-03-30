;;; swap-regions.el --- Swap current region and previous region  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/swap-regions.el
;; Keywords: convenience
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable, use:
;;
;; (require 'swap-regions)
;;
;; To exchange current (active) region and previous region, call M-x
;; swap-regions, if you think it is worth, you can assign a key to it,
;; use e.g.:
;;
;; (define-key global-map "\C-c\C-t" #'swap-regions)
;;
;; Tips: these two region don't need to belong to the same buffer.

;; TODO: Replace this-region with last-region

;;; Code:

(defvar swap-regions-last-region nil)
(defvar swap-regions-this-region nil)

(defun swap-regions-track-region ()
  (setq swap-regions-last-region
        (cons (current-buffer)
              (cons (region-beginning) (region-end)))))

(add-hook 'deactivate-mark-hook #'swap-regions-track-region)

;;;###autoload
(defun swap-regions (beg end)
  "Exchange the region and last region."
  (interactive "*r")
  (unless swap-regions-last-region
    (user-error "Nedd previous region"))
  (if (region-active-p)
      (setq swap-regions-this-region
            (cons (current-buffer)
                  (cons beg end)))
    (user-error "Need active region"))
  (let ((last-buf (car swap-regions-last-region))
        (last-pos (cdr swap-regions-last-region))
        (this-buf (car swap-regions-this-region))
        (this-pos (cdr swap-regions-this-region)))
    (if (eq last-buf this-buf)
        (transpose-subr-1 last-pos this-pos)
      (let ((text1 (with-current-buffer last-buf
                     (buffer-substring (car last-pos) (cdr last-pos))))
            (text2 (with-current-buffer this-buf
                     (buffer-substring (car this-pos) (cdr this-pos)))))
        (with-current-buffer last-buf
          (delete-region (car last-pos) (cdr last-pos))
          (insert text2))
        (with-current-buffer this-buf
          (delete-region (car this-pos) (cdr this-pos))
          (insert text1))))))

(provide 'swap-regions)
;;; swap-regions.el ends here
