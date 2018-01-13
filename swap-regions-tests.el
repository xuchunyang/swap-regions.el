;; swap-regions-tests.el --- Tests for swap-regions.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)

(require 'swap-regions)

(ert-deftest swap-regions ()
  (with-temp-buffer
    (insert "Hello, World!")
    (swap-regions (current-buffer) 1 6
                  (current-buffer) 8 13)
    (should (string= "World, Hello!" (buffer-string))))

  (let (buf-A buf-B)
    (with-temp-buffer
      (insert "Hello, World!")
      (setq buf-A (current-buffer))
      (with-temp-buffer
        (insert "Hello, World!")
        (setq buf-B (current-buffer))
        (swap-regions buf-A 1 6
                      buf-B 8 13)
        (should (string= "Hello, Hello!" (buffer-string))))
      (should (string= "World, World!" (buffer-string))))))


(provide 'swap-regions-tests)
;;; swap-regions-tests.el ends here

;; Local Variables:
;; fill-column: #x50
;; indent-tabs-mode: nil
;; End:
