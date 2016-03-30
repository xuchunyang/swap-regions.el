# swap-regions.el

This package provides a command `swap-regions`, which exchanges current region
and previous region. When called with a prefix argument, replaces current
region with previous region.

These two region don't need to belong to the same buffer.

## Setup

To enable, use:

    (require 'swap-regions)

(Optional) To bind the command `swap-regions` globally, use e.g.:

    (define-key global-map "\C-c\C-t" #'swap-regions)
