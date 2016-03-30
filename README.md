# swap-regions.el

## Introduction

This package provides a command `swap-regions`, which exchanges current region
and previous region. When called with a prefix argument, replaces current
region with previous region.

These two regions don't need to belong to the same buffer.

## Screenshot

![swap-regions.gif](image/swap-regions.gif)

## Setup

To enable, use:

    (require 'swap-regions)

(Optional) To bind the command `swap-regions` globally, use e.g.:

    (define-key global-map "\C-c\C-t" #'swap-regions)
