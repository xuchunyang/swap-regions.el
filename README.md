# swap-regions.el

## Introduction

This package provides a command `swap-regions`, which exchanges current region
and previous region. When called with a prefix argument, replaces current
region with previous region.

These two regions don't need to belong to the same buffer.

## Screenshot

![swap-regions.gif](image/swap-regions.gif)

## Setup

If you install this package with Emacs's packaging system, no setup is
required.

(Optional) To bind the command `swap-regions` globally, use e.g.:

    (global-set-key "\C-c\C-t" #'swap-regions)
