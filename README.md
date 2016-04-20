# swap-regions.el [![MELPA](http://melpa.org/packages/swap-regions-badge.svg)](http://melpa.org/#/swap-regions)

## Introduction

This package provides a `swap-regions` command to swap (and replace) two regions
of text.

## Screenshot

![swap-regions.gif](image/swap-regions.gif)

## Setup

Before you can use the `swap-regions` command, you need to enable
`swap-regions-mode`. Type M-x swap-regions-mode or adding:

    (swap-regions-mode)

to your init file.

## Usage

### M-x swap-regions

Swap the current (active) region and the previous region

### C-u M-x swap-regions

Replace the current (active) region with the previous region

### C-u C-u M-x swap-regions

Replace the previous region with the current (active) region

## Key Binding

If you use the command a lot, you should bind some key to it.  Here is my
binding, please modify the key to your own liking:

    (global-set-key "\C-c\C-t" #'swap-regions)

## Known issues

- This package doesn't work with Evil and expand-region. However, fixing them is
  beyond my capabilities. Besides, considering I almost don't use these two
  packages, so I also don't have motivation for fixing it. If you find this
  package useful to you (really?) and want Evil and/or expand-region support,
  you have to fix it yourself, then send me a PR if you like.
