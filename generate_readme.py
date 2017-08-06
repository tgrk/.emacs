#!/usr/bin/python

import os

if __name__ == "__main__":
    base_dir = os.environ['HOME'] + "/.emacs.d/"

    # first list elpa package subdirs
    elpa_dir = base_dir + "/elpa/"
    for dir in os.walk(emacs_dir):
        print dir

    # secondly list custom packages
    lisp_dir = base_dir + "/lisp/"
