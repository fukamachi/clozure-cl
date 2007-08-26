;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          build-application.lisp
;;;; Version:       0.9
;;;; Project:       Cocoa application builder
;;;; Purpose:       the in-process application builder
;;;;
;;;; ***********************************************************************

(require "builder-utilities")

(in-package :ccl)

(defun build-application (&key
                          (name "MyApplication")
                          (type-string "APPL")
                          (creator-string "OMCL")
                          (directory (current-directory))
                          (application-class 'cocoa-application)
                          (toplevel-function nil)
                          (swank-loader nil)
                          (autostart-swank-on-port nil))
  ;;; if the path to swank-loader.lisp is given, then load
  ;;; swank before building the application
  (when swank-loader
    (assert (probe-file swank-loader)(swank-loader)
            "Swank loader not found at path '~A'" swank-loader)
    (load swank-loader)
    ;; when autostart-swank-on-port is also given, setup
    ;; swank to start up on launch (still don't know how
    ;; we're actually going to do this)
    (when autostart-swank-on-port
      (assert (integerp autostart-swank-on-port)(autostart-swank-on-port)
              "The port for :autostart-swank-on-port must be an integer or nil, not '~S'"
              autostart-swank-on-port)
      ;; if we get this far, setup the swank autostart
      ;; (however we're going to do that...)
      ))
  ;;; build the application
  (let* ((ide-bundle (#/mainBundle ns:ns-bundle))
         (ide-bundle-path-nsstring (#/bundlePath ide-bundle))
         (ide-bundle-path (pathname 
                           (ensure-directory-pathname 
                            (lisp-string-from-nsstring ide-bundle-path-nsstring))))
         (app-bundle (make-application-bundle name type-string creator-string directory))
         (image-path (namestring (path app-bundle "Contents" "MacOS" name))))
    ;; copy IDE resources into the application bundle
    (recursive-copy-directory (path ide-bundle-path "Contents" "Resources/")
                              (path app-bundle  "Contents" "Resources/"))
    ;; save the application image
    (save-application image-path
                      :application-class application-class
                      :toplevel-function toplevel-function
                      :prepend-kernel t)))



