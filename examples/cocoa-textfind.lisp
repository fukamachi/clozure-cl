(in-package "CCL)

;;; This is stolen (rather shamelessly) from the TextFinder class in
;;; Apple's TextEdit example.

(require "COCOA-WINDOW")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* *objc-readtable*))

(def-objc-class "TextFinder" "NSObject"
  ((findstring "findString") (* (:struct :<NSS>tring)))
  ((findtextfield "findTextField") :id)
  ((replacetextfield "replaceTextField") :id)
  ((ignorecasebutton "ignoreCaseButton") :id)
  ((findnextbutton "findNextButton") :id)
  ((statusfield "statusField") :id)
  ((lastfindwassuccessful "lastFindWasSuccessful") :<BOOL>))

  