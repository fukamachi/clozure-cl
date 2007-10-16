(cl:defpackage :easygui
  (:use :cl)
  (:import-from :ccl with-autorelease-pool @selector lisp-string-from-nsstring +null-ptr+)
  (:export #:point #:range #:rectangle #:window
           #:point-x #:point-y #:rectangle-x #:rectangle-y #:rectangle-width
           #:rectangle-height
           ;; view classes
           #:view #:static-text-view #:text-input-view #:password-input-view
           #:push-button-view
           #:form-view #:form-cell-view #:box-view #:drawing-view
           ;; operators
           #:cocoa-ref
           #:add-subviews #:window-show #:set-window-title
           #:content-view
           #:initialize-view #:action #:view-text
           #:add-entry #:add-entries #:editable-p
           #:draw-view-rectangle
           #:entry-text #:nth-cell #:selection))

(cl:defpackage :easygui-user
  (:use :cl :easygui))