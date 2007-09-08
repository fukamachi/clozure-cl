(in-package "CCL")

;;; define the classes referenced in the nibfile

(defclass converter (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/convertCurrency:atRate: :float) ((self converter) (currency :float) (rate :float))
  (* currency rate))

(defclass converter-controller (ns:ns-object)
  ((amount-field :foreign-type :id :accessor amount-field)
   (converter :foreign-type :id :accessor converter)
   (dollar-field :foreign-type :id :accessor dollar-field)
   (rate-field :foreign-type :id :accessor rate-field))
  (:metaclass ns:+ns-object))

(objc:defmethod #/convert: ((self converter-controller) sender)
  (let* ((conv (converter self))
         (dollar-field (dollar-field self))
         (rate-field (rate-field self))
         (amount-field (amount-field self))
         (dollars (#/floatValue dollar-field))
         (rate (#/floatValue rate-field))
         (amount (#/convertCurrency:atRate: conv dollars rate)))
    ;;(#/setFloatValue: amount-field (* dollars rate))
    (#/setFloatValue: amount-field amount)
    (#/selectText: rate-field self)))

;;; load the nibfile

(defun load-nibfile (nib-path)
  (let* ((appclass (#_NSClassFromString (%make-nsstring "NSApplication")))
	 (app (#/sharedApplication appclass))
	 (main-nib-name (%make-nsstring (namestring nib-path))))
	 ;; ----------------------------------------
	 ;; load the application nib
	 (#/loadNibNamed:owner: (@class ns-bundle)
		   main-nib-name
		   app)
	 app))

#|
"/Users/mikel/Valise/clozure/openmcl/example-code/currency-converter/CurrencyConverter.nib"

building the app:

(progn
  (setf (current-directory) "/Users/mikel/Valise/clozure/openmcl/example-code/currency-converter/")
  (load "currency-converter")
  (require "build-application")
  (ccl::build-application :name "CurrencyConverter"
                          :main-nib-name "CurrencyConverter"
                          :nibfiles '(#P"/Users/mikel/Valise/clozure/openmcl/example-code/currency-converter/CurrencyConverter.nib")))

TODO NOTES:

The name of the app in the main menu title is determined by the CFBundleName field in the
InfoPlist.strings file in the English.lproj resources folder. The HOWTO should tell readers
to change this in order to make the app have the right name in its main menu.

|#