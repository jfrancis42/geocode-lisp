;;;; geocode.asd

(asdf:defsystem #:geocode
  :description "A Common Lisp client for doing geocoding and reverse geocoding using the Google Maps API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
	       #:babel
	       #:aviation-formulary
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "geocode")))

