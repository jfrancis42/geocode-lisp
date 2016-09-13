;;;; geocode.asd

(asdf:defsystem #:geocode
  :description "Describe geocode here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
	       #:aviation-formulary
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "geocode")))

