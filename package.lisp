;;;; package.lisp

(defpackage #:geocode
  (:use #:cl)
  (:import-from :aviation-formulary
		:pp
		:geocode-point
		:2d-point
		:point-lat
		:point-lon
		:address
		:point-creation-time
		:point-name
		:point-description
		:point-datum
		:lat
		:lon)
  (:export :pp
	   :google-url-hybrid
	   :google-url-photo
	   :google-url-map
	   :point-lat
	   :point-lon
	   :geocode-point
	   :address
	   :lookup-location
	   :lookup-place
	   :extract-street-address-from-json
	   :street-address-to-lat-lon
	   :location-to-street-address
	   :lat-lon-to-location
	   :lat-lon-to-street-address))
