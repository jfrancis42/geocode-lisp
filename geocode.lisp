;;;; geocode.lisp

(in-package #:geocode)

(defun bytes-to-ascii (bytelist)
  "Turn a list of bytes into string."
  (map 'string #'code-char bytelist))

(defun lookup-place (place google-api-key)
    "Use the Google Geocoding API to do a  geocode lookup (ie,
address to lat lon)."
  (let* ((url (concatenate 'string "https://maps.googleapis.com/maps/api/geocode/json?address="
			   (drakma:url-encode place :utf-8)
			   "&key=" google-api-key))
	 (result (drakma:http-request url
				      :method :get
				      :accept "application/json"
				      :content-type "application/json")))
    (if (> (length result) 0)
	(json:decode-json-from-string (bytes-to-ascii 
				       (nth-value 0 result)))
	nil)))

(defun lookup-location (loc google-api-key)
  "Use the Google Geocoding API to do a reverse geocode lookup (ie,
convert lat lon to address)."
  (let* ((url (concatenate 'string "https://maps.googleapis.com/maps/api/geocode/json?latlng="
			   (format nil "~A" (point-lat loc)) ","
			   (format nil "~A" (point-lon loc))
			   "&key=" google-api-key))
	 (result (drakma:http-request url
				      :method :get
				      :accept "application/json"
				      :content-type "application/json")))
    (if (> (length result) 0)
	(json:decode-json-from-string (bytes-to-ascii 
				       (nth-value 0 result)))
	nil)))

(defun extract-street-address-from-json (geocoded-result)
  "Extract the formatted street address from (lookup-location)
result."
  (cdr (assoc :formatted--address (cadar geocoded-result))))

(defun extract-lat-lon-from-json (geocoded-result)
  "Extract the lat/lon from (lookup-location) result."
  (cdr (assoc :location (cdr (assoc :geometry (cadar geocoded-result))))))

(defun street-address-to-lat-lon (addr google-api-key)
  "Return an object representing the location of the given street
address."
  (let* ((json (lookup-place addr google-api-key))
	 (ll (extract-lat-lon-from-json json)))
    (make-instance '2d-point
		   :description (extract-street-address-from-json json)
		   :lat (cdr (assoc :lat ll))
		   :lon (cdr (assoc :lng ll)))))

(defun lat-lon-to-location (lat lon google-api-key)
  "Convert an arbitrary lat/lon into a location."
  (lookup-location (make-instance '2d-point :lat lat :lon lon) google-api-key))

(defun lat-lon-to-street-address (lat lon google-api-key)
  "Convert an arbitrary lat/lon into a street address."
  (let* ((json (lat-lon-to-location lat lon google-api-key))
	 (ll (extract-lat-lon-from-json json)))
    (make-instance '2d-point
		   :description (extract-street-address-from-json json)
		   :lat (cdr (assoc :lat ll))
		   :lon (cdr (assoc :lng ll)))))
