;;;; geocode.lisp

(in-package #:geocode)

(defconstant point-geocode 4)

;;  Decendant of the aviation-formulary's 2d-point.  Adds an address
;;  field for Google's geocoding service.
(defclass geocode-point (af:2d-point)
  ((address :accessor point-address
	    :initarg :address
	    :initform nil)))

(defmethod point-serialize ((p geocode-point))
  "Serialize a GEOCODE point."
  (append
   (list
    '(type geocode-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p))
    (list 'address (point-address p))
    )
   (af:point-metadata-serialize p)))

(defmethod pp ((p geocode-point))
  "Pretty print a geocode point."
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (point-creation-time p)))
  (format t "Age (sec): ~A~%" (- (local-time:timestamp-to-unix (local-time:now)) (point-creation-time p)))
  (format t "Name:  ~A~%" (point-name p))
  (format t "Descr:  ~A~%" (point-description p))
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Address: ~A~%" (point-address p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p geocode-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (point-metadata-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
	       ((equal (first n) 'lat)
		(setf (point-lat p) (second n)))
	       ((equal (first n) 'lon)
		(setf (point-lon p) (second n)))
	       ((equal (first n) 'address)
		(setf (point-address p) (second n)))
	       ((equal (first n) 'datum)
		(setf (point-datum p) (second n)))
	       ))
	  point-data))

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
	(json:decode-json-from-string (babel:octets-to-string 
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
	(json:decode-json-from-string (babel:octets-to-string 
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
    (make-instance 'geocode-point
		   :address (extract-street-address-from-json json)
		   :lat (cdr (assoc :lat ll))
		   :lon (cdr (assoc :lng ll)))))

(defun lat-lon-to-location (lat lon google-api-key)
  "Convert an arbitrary lat/lon into a location."
  (lookup-location (make-instance 'geocode-point :lat lat :lon lon) google-api-key))

(defun lat-lon-to-street-address (lat lon google-api-key)
  "Convert an arbitrary lat/lon into a street address."
  (let* ((json (lat-lon-to-location lat lon google-api-key))
	 (ll (extract-lat-lon-from-json json)))
    (make-instance 'geocode-point
		   :address (extract-street-address-from-json json)
		   :lat (cdr (assoc :lat ll))
		   :lon (cdr (assoc :lng ll)))))

(defun location-to-street-address (loc google-api-key)
  "Use lat/lon from a location object to create a new fully-populated
location object."
  (let* ((json (lat-lon-to-location (point-lat loc) (point-lon loc) google-api-key))
	 (ll (extract-lat-lon-from-json json)))
    (make-instance 'geocode-point
		   :address (extract-street-address-from-json json)
		   :lat (cdr (assoc :lat ll))
		   :lon (cdr (assoc :lng ll)))))

(defun google-url-hybrid (p)
  "Print a google url for this point.."
  (format nil "http://maps.google.com/maps?ll=~F,~F&spn=0.006362199783325195,0.009344816207885742&t=h&hl=en"
	  (point-lat p) (point-lon p)))

(defun google-url-photo (p)
  "Print a google url for this point.."
  (format nil "http://maps.google.com/maps?ll=~F,~F&spn=0.006362199783325195,0.009344816207885742&t=k&hl=en"
	  (point-lat p) (point-lon p)))

(defun google-url-map (p)
  "Print a google url for this point.."
  (format nil "http://maps.google.com/maps?spn=~F,~F&hl=en"
	  (point-lat p) (point-lon p)))

