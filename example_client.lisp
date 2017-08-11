(ql:quickload :geocode)
(ql:quickload :creds)

(creds:load-creds)

(defun what-is-the-lat-lon-of-the-whitehouse? ()
  "Return an object representing the location of the White House."
  (let ((wh (geocode:street-address-to-lat-lon "1600 Pennsyvania Ave NW, Washington DC" (creds:get-cred "googlegeoapi"))))
    (list (geocode:point-lat wh) (geocode:point-lon wh))))

(defun what-is-the-address-of-the-whitehouse? ()
  "Return the address of 38.89761,-77.036736."
  (geocode:point-address (geocode:lat-lon-to-street-address 38.89761 -77.036736 (creds:get-cred "googlegeoapi"))))
