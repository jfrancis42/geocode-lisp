(ql:quickload :geocode)
(ql:quickload :creds)

(creds:load-creds)

(defun what-is-the-lat-lon-of-this-address? ()
  "Return an object representing the location of the White House."
  (geocode:pp (geocode:street-address-to-lat-lon "1600 Pennsyvania Ave NW, Washington DC" (creds:get-cred "googlegeoapi"))))

(defun what-is-the-address-of-this-lat-lon? ()
  "Return the address of 38.89761,-77.036736."
  (geocode:pp (geocode:lat-lon-to-street-address 38.89761 -77.036736 (creds:get-cred "googlegeoapi"))))
