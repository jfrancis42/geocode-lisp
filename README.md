# geocode-lisp
A Common Lisp client for doing geocoding and reverse geocoding using
the Google Maps API.

The Google Maps API provides for both geocoding and reverse geocoding (ie, looking up an address to determine it's latitude and longitude, and looking up a latitude and longitude to determine the address at that location). This library provides an interface to that API. Use of the Google Geocoding API is subject to their terms of services and requires an API key (obtained from their web page).

To use this library, start by loading it:

```
CL-USER> (ql:quickload :geocode)
To load "geocode":
  Load 1 ASDF system:
    geocode
; Loading "geocode"
[package geocode]
(:GEOCODE)
CL-USER>
```

This library uses the location objects found in the aviation-formulary package (and as a result, these object can also be used in any other formulas, such as for calculating bearing and distance, found in that library). Looking up an address returns a geocode-point object:

```
CL-USER> (geocode:street-address-to-lat-lon "1600 Pennsyvania Ave NW, Washington DC" "seekritsquirrel")
#<AVIATION-FORMULARY:GEOCODE-POINT {1011BBEF33}>
CL-USER>
```

This object contains a number of useful fields, in this case, we care about the lat and lon fields:

```
CL-USER> (describe (geocode:street-address-to-lat-lon "1600 Pennsyvania Ave NW, Washington DC" "seekritsquirrel"))
#<AVIATION-FORMULARY:GEOCODE-POINT {1011C8F403}>
  [standard-object]

Slots with :INSTANCE allocation:
  SERIAL-NUMBER    = 20
  CREATION-TIME    = 1473810252
  CREATION-SOURCE  = 0
  NAME             = ""
  DESCRIPTION      = ""
  LAT              = 38.89761
  LON              = -77.036736
  DATUM            = "WGS84"
  ADDRESS          = "The White House, 1600 Pennsylvania Ave NW, Washington, DC 20500, USA"
; No value
CL-USER>
```

Reverse geocoding does the exact opposite:

```
CL-USER> (describe (geocode:lat-lon-to-street-address 38.89761 -77.036736 "seekritsquirrel"))
#<AVIATION-FORMULARY:GEOCODE-POINT {1012044C63}>
  [standard-object]

Slots with :INSTANCE allocation:
  SERIAL-NUMBER    = 26
  CREATION-TIME    = 1473810330
  CREATION-SOURCE  = 0
  NAME             = ""
  DESCRIPTION      = ""
  LAT              = 38.89761
  LON              = -77.036736
  DATUM            = "WGS84"
  ADDRESS          = "The White House, 1600 Pennsylvania Ave NW, Washington, DC 20500, USA"
; No value
CL-USER> (geocode:address (geocode:lat-lon-to-street-address 38.89761 -77.036736 "seekritsquirrel"))
"The White House, 1600 Pennsylvania Ave NW, Washington, DC 20500, USA"
CL-USER>
```

That's pretty much all there is to it, though you can make a raw API call and get back the full JSON for your own use with (lookup-place ...) and (lookup-location ...) (note that lookup-location expect a geocode-point object as an argument):

```
CL-USER> (geocode:lookup-location (make-instance 'geocode:geocode-point :lat 38.89761 :lon -77.036736) "seekritsquirrel")
((:RESULTS
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "The White House") (:SHORT--NAME . "The White House")
     (:TYPES "premise"))
    ((:LONG--NAME . "1600") (:SHORT--NAME . "1600") (:TYPES "street_number"))
    ((:LONG--NAME . "Pennsylvania Avenue Northwest")
     (:SHORT--NAME . "Pennsylvania Ave NW") (:TYPES "route"))
    ((:LONG--NAME . "Northwest Washington")
     (:SHORT--NAME . "Northwest Washington")
     (:TYPES "neighborhood" "political"))
    ((:LONG--NAME . "Washington") (:SHORT--NAME . "Washington")
     (:TYPES "locality" "political"))
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political"))
    ((:LONG--NAME . "20500") (:SHORT--NAME . "20500") (:TYPES "postal_code")))
   (:FORMATTED--ADDRESS
    . "The White House, 1600 Pennsylvania Ave NW, Washington, DC 20500, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.897903) (:LNG . -77.035515))
     (:SOUTHWEST (:LAT . 38.897316) (:LNG . -77.03796)))
    (:LOCATION (:LAT . 38.89761) (:LNG . -77.036736))
    (:LOCATION--TYPE . "ROOFTOP")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.89896) (:LNG . -77.035385))
     (:SOUTHWEST (:LAT . 38.89626) (:LNG . -77.038086))))
   (:PLACE--ID . "ChIJGVtI4by3t4kRr51d_Qm_x58") (:TYPES "premise"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "West Executive Avenue Northwest")
     (:SHORT--NAME . "W Executive Ave NW") (:TYPES "route"))
    ((:LONG--NAME . "Northwest Washington")
     (:SHORT--NAME . "Northwest Washington")
     (:TYPES "neighborhood" "political"))
    ((:LONG--NAME . "Washington") (:SHORT--NAME . "Washington")
     (:TYPES "locality" "political"))
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political"))
    ((:LONG--NAME . "20500") (:SHORT--NAME . "20500") (:TYPES "postal_code")))
   (:FORMATTED--ADDRESS . "W Executive Ave NW, Washington, DC 20500, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.89872) (:LNG . -77.03799))
     (:SOUTHWEST (:LAT . 38.89641) (:LNG . -77.03799)))
    (:LOCATION (:LAT . 38.897564) (:LNG . -77.03799))
    (:LOCATION--TYPE . "GEOMETRIC_CENTER")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.898914) (:LNG . -77.036644))
     (:SOUTHWEST (:LAT . 38.896217) (:LNG . -77.03934))))
   (:PLACE--ID . "ChIJPWmSlby3t4kRBbzLK31n4RE") (:TYPES "route"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "Northwest Washington")
     (:SHORT--NAME . "Northwest Washington")
     (:TYPES "neighborhood" "political"))
    ((:LONG--NAME . "Washington") (:SHORT--NAME . "Washington")
     (:TYPES "locality" "political"))
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "Northwest Washington, Washington, DC, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.995556) (:LNG . -77.00606))
     (:SOUTHWEST (:LAT . 38.889194) (:LNG . -77.11975)))
    (:LOCATION (:LAT . 38.93809) (:LNG . -77.04493))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.995556) (:LNG . -77.00606))
     (:SOUTHWEST (:LAT . 38.889194) (:LNG . -77.11975))))
   (:PLACE--ID . "ChIJi7U4CaS3t4kR0m_KoPJ00Bw")
   (:TYPES "neighborhood" "political"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "Washington") (:SHORT--NAME . "Washington")
     (:TYPES "locality" "political"))
    ((:LONG--NAME . "District of Columbia")
     (:SHORT--NAME . "District of Columbia")
     (:TYPES "administrative_area_level_2" "political"))
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "Washington, DC, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.99555) (:LNG . -76.90939))
     (:SOUTHWEST (:LAT . 38.80315) (:LNG . -77.11974)))
    (:LOCATION (:LAT . 38.907192) (:LNG . -77.03687))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.99555) (:LNG . -76.90939))
     (:SOUTHWEST (:LAT . 38.80315) (:LNG . -77.11974))))
   (:PLACE--ID . "ChIJW-T2Wt7Gt4kRKl2I1CJFUsI")
   (:TYPES "locality" "political"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "20502") (:SHORT--NAME . "20502") (:TYPES "postal_code"))
    ((:LONG--NAME . "Northwest Washington")
     (:SHORT--NAME . "Northwest Washington")
     (:TYPES "neighborhood" "political"))
    ((:LONG--NAME . "Washington") (:SHORT--NAME . "Washington")
     (:TYPES "locality" "political"))
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "Washington, DC 20502, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.89876) (:LNG . -77.03387))
     (:SOUTHWEST (:LAT . 38.89261) (:LNG . -77.03848)))
    (:LOCATION (:LAT . 38.89676) (:LNG . -77.03702))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.89876) (:LNG . -77.03387))
     (:SOUTHWEST (:LAT . 38.89261) (:LNG . -77.03848))))
   (:PLACE--ID . "ChIJFfZlI6O3t4kRT21JVHxrwx4") (:TYPES "postal_code"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "20372") (:SHORT--NAME . "20372") (:TYPES "postal_code"))
    ((:LONG--NAME . "Washington") (:SHORT--NAME . "Washington")
     (:TYPES "locality" "political"))
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "Washington, DC 20372, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.910378) (:LNG . -77.008995))
     (:SOUTHWEST (:LAT . 38.882282) (:LNG . -77.057625)))
    (:LOCATION (:LAT . 38.899242) (:LNG . -77.03034))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.910378) (:LNG . -77.008995))
     (:SOUTHWEST (:LAT . 38.882282) (:LNG . -77.057625))))
   (:PLACE--ID . "ChIJGzCb9gq4t4kRN_0lpRhRhjU") (:TYPES "postal_code"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "District of Columbia") (:SHORT--NAME . "DC")
     (:TYPES "administrative_area_level_1" "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "District of Columbia, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 38.99555) (:LNG . -76.90939))
     (:SOUTHWEST (:LAT . 38.80315) (:LNG . -77.11974)))
    (:LOCATION (:LAT . 38.905987) (:LNG . -77.03342))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 38.99555) (:LNG . -76.90939))
     (:SOUTHWEST (:LAT . 38.80315) (:LNG . -77.11974))))
   (:PLACE--ID . "ChIJW-T2Wt7Gt4kRmKFUAsCO4tY")
   (:TYPES "administrative_area_level_1" "political"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "Washington-Arlington-Alexandria, DC-VA-MD-WV")
     (:SHORT--NAME . "Washington-Arlington-Alexandria, DC-VA-MD-WV")
     (:TYPES "political"))
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "Washington-Arlington-Alexandria, DC-VA-MD-WV, USA")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 39.720016) (:LNG . -76.38623))
     (:SOUTHWEST (:LAT . 37.99116) (:LNG . -78.38668)))
    (:LOCATION (:LAT . 39.12897) (:LNG . -77.37838))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 39.720024) (:LNG . -76.38623))
     (:SOUTHWEST (:LAT . 37.99116) (:LNG . -78.38668))))
   (:PLACE--ID . "ChIJy25QE7lFtokRgaAnbc9iMr8") (:TYPES "political"))
  ((:ADDRESS--COMPONENTS
    ((:LONG--NAME . "United States") (:SHORT--NAME . "US")
     (:TYPES "country" "political")))
   (:FORMATTED--ADDRESS . "United States")
   (:GEOMETRY
    (:BOUNDS (:NORTHEAST (:LAT . 71.38677) (:LNG . -66.95029))
     (:SOUTHWEST (:LAT . 18.910677) (:LNG . 172.44589)))
    (:LOCATION (:LAT . 37.09024) (:LNG . -95.71289))
    (:LOCATION--TYPE . "APPROXIMATE")
    (:VIEWPORT (:NORTHEAST (:LAT . 49.38) (:LNG . -66.94))
     (:SOUTHWEST (:LAT . 25.82) (:LNG . -124.39))))
   (:PLACE--ID . "ChIJCzYy5IS16lQRQrfeQ5K5Oxw")
   (:TYPES "country" "political")))
 (:STATUS . "OK"))
CL-USER>
```

The Google Maps API page will give you a lot more detail on what's contained in that huge blob.
