(require 'parsec)

(setq test-input "import { geopointFromGoogleMaps } from './geopoint';
import axios, { AxiosResponse } from 'axios';
import {
  Client,
  GeocodeResult,
  Status,
} from '@googlemaps/google-maps-services-js';
import { GeocodeResponseData } from '@googlemaps/google-maps-services-js/dist/geocode/geocode';
import { match, TaskEither } from 'fp-ts/lib/TaskEither';
import { identity, pipe } from 'fp-ts/lib/function';

const toPromise = <E, A>(te: TaskEither<E, A>): Promise<A> => {
  return pipe(
    te,
    match((error) => {
      throw error;
    }, identity)
  )();
};

describe('Creating Geopoints from Google Maps Geocoding API', () => {
  const expectedError = new Error('because I can');
  const erroringAxios = axios.create({
    adapter(_) {
      return Promise.reject(expectedError);
    },
  });

  const validGeopoint = { lng: -13.81, lat: 30.4 };

  const invalidGeopoint = { lng: 360, lat: 100 };

  const axiosStubReturningLocation = (location: any) =>
    axios.create({
      async adapter(config) {
        return {
          config,
          data: {
            results: [{ geometry: { location } } as GeocodeResult],
            status: Status.OK,
          },
        } as AxiosResponse<GeocodeResponseData>;
      },
    });

  const axiosReturningEmptyGeocodeResponse = axios.create({
    async adapter(config) {
      return {
        config,
        data: { status: Status.ZERO_RESULTS },
      } as AxiosResponse<GeocodeResponseData>;
    },
  });

  const geocodingRequest = {
    params: {
      address: '41 Turing Street, New Machines',
      key: '',
    },
  };

  describe('on success', () => {
    test('creates a valid geopoint', async () => {
      const program = geopointFromGoogleMaps(
        new Client({
          axiosInstance: axiosStubReturningLocation(validGeopoint),
        })
      );
      const result = await toPromise(program(geocodingRequest));
      expect(result.latitude).toEqual(validGeopoint.lat);
      expect(result.longitude).toEqual(validGeopoint.lng);
    });
  })

  describe('on failure', () => {

    test('returns invalid_geopoint on ZERO_RESULTS', async () => {
      const program = geopointFromGoogleMaps(
        new Client({
          axiosInstance: axiosReturningEmptyGeocodeResponse,
        })
      );

      try {
        await toPromise(program(geocodingRequest));
      } catch (error) {
        expect(error.type).toEqual('invalid_geopoint');
      }
    });

    test('returns invalid_geopoint if Geocoding returns an invalid geopoint', async () => {
      const program = geopointFromGoogleMaps(
        new Client({
          axiosInstance: axiosStubReturningLocation(invalidGeopoint),
        })
      );
      try {
        await toPromise(program(geocodingRequest));
      } catch (error) {
        expect(error.type).toEqual('invalid_geopoint');
        expect(error.value).toEqual(invalidGeopoint);
      }
    });

    test('returns the network error on network failure', async () => {
      const program = geopointFromGoogleMaps(
        new Client({ axiosInstance: erroringAxios })
      );
      try {
        await toPromise(program(geocodingRequest));
      } catch (error) {
        expect(error.type).toBe('network_error');
      }
    });
  })
});

describe('Geofence construction tests', () => {
  test('overlapping geofences are resized to respect the minimum spacing', () => {
    fc.assert(
      fc.property(overlappingGeofenceArb, ([first, second]) => {
        if (first && second) {
          const distanceApart = haversineDistance(first.point, second.point);
          expect(distanceApart).toBeGreaterThan(first.radius + second.radius);
        }
      }),
      {
        endOnFailure: true,
      }
    );
  });
});

")

(defun open-paren ()
  (unless (parsec-error-p (parsec-one-of ?\())
    '(:open-bracket "("))
  )

(parsec-with-input "()describe()" (open-paren))

(defun close-paren ()
  (unless (parsec-error-p (parsec-one-of ?\)))
    '(:close-bracket ")"))
  )

(defun describe-stanza ()
  (unless (parsec-error-p (parsec-re "[[:blank:][:space:]]*describe("))
    '(:describe)))

(parsec-with-input "describe(\")" (describe-stanza))

(defun test-stanza ()
  (unless (parsec-error-p (parsec-or
                           (parsec-re "[[:blank:][:space:]]*it(")
                           (parsec-re "[[:blank:][:space:]]*test(")))
    '(:test)))

(parsec-with-input "  test(\")" (test-stanza))

(defun stanza ()
  (parsec-or (describe-stanza) (test-stanza)))

(defun string-delim ()
  (parsec-one-of ?' ?\" ?`)
  )

(defun parse-test-name ()
  (let ((stanza (stanza))
        (q (string-delim)))
    (unless (or (parsec-error-p stanza) (parsec-error-p q))
      (let ((result (parsec-until-as-string (parsec-ch (aref q 0)))))
        (unless (parsec-error-p result)
          (append stanza result)))
      ))
  )

(defun eof ()
  (unless (parsec-error-p (parsec-eof))
    '(:eof "EOF")))

(defun parser-state (last-result open-brackets)
  (pcase (car last-result)
    (:describe `((parsec-or (open-paren) (close-paren) (parse-test-name) (eof)) ,(1+ open-brackets)))
    (:test `((parsec-or (open-paren) (close-paren) (eof)) ,(1+ open-brackets)))
    (:open-bracket `((parsec-or (close-paren) (open-paren) (eof)) ,(1+ open-brackets)))
    (:close-bracket `((parsec-or (open-paren) (close-paren) (parse-test-name) (eof)) ,(1- open-brackets)))
    (:start `((parsec-or (parse-test-name) (eof)) 0))
    )
  )

(parser-state '(:close-bracket "title") 0)

(defun run-parse ()
  (interactive)

  (let* ((results '())
         (open-brackets 0)
         (next-state (parser-state '(:start) open-brackets))
         (next-parser (car next-state))
         (namespaces (vector `("" ,open-brackets)))
         (result (parsec-until (eval next-parser) :end))
         (parsed-kind nil)
         (parsed-value nil)
         )

    (while (not (or (parsec-error-p result) (equal parsed-kind :eof)))
      (setq next-state (parser-state result open-brackets)
            parsed-kind (car result)
            parsed-value (cdr result)
            )
      (pcase parsed-kind
        (:close-bracket (let* ((ns (elt namespaces 0))
                               (ns-brackets (cadr ns))
                               (open-brackets (cadr next-state)))
                          (when (< open-brackets ns-brackets)
                            (setq namespaces (seq-drop namespaces 1))))
                        )
        (:describe (let* ((ns (elt namespaces 0))
                          (ns-title (car ns))
                          (ns-brackets (cadr ns))
                          (open-brackets (cadr next-state))
                          (new-ns (string-join (list ns-title parsed-value) (unless (string-empty-p ns-title) "/"))))
                     (setq namespaces (vconcat (vector `(,new-ns ,open-brackets)) namespaces))
                     (setq results (append `(,new-ns) results))
                     ))
        (:test (let* ((ns (elt namespaces 0))
                      (ns-title (car ns))
                      (new-ns (string-join (list ns-title parsed-value) (unless (string-empty-p ns-title) "/"))))
                 (setq results (append `(,new-ns) results))
                 ))
        )
      (setq
       next-parser (car next-state)
       open-brackets (cadr next-state)
       result (parsec-until (eval next-parser) :end)
       )
      )
    results
    )
  )

(parsec-with-input test-input
  (let* ((test-names (reverse (run-parse)))
         (choices (seq-map-indexed
                   (lambda (item idx) `(,item ,idx))
                   test-names))
         (choice (widget-choose "Select test to run: " choices))
         )
    (message "You chose to run '%s'" (car (rassoc choice choices)))
    )
 )
