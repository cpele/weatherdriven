(use-modules (web server)
             (web response)
	     (srfi srfi-19)
	     (ice-9 threads)
	     (ice-9 optargs)
             (ice-9 exceptions)
             (ice-9 format)
	     (json))

(define (logd message)
  (display
   (string-append
    "[" (current-date-time-str) "] " message "\n"))
  (flush-all-ports))

;; MVU program

(define (init)
  `((model . ((title . "Today's weather")
              (date . ,(date->string (current-date)))
              (forecast . ((hi . ((text . "Yo")
                                  (temp-deg . 18)))
                           (lo . ((text . "Hey")
                                  (temp-deg . 10)))))))
    (effects . ((lambda ()
                  (fetch))))))

(define (fetch)
  (display "TBD: Fetch"))

(define (view model)
  (scm->json-string model))

;; Runtime

(define (handle-req request body model)
  (catch #t
    (lambda ()
      (values
       '[(content-type . (application/json))]
       (view model)))
    (lambda (exn . args)
      (values
       '[(content-type . (text/plain))]
       (format #f "Error handling request: ~a\n" exn)))))

(define (current-date-time-str)
  (date->string (current-date) "~Y~m~d-~H~M~S"))

(define-once server-thread #f)
(define-once server-socket #f)

(define (start-server)
  (unless server-socket
    (logd "Setting up server socket... ")
    (set! server-socket (socket AF_INET SOCK_STREAM 0))
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    (bind server-socket AF_INET INADDR_LOOPBACK 8080)
    (listen server-socket 128)
    (logd "Server socket set up"))
  (unless server-thread
    (logd "Setting up server thread... ")
    (set! server-thread
	  (call-with-new-thread 
	   (lambda ()
             (with-exception-handler
                 (lambda (exception)
                   (format (current-error-port)
                           "Error starting server: ~a\n"
                           exception)
                   (force-output (current-error-port))
                   (exit 1))
               (lambda ()
                 (dynamic-wind
                   (lambda ()
                     (logd "(Before running server)"))
                   (lambda ()
                     (let* [(change (init))
                            (model (assoc-ref change 'model))
                            (effects (assoc-ref change 'effects))]
                       (logd (format #f "Effects: ~a" effects))
                       (run-server
		        (lambda (request body)
                          (handle-req request body model))
		        'http
		        `(#:socket ,server-socket))))
                   (lambda ()
                     (logd "(After running server)")))))))))
  (logd "Server thread set up"))

(define (stop-server)
  (when server-socket
    (logd "Closing server socket... ")
    (close server-socket)
    (logd "Socket closed")
    (set! server-socket #f))
  (when server-thread
    (logd "Waiting for server thread to terminate... ")
    (cancel-thread server-thread)
    (join-thread server-thread)
    (logd "Server thread terminated")
    (set! server-thread #f)))

(define (restart-server)
  (stop-server)
  (start-server))

(restart-server)




