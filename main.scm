(use-modules (web server)
             (web response)
	     (srfi srfi-19)
	     (ice-9 threads)
	     (ice-9 optargs)
	     (json))

(define (logd message)
  (display (string-append "[" (current-date-time-str) "] " message "\n")))

;; MVU program

(define (init)
  `((title . "Today's weather")
    (date . ,(date->string (current-date)))
    (forecast . ((hi . ((text . "Yo")
                        (temp-deg . 18)))
                 (lo . ((text . "Hey")
                        (temp-deg . 10)))))))

(define (view model)
  (scm->json-string model))

;; Runtime

(define (handle-req request body model)
  (values
   '[(content-type . (application/json))]
   (view model)))

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
    (let [(model (init))]
      (set! server-thread
	    (call-with-new-thread
	     (lambda ()
               (run-server
		(lambda (request body)
		  (handle-req request body model))
		'http
		`(#:socket ,server-socket)))))))
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




