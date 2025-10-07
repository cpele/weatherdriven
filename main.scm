(use-modules (web server)
	     (srfi srfi-19)
	     (ice-9 threads)
	     (json))

(define-json-type <view>
  (server-started))

(define (handle-req request body start-date-str)
  (values
     '((content-type . (application/json)))
     (view->json (make-view start-date-str))))

(define (current-date-time-str)
  (date->string (current-date) "~Y~m~d-~H~M~S"))

(define-once server-thread #f)
(define-once server-socket #f)

(define (start-server)
  (unless server-socket
    (display "Setting up server socket... ")
    (set! server-socket (socket AF_INET SOCK_STREAM 0))
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    (bind server-socket AF_INET INADDR_LOOPBACK 8080)
    (listen server-socket 128)
    (display "Server socket set up\n"))
  (unless server-thread
    (display "Setting up server thread... ")
    (let [(start-date-str (current-date-time-str))]
      (set! server-thread
	    (call-with-new-thread
	     (lambda ()
	       (run-server
		(lambda (request body)
		  (handle-req request body start-date-str))
		'http
		`(#:socket ,server-socket))))))
    (display "Server thread set up\n")))

(define (stop-server)
  (when server-socket
    (display "Closing server socket... ")
    (close server-socket)
    (display "Socket closed\n")
    (set! server-socket #f))
  (when server-thread
    (display "Waiting for server thread to terminate... ")
    (cancel-thread server-thread)
    (join-thread server-thread)
    (display "Server thread terminated\n")
    (set! server-thread #f)))

(define (restart-server)
  (stop-server)
  (start-server))

(restart-server)




