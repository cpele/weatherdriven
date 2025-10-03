(use-modules (web server)
	     (srfi srfi-19)
	     (ice-9 threads))

;; Define web server handler
(define (handle-req request request-body)
  (values
     '((content-type . (text/plain)))
     "Yo 🙏"))

(define (current-date-time-str)
  (date->string (current-date) "~Y~m~d-~H~M~S: "))

(display (string-append
	  (current-date-time-str)
	  "Starting server\n"))

;; Define empty (undefined?) server thread & socket vars
(define server-thread #f)
(define server-socket #f)

;; Start-server function:
;; - Skip when server-thread is defined 
;; - Create socket (inet, stream)
;; - Set socket options: SOL_SOCKET? SO_REUSEADDR? 1?
;; - Bind socket with family AF_INET, address INADDR_ANY and port 8080
;; - Make socket listen with a backlog of 128
;; - Execute run-server using the server socket in a new thread
;; - Keep the thread in the server-thread var
(define (start-server)
  (unless server-socket
    (display "Setting up server socket...\n")
    (set! server-socket (socket AF_INET SOCK_STREAM 0))
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    (bind server-socket AF_INET INADDR_LOOPBACK 8080)
    (listen server-socket 128)
    (display "Server socket set up\n"))
  (unless server-thread
    (display "Setting up server thread...\n")
    (set! server-thread
	  (call-with-new-thread
	   (lambda ()
	     (run-server
	      (lambda (request request-body)
		(handle-req request request-body))
	      'http
	      `(#:socket ,server-socket)))))
    (display "Server thread set up\n")))

;; Stop-server function:
;; - Check for existing server socket
;; - Close the server socket
;; - Clear the server-socket variable
;; - Check for existing server thread
;; - Join the server thread to let the server loop exit
;; - Clear the server-thread var
(define (stop-server)
  (when server-socket
    (display "Closing server socket...\n")
    (close server-socket)
    (display "Socket closed\n")
    (set! server-socket #f))
  (when server-thread
    (display "Waiting for server thread to terminate...\n")
    (cancel-thread server-thread)
    (join-thread server-thread)
    (display "Server thread terminated\n")
    (set! server-thread #f)))

;; Restart-server function: stop then start
;; TBD

;; Use restart-server function
;; TBD

;; (define server-thread
;;   (call-with-new-thread
;;    (lambda ()
;;      (run-server handle-req))))



