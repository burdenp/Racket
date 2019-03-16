;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lab10) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))

;; *****************************************************
;; Chat client Skeleton

(require 2htdp/image)
(require 2htdp/universe)

;; A (chat) Client is (make-client name lines editor curse-on?)
;;   where name is a String, lines is a [Listof String],
;;   and editor is a String. curse-on? is a boolean
;;   used for making the cursor blink.
;; A (chat) Client is our local World.
;;
;; Interp.
;; A chat client needs three things:
;;   - The (nick)name of the chatter
;;   - A list of previous lines in the chat
;;     (prefixed by the nick of the sender)
;;   - The current line being edited
;;   - The state of the cursor (true or false) - used for making
;;     the cursor blink

(define-struct client (name lines editor curse-on?))

;;; Alfred E. Neuman (What, me worry?)
(define client-1  (make-client "neuman"
                               (list "neuman: Hola peeps"
                                     "neuman: Lame chat party :/")
                               "" false))
;;; The Joker
(define client-2 (make-client "Joker" '() "Why so seri" true))


;; Constants for various specifics of the client GUI
(define CLIENT-HEIGHT 200)
(define CLIENT-WIDTH 400)
(define FONT-SIZE 12)
(define FONT-COLOR "black")
(define LINE-SPACING 5)
(define LINE-INDENT 5)
(define DIVIDING-COLOR "blue")
(define NICK-DIVIDER " :  ")
(define CURSOR-IMG-ON (rectangle 4 (+ 4 (* 2 LINE-SPACING)) "solid" "red"))
(define CURSOR-IMG-OFF (rectangle 3 (+ 4 (* 2 LINE-SPACING)) "solid" "black"))

(define CHAT-AREA
  (local [(define SEP-Y (- CLIENT-HEIGHT FONT-SIZE (* 2 LINE-SPACING)))]
    (scene+line (empty-scene CLIENT-WIDTH CLIENT-HEIGHT)
                0 SEP-Y CLIENT-WIDTH SEP-Y DIVIDING-COLOR)))

;; W/2 : Image -> Number
;; Calculate the image width divided by 2
(define (W/2 img)
  (if (image? img)
      (/ (image-width img) 2)
      (error "W/2 expect an image, got: " img)))

;; H/2 : Image -> Number
;; Calculate the image height divided by 2
(define (H/2 img)
  (if (image? img)
      (/ (image-height img) 2)
      (error "W/2 expect an image, got: " img)))

;; chattify : String -> Image
(define (chattify s)
  (if (string? s)
      (text s FONT-SIZE FONT-COLOR)
      (error "chattify expects a string, got: " s)))

;; render-client : Client -> Scene
;; Render the client editor and stored chat lines
(define (render-client c)
  (if (and (client? c) (string? (client-name c)) (string? (client-editor c))
           (list? (client-lines c)) (andmap string? (client-lines c)))
      (local [(define editor (chattify (string-append (client-name c)
                                                      NICK-DIVIDER
                                                      (client-editor c))))]
        (add-chattings (client-lines c) (- CLIENT-HEIGHT (image-height editor)
                                           (* LINE-SPACING 2))
                       (place-image (if (client-curse-on? c) CURSOR-IMG-ON CURSOR-IMG-OFF)
                                    (+ LINE-INDENT (image-width editor) 4)
                                    (- CLIENT-HEIGHT (H/2 editor))
                                    (place-image editor
                                                 (+ LINE-INDENT (W/2 editor))
                                                 (- CLIENT-HEIGHT (H/2 editor))
                                                 CHAT-AREA))))
      (error "render-client expects a client, got: " c)))

;; add-chattings : [Listof String] Number Scene -> Scene
;; Add the given strings to the chat scene...
(define (add-chattings los y scn)
  (cond [(not (and (list? los) (andmap string? los)))
         (error "add-chattings: first argument must be a list of strings, was: " los)]
        [(not (number? y))
         (error "add-chattings: second argument must be a number, was: " y)]
        [(not (image? scn))
         (error "add-chattings: third argument must be a scene, was: " scn)]
        [else ; all good
         (cond [(empty? los) scn]
               [else (local [(define txt (chattify (first los)))]
                       (place-image txt
                                    (+ LINE-INDENT (W/2 txt))
                                    (- y (H/2 txt))
                                    (add-chattings (rest los)
                                                   (- y (image-height txt)
                                                      LINE-SPACING) scn)))])]))

;; strip-last : String -> String
;; strips the last character (if any) from a string
(define (strip-last s)
  (if (string? s)
      (substring s 0 (max (sub1 (string-length s)) 0))
      (error "strip-last expects a string, got: " s)))

(check-expect (strip-last "") "")
(check-expect (strip-last "omg") "om")

;; tick : Client -> Client
;; Blink the cursor...
(define (tick c)
  (if (and (client? c) (string? (client-name c)) (string? (client-editor c))
           (list? (client-lines c)) (andmap string? (client-lines c)))
      (make-client (client-name c)
                   (client-lines c)
                   (client-editor c)
                   (not (client-curse-on? c)))
      (error "tick expects a client, got: " c)))

;; handle-key : Client KeyEvent -> Client
;; handles key presses for the chat client

;;package is: (make-package World Message)
;;a message is a Sexpr
(define (handle-key c ke)
  (cond [(not (and (client? c) (string? (client-name c)) (string? (client-editor c))
                   (list? (client-lines c)) (andmap string? (client-lines c))))
         (error "handle-key: first argument must be a client, was: " c)]
        [(not (key-event? ke))
         (error "handle-key: second argument must be a key-event, was: " c)]
        [else ; all good
         (cond
           ;; ******* MODIFY HERE *********
           ;; ** Local Version... comment out to run Universe...
           #|  [(key=? ke "\r")
               (make-client (client-name c)
                          (cons (string-append (client-name c)
                                               NICK-DIVIDER
                                               (client-editor c))
                                (client-lines c))
                          "" (client-curse-on? c))]
|#
           ;; ******* MODIFY HERE *********
           ;; ** Universe Version...
           [(key=? ke "\r" ) (make-package (make-client (client-name c)
                                                        (client-lines c)
                                                        (client-editor c) (client-curse-on? c)) (list "MSG"
                                                                                       (client-editor c)))]
           [(key=? ke "\b")
            (make-client (client-name c) (client-lines c)
                         (strip-last (client-editor c))
                         (client-curse-on? c))]
           [(= (string-length ke) 1)
            (make-client (client-name c) (client-lines c)
                         (string-append (client-editor c) ke)
                         (client-curse-on? c)) ]
           [else c])]))

(check-expect (handle-key client-2 "\r")
              (make-package (make-client "Joker" empty "" true) "Joker :  Why so seri"))
(check-expect (handle-key client-2 "\b")
              (make-client "Joker" (list) "Why so ser" true))
(check-expect (handle-key client-1 "a")
              (make-client "neuman"
                           (list "neuman: Hola peeps"
                                 "neuman: Lame chat party :/")
                           "a" false))

;; handle-msg : Client Message -> Client
;; Handle an incoming message (String) by posting it
;; ******* MODIFY HERE *********
;;handle-msg: string -> client
(define (handle-msg c msg)
         (make-package (make-client (client-name c)
                      (cons (string-append (client-name c)
                                           NICK-DIVIDER
                                           (client-editor c))
                            (client-lines c))
                      (client-editor c) (client-curse-on? c)) (list "MSG" (client-editor c))))

#|
(check-expect (handle-msg client-2 "alice: What's up")
              (make-client "Joker" (list "alice: What's up")
                           "Why so seri" true))
(check-expect (handle-msg client-1 "bob: Hacking")
              (make-client "neuman"
                           (list "bob: Hacking"
                                 "neuman: Hola peeps"
                                 "neuman: Lame chat party :/")
                           "" false))
|#

;; run : String -> Client
;; Runs the chat client, given a nickname
(define (run nick)
  (if (string? nick)
      (big-bang (make-client nick (list) "" false)
                (on-draw render-client)
                (on-key handle-key)
                (on-tick tick 3/4)
                
                ;; ******* MODIFY HERE *********
                ;; ** Uncomment to run in Universe
                (register "seel.ccs.neu.edu")
                (on-receive handle-msg)
                (name nick))
      (error "run expects a string, got: " nick)))

;; ******* MODIFY HERE *********
(run "pat")