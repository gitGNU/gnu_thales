(define-module (im irc)
    #:version (1 6 0)
    #:use-module (prelude)
    #:use-module (oop goops)
    #:use-module (ice-9 optargs)
    #:use-module (ice-9 receive)
    #:export (create-connection raw! run! join! quit! disconnect! hook-ref!))
(load-extension "libguile-ircclient" "scm_init_im_irc")

(define-class <irc-session> ()
  (server #:getter &server)
  (nickname #:getter &nickname)
  event-hook-alist ;; (#:event . (hook arity))
  (ffi-session #:getter &ffi-session))

(define-method (initialize (obj <irc-session>) args)
  "Documenation to be written."
  (let-keywords
   args #f ((server (error "<irc-session>:: server must be specified"))
            (nickname (error "<irc-session>:: nickname must be specified"))
            (port 6667)
            server-password username realname ipv6)
   (slot-set! obj 'server server)
   (slot-set! obj 'nickname nickname)
   (initialize-event-hook-alist obj)
   (let ((ffi-session (irc-create-session obj event-dispatcher numeric-dispatcher)))
     (slot-set! obj 'ffi-session ffi-session)
     (irc-connect ffi-session server port nickname
                  server-password username realname ipv6))))


(define-public (create-connection . args)
  (apply make <irc-session> args))


(define-method (run! (obj <irc-session>))
  (irc-run (slot-ref obj 'ffi-session)))


(define-method (join! (obj <irc-session>) (channel <string>) . args)
  (let-keywords args #f
                ((password #f))
                (irc-cmd-join (slot-ref obj 'ffi-session) channel password)))

(define-method (quit! (obj <irc-session>) . args)
  (let-keywords args #f
                ((reason #f))
                (irc-cmd-quit (slot-ref obj 'ffi-session) reason)))
(define-method (raw! (obj <irc-session>) command)
  (irc-send-raw (slot-ref obj 'ffi-session) command))

(define-method (hook-ref! (obj <irc-session>) (event-keyword <keyword>))
  "Get hook for event "
  (car (or (assq-ref (slot-ref obj 'event-hook-alist) event-keyword)
           (error "Unsupported event" event-keyword))))

(define-method (disconnect! (obj <irc-session>))
  "Rude disconnect from server. Due bug in underlying library,
 do not use it."
  (irc-disconnect (slot-ref obj 'ffi-session)))

(define +event::symbol-arity-alist+
  '((#:connect . ()) ; <closure>
    (#:nick . (#f 0))  ; <closure> <old-nick> <new-nick>
    (#:quit . (#f 0)) ; <closure> <nick> <?reason>
    (#:join . (0 #f)) ; <closure> <channel> <nick>
    (#:part . (0 #f 1)) ; <closure> <channel> <nick> <?reason>
    (#:mode . (0 #f 1 2)) ; <closure> <channel> <nick> <mode> <?mode-args>
    (#:umode . (#f 0)) ; <closure> <nick> <mode>
    (#:topic . (0 #f 1)) ; <closure> <channel> <nick> <?new-topic>
    (#:kick . (0 #f 1 2)) ; <closure> <channel> <nick> <?victim> <?reason>
    (#:channel . (0 #f 1)) ;  <closure> <channel> <nick> <?text>
    (#:privmsg . (#f 1)) ; <closure> <visavi> <?text>
    (#:notice . (#f 1)) ; <closure> <source> <?text>
    (#:invite . (1 #f)))) ;<closure> <channel> <origin>

(define (get-arg origin params index)
  "Return argument, corresponding to `index`.
If `index` equal -1, return origin, else params[index]."
  (if index
      (if (>= index (length params)) "" (list-ref params index))
      (irc-target-get-nick origin)))

(define (get-args origin params template)
  "Return list of arguments to pass to `run-hook`."
  (map (partial-apply get-arg origin params) template))

(define (event-dispatcher obj event-string origin params)
  "Dispatch C-API event handler to specific events hooks."
  (let* ((event-keyword (symbol->keyword (string->symbol (string-downcase event-string))))
         (hook&template (or (assq-ref (slot-ref obj 'event-hook-alist) event-keyword)
                            (error "Unhandled event." event-keyword)))
         (hook (car hook&template))
         (template (cdr hook&template)))
    (apply run-hook hook obj (get-args origin params template))))

(define (initialize-event-hook-alist obj)
  "Initialize event-hook-alist field of <irc-session> obj
to list in format (#:event-keyword . (hook . hook-args-template))"
  (slot-set! obj 'event-hook-alist
             (map
              (lambda (pair)
                (let* ((keyword (car pair))
                       (args-template (cdr pair))
                       (arity (1+ (length args-template))))
                  (cons
                   keyword
                   (cons (make-hook arity) args-template))))
              +event::symbol-arity-alist+)))

(define (numeric-dispatcher obj event-code origin params)
  (display (format "~d\n" event-code)))
