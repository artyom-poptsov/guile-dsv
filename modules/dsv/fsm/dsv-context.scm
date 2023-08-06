(define-module (dsv fsm dsv-context)
  #:use-module (dsv fsm context)
  #:export (none
            event-source
            add-field
            add-non-empty-field
            add-row
            end-of-row?
            delimiter?
            non-printable-character?
            comment-prefix?
            push-non-printable-character
            prepare-result))

(define event-source next-char)

(define (none context)
  context)



(define (non-printable-character? context char)
  (or (char=? char #\f)
      (char=? char #\n)
      (char=? char #\r)
      (char=? char #\t)
      (char=? char #\v)))

(define (push-non-printable-character context char)
  (push-event-to-buffer context
                        (cond
                         ((char=? char #\f) #\page)
                         ((char=? char #\n) #\newline)
                         ((char=? char #\r) #\return)
                         ((char=? char #\t) #\tab)
                         ((char=? char #\v) #\vtab))))

(define (end-of-row? context char)
  (or (char:cr? context char)
      (char:lf? context char)))

(define (delimiter? context char)
  (let* ((custom-data (context-custom-data context))
         (delimiter   (assoc-ref custom-data 'delimiter)))
    (cond
     ((char? delimiter)
      (char=? delimiter char))
     ((string? delimiter)
      (char=? (string-ref delimiter 0) char))
     (else
      #f))))

(define (comment-prefix? context char)
  (let* ((custom-data (context-custom-data context))
         (prefix      (assoc-ref custom-data 'comment-prefix)))
    (cond
     ((char? prefix)
      (char=? prefix char))
     ((string? prefix)
      (char=? (string-ref prefix 0) char))
     (else
      #f))))

(define (context-buffer->string context)
  (list->string (context-buffer/reversed context)))

(define* (add-field context #:optional char)
  (log-debug "add-field: buffer: ~S; stanza: ~a"
             (context-buffer context)
             (context-stanza context))
  (clear-buffer
   (push-event-to-stanza context (context-buffer->string context))))

(define* (add-non-empty-field context #:optional char)
  (if (buffer-empty? context)
      context
      (add-field context)))

(define* (add-row context #:optional char)
  (log-debug "add-row: buffer: ~S; stanza: ~a"
             (context-buffer context)
             (context-stanza context))
  (clear-stanza
   (push-event-to-result context (context-stanza/reversed context))))

(define* (prepare-result context #:optional char)
  (log-debug "prepare-result: buffer: ~S; stanza: ~a; result: ~S"
             (context-buffer context)
             (context-stanza context)
             (context-result context))
  (reverse-result context))

;;; unix.scm ends here.
