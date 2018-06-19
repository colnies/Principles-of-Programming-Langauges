 (define (echo lst)
  (if (null? lst)                              ;If list is null
      '()                                      ;Return
      (cons (car lst)                          ;Print item twice
            (cons (car lst)
                  (echo (cdr lst)) ) ) ))      ;Recursive call on item pair

(define (echo-lots 1st n)
  (define (echo-b e c)                         ;Loop print function
    (if (= c 0)
        '()
        (cons e (echo-b e (- c 1) ) ) ))
  (if (null? 1st)                              ;If list is not null, append return of loop function with
      '()                                      ;recursive call on item pair
      (append (echo-b (car 1st) n) (echo-lots (cdr 1st) n) ) ))

(define (echo-all 1st)
  (if (null? 1st)
      '()
      (if(not (list? (car 1st)))
         (cons (car 1st)
               (cons (car 1st)
                     (echo-all (cdr 1st))))
         (cons (echo (car 1st))
                     (cons (echo (car 1st))
                           (echo-all (cdr 1st))) ) ) ))

(define (nth n 1st)
  (if (= n 0)                                  ;If n=0
      (car 1st)                                ;Print item and return
     (nth (- n 1)(cdr 1st) ) ))                ;Recursive call to decrease n val and second item

(define (assoc-all keys lst)
    (if (null? keys)
        '()
        (cons (cadr (assoc (car keys) lst))
              (assoc-all (cdr keys) lst))))

(define (filter fn 1st)
  (if (null? 1st)                              ;If list is null, return
      '()
      (if (fn (car 1st))                       ;If first item on list matches function def
          (cons (car 1st)                      ;Print item
          (filter fn (cdr 1st) ) )             ;Recursive call on second item
                (filter fn (cdr 1st) ) ) ))    ;Recursive call on second item if not hit by if statements

(define (filter-out-er fn)
  (define f (lambda(x)
              (if (null? x)
                  '()
                  (if (not (fn (car x)))
                      (cons (car x)
                            (f (cdr x)) )
                      (f (cdr x)) ) ))) f)