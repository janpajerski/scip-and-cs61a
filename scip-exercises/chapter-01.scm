; Exercises from SCIP Chapter 1

; ***Exercise 1.1***
; This asks what is te result printed by the interpreter in response to each expression

10  ;10

(+ 5 3 4) ;12

(- 9 1) ;8

(/ 6 2) ;3

(+ (* 2 4) (- 4 6)) ;6

(define a 3) ;a

(define b (+ a 1)) ;b

(+ a b (* a b)) ;19

(= a b) ;0 (wrong, the output is #f)

(if (and (> b a) (< b (* a b)))
    b
    a); 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)); 16

(+ 2 (if (> b a) b a)); 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)); 16

; ***Exercise 1.2***
; Translate an expression into prefix notation.

(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7))); -0.246666666666666667, verified correct using a calculator

; ***Exercise 1.3***
; Define a procedure that takes three nubmers as arguments
; and returns the sum of the squres of the two larger numbers.
;
; This is the method that first came to mind:
;
; Pseudo Code:
;
; Get three numbers as input
; Identify the largest two numbers
; Square the two largest numbers
; Sum the result.
;
; Getting three numbers is in the definition of the procdure, so trivial.
; Squaring and summing are both trivial. The crux is identifying the two
; largest numbers out of the three.
;
; Psuedo Code to fine the two largest number out of three (x y z)
; Number 1: largest or equal out of (x y)
; Number 2: largest or equal out of ((smallest or equal x y) z)

; Let's start
; First define square
(define (square x)
  (* x x))

; Now define larger-or-equal, and smaller-or-equal

(define (larger-or-equal x y)
  (cond ((< x y) y)
        (else x)))

(define (smaller-or-equal x y)
  (cond ((> x y) y)
        (else x)))

; We not have the building blocks of the procedure
; Let's define the procedure


(define (sum-square-larger-two x y z)
  (+ (square (larger-or-equal x y))
     (square (larger-or-equal (smaller-or-equal x y) z))))

; This procedure was tested and yields correct answers

; Another way to do it using the cond special form and primitive predicates
; and logical operators.
;
; Psuedo Code
;
; For numbers (x y z)
; IF (x + y) >= (x + z) AND (x + y) >= (y + z) then sum(square (x), square (y))
; ELSIF (x + z) >= (x + y) AND (x + z) >= (y + z) then sum(square (x), square (z))
; ELSE sum(square(y),square(z))

; I can use the square procedure defined previously
;

(define (sum-square-larger_two x y z)
  (cond ((and (>= (+ x y) (+ x z)) (>= (+ x y) (+ y z))) (+ (square x) (square y)))
        ((and (>= (+ x z) (+ x y)) (>= (+ x z) (+ y z))) (+ (square x) (square z)))
        (else (+ (square y) (square z)))))

; This procedure was tested and yields correct answers.

;***Exercise 1.4***
;Observe that our model of evaluation allows for combinations whose operators
;are compound expressions. Use this observation to describe the behaviour of
;the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; This procedure takes two numbers a and b, and adds a to the absolute value of b.
;
; More specifically,
; The procedure evaluates whether b is greater than 0, then based on the result
; it executes a primitive procedure where the operand is either + (if b was > 0)
; or the operand is - (if b is <= 0). The selection of the operand makes the
; expression equivalent to a + |b|
;
; By example, a and b are 3 and -5 respectively,
;
; ((if (> b 0) + -) a b)) results in (- 3 -5) which is 3 - (-5) which is equivalent to
; 3 + |-5|.
;
; If a and b are 3 and 5 respecively,
;
; ((if (> b 0) + -) a b) results in (+ 3 5) which is equivalent to 3 + |5|.
;
;
; ***exercise 1.5****
;
; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with
; is using applicative-order evaluation or normal-order evaluation. He defines the
; following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Then he evaluates the expression

(test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order
; evaluation? What behavior will he observe with an interpreter that uses normal-order
; evaluation? Explain your answer.
; (Assume that the evaluation rule for the special form if is the same whether the
; interpreter is using normal or applicative order: The predicate expression is evaluated
; first, and the result determines whether to evaluate the consequent or the alternative
; expression.)
;
; ***Answer***
;
; To recap, applicable-order evaluation methond is "evaluate the arguments and then apply"
; whereas the normal-order evaluation is "fully expand and then reduce".
;
; Based on these definitions, it's probably safe to assume that the second procedure will
; result in a different output. Likewise we can see that the frist procedure definition calls
; itself, therefore if the procedures is called it will loop without end.
;
; Lets evaluate the second procedure using the normal evaluation method:

(test 0 (p))

; becomes

(if (= 0 0)
    0
    (p))

; which results in 0.

; Let's evaluate using the applicative evaluation method:
;
(test 0 (p))

; the procedure will attempt to evaluate (p), which is (p), which is (p), which is (p)...
; and the procedure will hang.
;
; ***Example Calculating sqrt using Newton's approximation***
;
; To compute sqrt(x)
; 1. make a guess (y)
; 2. Get a better guess by averaging y with x/y
; 3. Good enough when (guess n) - (guess n-1) is < 0.001
;

(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (better-guess a guess)
  (/ (+ guess (/ a guess))
     2))


; one way

(define (good-enough a guess)
  (if (< (abs (- (square guess)
                 a))
         .001)
      guess
      (good-enough a (better-guess a guess))))

; another way

(define (good-enough a guess)
  (if (< (abs (- (better-guess a guess)
                 guess))
         .001)
      guess
      (good-enough a (better-guess a guess))))

; Let's define sqrt in terms of the

(define (sqrt x)
  (good-enough x 1))

; Both these procedures work
; Slightly different than what's in the example in the book.
; Upon review, it would be better style to have the good-enough
; test [the (< ) statement]; as a procedure on it's own.
; Then rename the "good-enough" procedure above as an iter-something
; because it's an iteration, that calls "good enough". My version
; combined the good-enough and and iteration in one.
;
; ALl good...lots of ways to skin the cat!!
;
; ***Exercise 1.6***
;
;  Alyssa P. Hacker doesn’t see why if needs to be provided as a special form.
;  “Why can’t I just define it as an ordinary procedure in terms of cond?” she
;  asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she
;  defines a new version of if:

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0

;Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

; What happens when Alyssa attempts to use this to compute square roots? Explain.
;
; I don't have intuition about what will happen.
; Let's summarize the procedures used in the sqrt program.

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))

; Using applicative order evaluation, i.e., evaluate arguments and then apply.

(sqrt-iter 1.0 x)
  (new-if (good-enough? 1.0 x)
          guess
          (sqrt-iter (improve guess x) x))

; Given that new-if is a procedure, the arguments are evaluated, then applied.
; So good-enough? Is evaluated to true, and sqrt-iter is evaluated, using improve
; guess. When sqrt-iter is called it evaluates both good-enough and sqrt-iter...
; and the process is stuck in a loop.

; I tested the code above using the interpreter. When I typed in sqrt (4) the process
; hung for a while then exited with an error. I'm assuming that what I described is
; what happend.


; ***Exercise 1.7****
; The good-enough? test used in computing square roots will not be very effective
; for finding the square roots of very small numbers. Also, in real computers,
; arithmetic operations are almost always performed with limited precision. This makes
; our test inadequate for very large numbers. Explain these statements, with examples
; showing how the test fails for small and large numbers. An alternative strategy for
; implementing good-enough? is to watch how guess changes from one iteration to the
; next and to stop when the change is a very small fraction of the guess. Design a
; square-root procedure that uses this kind of end test. Does this work better for
; small and large numbers?
;
; For very small numbers, i.e., .00004, the procedure yields errors as the guess
; iteration can meet the < .001 criteria and still be orders of magnitude from the
; small number --> the validation criteria is greater than then number being squre-rooted.
; Using the example above, (sqrt .00004) results in  0.0316750950802322
;
; For very large numbers, the limited precision of arithmetic operations results in errors
; as very large numbers are rounded.
; For example, the number 888,888,888,888,888,888,888,888,888,888,888,888,888,888 will be
; rounded to something like 8.888888888888888888889 in calculations. Accurate to 21 significant
; digits, but 20 digits away from the actual number. Using this example, the (sqrt) of the
; number above is calculated to be 9.42809041582063e+20. A result that is 20 or so digits away
; from the actual number. Squaring this result yields 8.88888888888888e+41 which is significantly
; different from the original number in terms of absolute value

; Here is the program:
;
(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (average x y)
  (/ (+ x y)
     2))

(define (better-guess radicand guess)
  (average guess (/ radicand guess)))

(define (sqrt-iter radicand guess)
  (if (good-enough radicand guess)
      guess
      (sqrt-iter radicand (better-guess radicand guess))))


(define (good-enough radicand guess)
  (define deltaGuess (abs (- (better-guess radicand guess) guess)))
  (< deltaGuess (/ guess 10000)))


(define (sqrt x)
  (sqrt-iter x 1))

; This program works better for small numbers. For large numbers,
; it is worse than the original progam.
