
#|
Copyright (c) 2005-2007 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; Change Log
;;;
;;; 11/03/2016: added UNIFY tests [CKR]
;;; 10/21/2015: Moved planners to ddr-plan.lisp [CKR]
;;; 10/19/2015 Changed planner tests to new Planner, added blocks world [CKR]
;;; 02/14/2007: changed can-satisfy-hunger rules in *tiger-kb* [CKR]
;;; 03/21/2006: changed tests to use init-kb [CKR]
;;; 03/05/2005 Expanded documentation on Monkeys and Bananas [CKR]
;;; 03/05/2005 Added Peano addition example [CKR]


(in-package :ddr-tests)

;;; Test unifier, since it can have subtle bugs

(define-test unify
  (assert-true (unify '?x '?x))
  (assert-true (unify '(?x a) '(a ?x)))
  (assert-false (unify '(?x b) '(a ?x)))
  (assert-true (unify '(a ?x) '(?x ?x)))
  (assert-true (unify '(?x ?y) '(?y a)))
  (assert-false (unify '?x '(f ?x)))
  (assert-false (unify '(f ?x) '?x))
  (assert-false (unify '(?x ?y) '(?y (f ?x))))
  (assert-true (unify '(?x a) '(?x ?x)))
  (assert-true (unify '?x '(a)))
  (assert-false (unify '(?y ?x) '((f ?x) ?y)))
  (assert-true (unify '(?x a) '((f ?y) ?y)))
  (assert-false (unify '(a ?x) '(b c)))
  (assert-true (unify '(?y ?x a) '(?x ?y ?x)))
  )


;;; Simple ancestry-type knowledge base.
;;;
;;; NOTE: This is a BAD set of rules. It's
;;; combinatorially explosive and produces 
;;; many redundant answers.
;;; E.g., try (ASK-TRACE '(HORSE ?X)).
;;; How can you fix this and still
;;; pass the tests?

(defparameter *horses-kb*
  '(
    ;; x is ancestor of y if x is a parent or a parent of an ancestor
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
      
    ;; x is a horse if x is a descendant of a horse
    (<- (horse ?x) (ancestor ?y ?x) (horse ?y))
      
    ;; some real horses, not all real relationships
    (parent man-o-war war-admiral)
    (parent war-admiral seabiscuit)
    (parent seabiscuit kelso)
    (horse man-o-war)
    ))

(define-test horses
  (init-kb *horses-kb*)

  (dolist (x '(man-o-war war-admiral seabiscuit kelso))
    (assert-true (ask `(horse ,x)) x))

  (assert-true
   (set-equal '(man-o-war war-admiral seabiscuit kelso)
              (ask '(horse ?x) '?x)))
  )


(defparameter *tiger-kb*
  '(
    (<- (can-satisfy-hunger-with ?x ?y)
        (eats ?x ?y)
        (can-bite ?x ?y))

    (<- (eats ?y ?x)
        (predator-of ?x ?y))

    (<- (can-bite ?x ?y)
        (isa ?x animal)
        (near ?x ?y))

    (<- (near ?x ?y)
        (at ?x ?loc)
        (at ?y ?loc))

    (eats antelope grass)
    (eats antelope ferns)
    (predator-of antelope tiger)
    (predator-of zebra tiger)

    (at antelope savannah)
    (at tiger savannah)
    (at grass savannah)

    (isa tiger animal)
    (isa antelope animal)
    ))

(define-test tiger
  (init-kb *tiger-kb*)
  (assert-true (ask '(at antelope savannah)))
  (assert-true (ask '(near antelope grass)))
  (assert-true (ask '(can-bite antelope grass)))
  (assert-true (ask '(can-satisfy-hunger-with antelope grass)))
  (assert-false (ask '(can-satisfy-hunger-with antelope ferns)))
  (assert-true (ask '(can-satisfy-hunger-with tiger antelope)))
  (assert-false (ask '(can-satisfy-hunger-with tiger grass)))
  (assert-false (ask '(can-bite grass antelope)))

  )


;;; Addition, Peano style
;;;
;;; A simple example of using functional terms to represent
;;; constructed results.


(defparameter *peano-kb*
  '(
    (add 0 ?x ?x)
    (<- (add (succ ?x) ?y (succ ?z))
        (add ?x ?y ?z))
    ))

(define-test peano
  (init-kb *peano-kb*)
  ;; 0 + 0 = 0
  (assert-true (ask '(add 0 0 0) '?x))
  ;; 0 + 1 = 1
  (assert-true (ask '(add 0 (succ 0) (succ 0))))
  ;; 1 + 0 = 1
  (assert-true (ask '(add (succ 0) 0 (succ 0))))
  ;; 2 + 2 = 4
  (assert-true (ask '(add (succ (succ 0)) (succ (succ 0))
                          (succ (succ (succ (succ 0)))))))
  ;; 2 + 1 != 4
  (assert-false (ask '(add (succ (succ 0)) (succ 0)
                           (succ (succ (succ (succ 0)))))))
  ;; 0 + 0 => 0
  (assert-equal '(0) (ask '(add 0 0 ?x) '?x))
  ;; 0 + 1 => 1
  (assert-equal '((succ 0)) (ask '(add 0 (succ 0) ?x) '?x))
  ;; 1 + 0 => 1
  (assert-equal '((succ 0)) (ask '(add (succ 0) 0 ?x) '?x))
  ;; 2 + 2 => 4
  (assert-equal '((succ (succ (succ (succ 0)))))
                (ask '(add (succ (succ 0)) (succ (succ 0)) ?x) 
                     '?x))
  ;; 1 + x = 3 => x = 2, or 3 - 1 => 2
  (assert-equal '((succ (succ 0)))
                (ask '(add (succ 0) ?x (succ (succ (succ 0))))
                     '?x))
  ;; x + y = 3 => <3, 0>, <2, 1>, <1, 2>, <0, 3>
  (assert-equal '(((succ (succ (succ 0))) 0) 
                  ((succ (succ 0)) (succ 0)) 
                  ((succ 0) (succ (succ 0)))
                  (0 (succ (succ (succ 0)))))                
                (ask '(add ?x ?y (succ (succ (succ 0))))
                     '(?x ?y)))
  )

;;; APPEND, Prolog-style

(defparameter *append-kb*
  '(
    (append nil ?x ?x)
    (<- (append (cons ?x ?l1) ?l2 (cons ?x ?l3))
        (append ?l1 ?l2 ?l3))
    ))


(define-test append
  (init-kb *append-kb*)
  (assert-equal '((cons a (cons b (cons c nil))))
                (ask '(append (cons a (cons b nil))
                              (cons c nil)
                              ?l)
                     '?l))
  (assert-equal '((cons c nil))
                (ask '(append (cons a (cons b nil))
                              ?l
                              (cons a (cons b (cons c nil))))
                     '?l))
  (assert-equal '((cons a (cons b nil)) (cons a nil) nil)
                (ask '(append ?x ?y (cons a (cons b nil)))
                     '?x))
  )

;;; This checks for a variable renaming bug that was in 
;;; ddr.lisp for 20 years! 

(defparameter *analogy-kb*
  '(
    (<- (analogous ?x ?y ?a ?b)
        (similar ?x ?a)
        (similar ?y ?b))
    (similar ?x ?x)
    ))

(define-test analogy
  (init-kb *analogy-kb*)
  
  (assert-true (ask '(analogous a a a a)))
  (assert-true (ask '(analogous b b b b)))
  (assert-true (ask '(analogous a b a b)))
  (assert-equal '((analogous a b a b))
                (ask '(analogous a b ?x ?y)))
  )


;;; This is a basic framework for doing planning
;;; with the deductive retriever (DDR). 
;;;
;;; There are three core predicates.
;;;
;;;   (PLAN plan start-state goal-state)
;;;      Asserts that plan leads from start-state
;;;      to goal-state. Plan is either nil or
;;;      (CONS action plan).
;;;
;;;   (RESULTS action current-state next-state)
;;;      Asserts that doing action in current-state
;;;      leads to next-state. 
;;;
;;;   (STEP action current-state goal-state)
;;;      Asserts that action is a good step in a plan to get
;;;      from current-state to goal-state. 
;;;
;;; To define a planner:
;;;   - Design the terms to represent actions and states.
;;;     States should be simple and unique, i.e.,
;;;     there should be just one way to describe any
;;;     given state.
;;;
;;;   - Define RESULTS rules to indicate how each action
;;;     changes a state. These rules are normally
;;;     very simple.
;;;
;;;   - Define STEP rules to pick the best action and avoid
;;;     endless loops.
;;;
;;;   - Include the following two rules for PLAN. No others
;;;     are needed.

(defparameter *plan-kb*
  '(
    ;; Use the empty plan if current state = goal state
    (<- (plan nil ?goal ?goal))
    
    ;; Use the plan (cons action actions) if 
    ;;   - action is a good step for the current and goal states
    ;;   - remaining actions is a plan that gets from the 
    ;;     action result state to the goal
    (<- (plan (cons ?action ?actions) ?current ?goal)
        (step ?action ?current ?goal)
        (results ?action ?current ?result)
        (plan ?actions ?result ?goal))
    ))
