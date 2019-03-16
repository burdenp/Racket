;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname homework6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;HTDP 14.1.3
(define-struct child (father mother name date eyes))
#|A child node is (make-child f m na da ec) where
f and m are either
      empty or
      child nodes;
na and ec are symbols;
da is a number
|#
;;count-persons: child -> number
;;takes in a child node structure and returns the number of people in its
;;family tree
(define (count-persons child)
  (cond [(and(empty? (child-father child)) 
             (empty?(child-mother child))) 1]
        [(empty? (child-father child))
         (+ 1 (count-persons (child-mother child)))]
        [(empty? (child-mother child))
         (+ 1 (count-persons (child-father child)))]
        [else (+ 1 (count-persons (child-mother child))
                 (count-persons (child-father child)))]))
(define bob (make-child empty empty 'bob 123 'blue))
(define carol (make-child empty empty 'carol 123 'green))
(define adam (make-child bob carol 'adam 123 'green))
(define george (make-child adam empty 'adam 123 'blue))
(check-expect (count-persons adam) 3)
(check-expect (count-persons george) 4)

;;HTDP 14.1.5
;;eye-colors: child -> list of symbols
;;takes in a child and returns a list of symbols representing the eye colors
;;of everyone in the family tree
(define (eye-colors child)
  (cond [(and(empty? (child-father child)) 
             (empty?(child-mother child))) 
         (list (child-eyes child))]
        [(empty? (child-father child))
         (append(list (child-eyes child) (eye-colors (child-mother child))))]
        [(empty? (child-mother child))
         (append(list (child-eyes child)) (eye-colors (child-father child)))]
        [else (append(list (child-eyes child)) (eye-colors (child-mother child))
                 (eye-colors (child-father child)))]))
(check-expect(eye-colors george) (list 'blue 'green 'green 'blue))
(check-expect(eye-colors adam) (list 'green 'green 'blue))

;;HTDP 14.2.4
(define-struct node (ssn name left right))
#|
A binary-tree (short: BT) is either
    false
    (make-node soc pn lft rgt) 
       where soc is a number, pn is a symbol, and lft and rgt are BTs.
A binary-search-tree (short: BST) is a BT:
false is always a BST;
(make-node soc pn lft rgt) is a BST if
      lft and rgt are BSTs,
      all ssn numbers in lft are smaller than soc, and
      all ssn numbers in rgt are larger than soc.
|#
;;search-bst: number BST-> symbol or false
;;takes in a number and a BST it searches through the BST comparing the number
;;to the BST's soc and returning that BSt's pn if there is a match
;;otherwise it returns false
(define (search-bst num bst)
  (cond [(= (node-ssn bst) num) (node-name bst)]
        [(and (false? (node-left bst)) (false? (node-right bst))) false]
        [(and (false? (node-left bst)) (< num (node-ssn bst))) false]
        [(and (false? (node-right bst)) (> num (node-ssn bst))) false]
        [(> num (node-ssn bst)) (search-bst num (node-right bst))]
        [(< num (node-ssn bst)) (search-bst num (node-left bst))]
        [else (node-name bst)]))
(define node1 (make-node 16 'ben false false))
(define node2 (make-node 12 'bob false node1))
(define node3 (make-node 15 'steve node2 node1))
(define node4 (make-node 17 'joe node1 false))
(check-expect (search-bst 16 node3) 'ben)
(check-expect (search-bst 16 node4) 'ben)

;;Part 2
;;(make-profile string symbol symbol LoF)
;; an LoF is a list of friend which is either
;; empty
;; (cons friend LoF)
(define-struct profile (user loc status friends))
;;(make-friend string symbol symbol)
(define-struct friend (name loc status))

(define profile1 (make-profile "ben" 'boston 'single empty))
(define friend1 (make-friend "ben" 'boston 'single))
(define LoF1 (list friend1))
(define profile2 (make-profile "bob" 'boston 'single LoF1))
(define friend2 (make-friend "bob" 'boston 'single))
(define LoF2 (list friend2 friend1))
(define profile3 (make-profile "amber" 'boston 'single LoF2))
(define friend3 (make-friend "amber" 'boston 'single))
(define LoF3 (list friend3 friend2 friend1))
#|
(define (profile-temp p)
  (cond [... (profile-user p)...]
        [... (profile-loc p)...]
        [... (profile-status p)...]
        [... (profile-friends p)...]))
(define (freinds-temp f)
  (cond [... (friend-name f)...]
        [... (friend-loc f)...]
        [... (friend-status f)...]))
(define (LoF-temp LoF)
  (cond [...(first-LoF)...]
        [... (rest-LoF)...]))
|#
;;total-friends: profile -> number
;;takes in a profile and returns the number of friends it has in its LoF
(define (total-friends p)
  (friends-count (profile-friends p)))
;;friends-count: LoF -> number
;;takes in a List and counts the elements in it 
;;helper function to total-friends
(define (friends-count LoF)
  (cond [(empty? LoF) 0]
        [else (+ 1 (friends-count (rest LoF)))]))
(check-expect(friends-count LoF1) 1)
(check-expect(friends-count LoF2) 2)
(check-expect(friends-count LoF3) 3)
(check-expect(total-friends profile2) 1)
(check-expect(total-friends profile3) 2)

;;add-friend: profile friend -> profile
;;takes in a profile and a friend and adds the friend to the profiles LoF
;;if the friend is already on the profile it does nothing
(define (add-friend p f)
  (cond [(in-list (profile-friends p) f) p]
        [else 
         (make-profile (profile-user p) 
                       (profile-loc p)
                       (profile-status p)
                       (cons f (profile-friends p)))]))
;;in-list: LoF friend -> boolean
;;takes in a list of friends and a friend
;;if the friend is on the list it returns true else false
;;helper for add-friend
(define (in-list LoF f)
  (cond [(empty? LoF) false]
        [(friend=? (first LoF) f) true]
        [else (in-list (rest LoF) f)]))
;;friend=?: friend friend -> boolean
;;takes in 2 friends and sees if they are the same
;;helper to in-list
(define (friend=? f1 f2)
  (and (string=? (friend-name f1) (friend-name f2))
       (symbol=? (friend-loc f1) (friend-loc f2))
       (symbol=? (friend-status f1) (friend-status f2))))
(check-expect(friend=? friend1 friend2) false)
(check-expect(friend=? friend2 friend3) false)
(check-expect(in-list LoF1 friend1) true)
(check-expect(in-list LoF2 friend1) true)
(check-expect(add-friend profile2 friend1) profile2)
(check-expect(add-friend profile3 friend1) profile3)

;;un-friend: profile friend -> profile
;;takes in a profile and a friend
;;removes the friend from the profiles LoF if it is in the profiles LoF
(define (un-friend p f)
  (cond [(not(in-list (profile-friends p) f)) p]
        [else (make-profile (profile-user p) 
                            (profile-loc p)
                            (profile-status p)
                            (remove-list (profile-friends p) f))]))
;;remove-list: LoF friend -> LoF
;;takes in a list of friends and a friend and removes that friend from the list
;;helper to un-friend
(define (remove-list LoF f)
  (cond [(empty? LoF) LoF]
        [(friend=? (first LoF) f) (rest LoF)]
        [else (cons (first LoF) (remove-list LoF f))]))
(check-expect(remove-list LoF2 friend2) LoF1)
(check-expect(remove-list LoF3 friend3) LoF2)
(check-expect(un-friend profile2 friend3) profile2)
(check-expect(un-friend profile1 friend2) profile1)

;;friends?: profile profile -> boolean
;;takes in 2 profiles and then sees if they are friends with each other
(define (friends? p1 p2)
  (and (in-list (profile-friends p1)(turn-friend p2))
       (in-list(profile-friends p2)(turn-friend p1))))
;;turn-friend: profile -> friend
;;takes a profile and turns it into a friend structure
(define (turn-friend p)
  (make-friend (profile-user p) (profile-loc p) (profile-status p)))

(check-expect (turn-friend profile1) friend1)
(check-expect (turn-friend profile2) friend2)
(check-expect (friends? profile1 profile2) false)
(check-expect (friends? profile3 profile1) false)

;;print-friends: profile -> string
;;takes a profile and 
;;prints out a string of all of the profiles friends names
;;does not have spaces nor does it print out empty at the end of the list
(define (print-friends p)
  (print-list (profile-friends p)))
;;print-list: list -> string
;;returns a string of the names in a list 
;;does not print empty there are no spaces
;;helper to print-friends
(define (print-list LoF)
  (cond [(empty? LoF) ""]
        [else (string-append (friend-name(first LoF))
                             (print-list (rest LoF)))]))
(check-expect(print-list LoF1) (friend-name friend1))
(check-expect(print-list LoF2) "bobben")
(check-expect(print-friends profile1) "")
(check-expect(print-friends profile2) (friend-name friend1))



        
        
        
        
        
        
        
        







