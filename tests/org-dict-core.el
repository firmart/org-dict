(require 'org-dict-core)

(setq org-dict-test--cntrl-dom (org-dict--url-to-dom "https://cnrtl.fr/definition/son"))

(defun org-dict-test--setup-dummy-dom ()
  (setq org-dict-test--dummy-node1  '(div ((class . "foo bar")) "Yes"))
  (setq org-dict-test--dummy-node2  '(div ((class . "foo")) "No"))
  (setq org-dict-test--dummy-dom  `(body ((width . "101"))
					 ,org-dict-test--dummy-node1
					 ,org-dict-test--dummy-node2)))

(org-dict-test--setup-dummy-dom)

(ert-deftest org-dict-test--url-to-dom ()
  (ert-skip "Not implemented yet"))

(ert-deftest org-dict-test--dom-replace-node ()
  "Replace two nodes in DOM."
  
  (let* ((dom org-dict-test--dummy-dom)
	 (node-list (dom-by-class dom "foo"))
	 (node1 (car node-list))
	 (node2 (cadr node-list))
	 (new-node '(div nil "SUCCESS"))
	 (expected-dom1 '(body ((width . "101"))
			       (div nil "SUCCESS")
			       (div ((class . "foo"))
				    "No")))
	 (expected-dom2 '(body ((width . "101"))
			       (div nil "SUCCESS")
			       (div nil "SUCCESS"))))
    (org-dict--dom-replace-node dom node1 new-node)
    (should (equal dom expected-dom1))
    (org-dict--dom-replace-node dom node2 new-node)
    (should (equal dom expected-dom2))
    (org-dict-test--setup-dummy-dom)))

(ert-deftest org-dict-test--dom-replace-nodes ()
  "Replace two nodes in DOM in one pass."
  (let* ((dom org-dict-test--dummy-dom)
	 (node-list (dom-by-class dom "foo"))
	 (new-node '(div nil "SUCCESS"))
	 (new-node-list (make-list 2 new-node))
	 (expected-dom2 '(body ((width . "101"))
			       (div nil "SUCCESS")
			       (div nil "SUCCESS"))))
    (org-dict--dom-replace-nodes dom node-list new-node-list)
    (should (equal dom expected-dom2))
    (org-dict-test--setup-dummy-dom)))

(ert-deftest org-dict-test--dom-node-class ()
  "Return classes of a node."
  (let* ((dom org-dict-test--dummy-dom)
	 (node (car (dom-by-class dom "foo bar")))
	 (node-classes (org-dict--dom-node-class node))
	 (expected-node-classes '("foo" "bar")))
    (should (equal node-classes expected-node-classes))))

(ert-deftest org-dict-test--dom-node-class-p ()
  "Check if a node has given class."
  (let* ((dom org-dict-test--dummy-dom)
	 (node (car (dom-by-class dom "foo bar"))))
    (should (org-dict--dom-node-class-p "foo" node))
    (should (org-dict--dom-node-class-p "bar" node))
    (should-not (org-dict--dom-node-class-p "foo bar" node))))

;; (ert-deftest org-dict-test--dom-node-tag-id-class-p ()
;;   (let* ((dom org-dict-test--dummy-dom)
;; 	 (node (car (dom-by-class dom "foo bar"))))
;;     (should (org-dict--dom-node-tag-id-class-p 'div nil "foo" node))
;;     (should (org-dict--dom-node-tag-id-class-p 'div nil "bar" node))
;;     (should (org-dict--dom-node-tag-id-class-p 'body nil nil dom))))

;; (ert-deftest org-dict-test--dom-by-class ()
;;   (let* ((dom org-dict-test--dummy-dom))
;;     (should (equal (org-dict--dom-by-class dom '("foo")) (list org-dict-test--dummy-node1 org-dict-test--dummy-node2)))
;;     (should (equal (org-dict--dom-by-class dom "foo") (list org-dict-test--dummy-node1 org-dict-test--dummy-node2)))
;;     (should (equal (org-dict--dom-by-class dom '("bar")) (list org-dict-test--dummy-node1)))
;;     (should (equal (org-dict--dom-by-class dom '("bar" "foo")) (list org-dict-test--dummy-node1)))
;;     (should (equal (org-dict--dom-by-class dom '("foo" "bar")) (list org-dict-test--dummy-node1)))))

(ert-deftest org-dict-test--dom-node-simple-selector-p ()
    (ert-skip "Not implemented yet"))
(ert-deftest org-dict-test--dom-by-attrs ()
    (ert-skip "Not implemented yet"))

(ert-deftest org-dict-test--dom-simple-select ()
  "Test simple selector w/ or w/o class/id/tag/attribute and composition with itself."
  (let ((dom org-dict-test--cntrl-dom)
	;; tag only
	(selector1 '(:tag div))
	(selector2 '(:tag span))
	(selector6 '(:tag td))
	;; tag with id
	(selector3 '(:tag div :attrs ((id . "wrap"))))
	(selector21 '(:tag div :attrs ((id))))
	;; tag with class
	(selector4 '(:tag div :attrs ((class . "tlf_paraputir"))))
	(selector5 '(:tag span :attrs ((class . "tlf_ctitre"))))
	;; attribute only w/ or w/o value: e.g. [width] and [width~="100%"]
	(selector7 '(:attrs ((width))))
	(selector8 '(:attrs ((width . "100%"))))
	;; tag with attribute w/ or w/o value
	(selector9 '(:tag table :attrs ((width . "100%"))))
	(selector10 '(:tag table :attrs ((width))))
	;; tag with multiple classes
	(selector11 '(:tag div :attrs ((class . "box"))))
	(selector19 '(:tag div :attrs ((class . "bottombox"))))
	(selector12 '(:tag div :attrs ((class . "box bottombox"))))
	(selector18 '(:tag div :attrs ((class . "bottombox box"))))
	;; unspecified class or id
	(selector13 '(:attrs ((class))))
	(selector14 '(:attrs ((id))))
	;; class or id only
	(selector15 '(:attrs ((id . "wrap"))))
	(selector16 '(:attrs ((class . "sep"))))
	;; multiple classes without tag
	(selector17 '(:attrs ((class . "sep current_left"))))
	(selector20 '(:attrs ((class . "current_left sep")))))

    (should (= (length (org-dict--dom-simple-select dom selector1)) 93))
    (should (= (length (org-dict--dom-simple-select dom selector2)) 429))
    (should (= (length (org-dict--dom-simple-select dom selector3)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector4)) 37))
    (should (= (length (org-dict--dom-simple-select dom selector5)) 43))
    (should (= (length (org-dict--dom-simple-select dom selector6)) 43))
    (should (= (length (org-dict--dom-simple-select dom selector7)) 15))
    (should (= (length (org-dict--dom-simple-select dom selector8)) 4))
    (should (= (length (org-dict--dom-simple-select dom selector9)) 3))
    (should (= (length (org-dict--dom-simple-select dom selector10)) 3))
    (should (= (length (org-dict--dom-simple-select dom selector11)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector12)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector13)) 534))
    (should (= (length (org-dict--dom-simple-select dom selector14)) 22))
    (should (= (length (org-dict--dom-simple-select dom selector15)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector16)) 9))
    (should (= (length (org-dict--dom-simple-select dom selector17)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector18)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector19)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector20)) 1))
    (should (= (length (org-dict--dom-simple-select dom selector21)) 13))
    (should (= (length (org-dict--dom-simple-select
			(org-dict--dom-simple-select dom selector1)
			selector5))
	       43))))

(ert-deftest org-dict-test--dom-p ()
  (should (org-dict--dom-p org-dict-test--cntrl-dom))
  (should (org-dict--dom-p org-dict-test--dummy-dom))
  (should-not (org-dict--dom-p (list org-dict-test--dummy-node1 org-dict-test--dummy-node2))))

(ert-deftest org-dict-test--select-child ()
  "Test child combinator + div hell 'div (> div)*' etc."
  (let* ((dom org-dict-test--cntrl-dom)
	 (div-nodes (org-dict--dom-simple-select dom '(:tag div)))
	 (td-nodes (org-dict--dom-simple-select dom '(:tag td)))
	 (div-lexicontent-nodes (org-dict--select-child dom div-nodes '(:tag div :attrs ((id . "lexicontent")))))
	 (div-lexicontent-nodes2 (org-dict--select-child dom div-nodes '(:attrs ((id . "lexicontent")))))
	 (div-nodes2 (org-dict--select-child dom div-nodes '(:tag div)))
	 (div-nodes3 (org-dict--select-child dom div-nodes2 '(:tag div)))
	 (div-nodes4 (org-dict--select-child dom div-nodes3 '(:tag div)))
	 (div-nodes5 (org-dict--select-child dom div-nodes4 '(:tag div)))
	 (div-nodes6 (org-dict--select-child dom div-nodes5 '(:tag div)))
	 (div-nodes7 (org-dict--select-child dom div-nodes6 '(:tag div)))
	 (div-nodes8 (org-dict--select-child dom div-nodes7 '(:tag div)))
	 (div-nodes9 (org-dict--select-child dom div-nodes8 '(:tag div)))
	 (div-nodes10 (org-dict--select-child dom div-nodes9 '(:tag div)))
	 (div-nodes11 (org-dict--select-child dom div-nodes10 '(:tag div))))
    (should (= (length (org-dict--select-child dom td-nodes '(:tag div ((class . "box bottombox")))))))
    (should (= (length div-lexicontent-nodes) 1))
    (should (= (length div-lexicontent-nodes2) 1))
    (should (= (length div-nodes2) 87))
    (should (= (length div-nodes3) 78))
    (should (= (length div-nodes4) 73))
    (should (= (length div-nodes5) 64))
    (should (= (length div-nodes6) 57))
    (should (= (length div-nodes7) 45))
    (should (= (length div-nodes8) 25))
    (should (= (length div-nodes9) 5))
    (should (= (length div-nodes10) 1))
    (should (= (length div-nodes11) 0))
    (should (= (length (org-dict--select-child dom div-nodes '(:tag span))) 249))
    (should (= (length (org-dict--select-child dom div-nodes '(:tag span :attrs ((class . "tlf_cexemple"))))) 43))))

(ert-deftest org-dict-test--select-subsequent-sibling ()
  "Test general/adjacent sibling combinator (+ and ~)"
  (let* ((dom org-dict-test--cntrl-dom)
	 (div-nodes (org-dict--dom-simple-select dom '(:tag div)))
	 (span-nodes (org-dict--dom-simple-select dom '(:tag span)))
	 (td-nodes (org-dict--dom-simple-select dom '(:tag td)))
	 (selector1 '(:tag span))
	 (selector2 '(:attrs ((class))))
	 (selector3 '(:tag div :attrs ((class . "tlf_parah"))))
	 (selector4 '(:tag td))
	 (selector5 '(:tag div))
	 (td-plus-nodes2 (org-dict--select-subsequent-sibling dom td-nodes selector4 t))
	 (td-plus-nodes3 (org-dict--select-subsequent-sibling dom td-plus-nodes2 selector4 t))
	 (div-plus-nodes2 (org-dict--select-subsequent-sibling dom div-nodes selector5 t))
	 (div-plus-nodes3 (org-dict--select-subsequent-sibling dom div-plus-nodes2 selector5 t))
	 (div-plus-nodes4 (org-dict--select-subsequent-sibling dom div-plus-nodes3 selector5 t))
	 (div-plus-nodes5 (org-dict--select-subsequent-sibling dom div-plus-nodes4 selector5 t))
	 (div-plus-nodes6 (org-dict--select-subsequent-sibling dom div-plus-nodes5 selector5 t))
	 (div-plus-nodes7 (org-dict--select-subsequent-sibling dom div-plus-nodes6 selector5 t))
	 (div-tilde-nodes2 (org-dict--select-subsequent-sibling dom div-nodes selector5))
	 (div-tilde-nodes3 (org-dict--select-subsequent-sibling dom div-tilde-nodes2 selector5))
	 (div-tilde-nodes4 (org-dict--select-subsequent-sibling dom div-tilde-nodes3 selector5))
	 (div-tilde-nodes5 (org-dict--select-subsequent-sibling dom div-tilde-nodes4 selector5))
	 (div-tilde-nodes6 (org-dict--select-subsequent-sibling dom div-tilde-nodes5 selector5))
	 (div-tilde-nodes7 (org-dict--select-subsequent-sibling dom div-tilde-nodes6 selector5))
	 (div-tilde-nodes8 (org-dict--select-subsequent-sibling dom div-tilde-nodes7 selector5))
	 (div-tilde-nodes9 (org-dict--select-subsequent-sibling dom div-tilde-nodes8 selector5))
	 )
    ;; Adjacent sibling combinator (+)
    ;; div + span
    (should (= (length (org-dict--select-subsequent-sibling dom div-nodes selector1 t)) 3))
    ;; div + [class]
    (should (= (length (org-dict--select-subsequent-sibling dom div-nodes selector2 t)) 49))
    ;; span + [class]
    (should (= (length (org-dict--select-subsequent-sibling dom span-nodes selector2 t)) 249))
    ;; span + div.tlf_parah
    (should (= (length (org-dict--select-subsequent-sibling dom span-nodes selector3 t)) 10))
    ;; td + td and td + td + td
    (should (= (length td-plus-nodes2) 24))
    (should (= (length td-plus-nodes3) 8))
    ;; div hell
    (should (= (length div-plus-nodes2) 53))
    (should (= (length div-plus-nodes3) 26))
    (should (= (length div-plus-nodes4) 14))
    (should (= (length div-plus-nodes5) 5))
    (should (= (length div-plus-nodes6) 2))
    (should (= (length div-plus-nodes7) 1))
    ;; General sibling combinator (~)
    ;; div ~ span 
    (should (= (length (org-dict--select-subsequent-sibling dom div-nodes selector1)) 3))
    ;; div ~ [class]
    (should (= (length (org-dict--select-subsequent-sibling dom div-nodes selector2)) 51))
    ;; span ~ [class]
    (should (= (length (org-dict--select-subsequent-sibling dom span-nodes selector2)) 339))
    ;; span ~ div.tlf_parah
    (should (= (length (org-dict--select-subsequent-sibling dom span-nodes selector3)) 27))
    ;; div hell
    (should (= (length div-tilde-nodes2) 55))
    (should (= (length div-tilde-nodes3) 29))
    (should (= (length div-tilde-nodes4) 17))
    (should (= (length div-tilde-nodes5) 8))
    (should (= (length div-tilde-nodes6) 4))
    (should (= (length div-tilde-nodes7) 3))
    (should (= (length div-tilde-nodes8) 2))
    (should (= (length div-tilde-nodes9) 1))))

(ert-deftest org-dict-test--dom-select ()
  (let* ((dom org-dict-test--cntrl-dom)
	 (selector1 '((:tag div)
		      (> (:tag div :attrs ((class . "font_change"))))
		      (+ (:attrs ((id))))))
	 (nodes1 (org-dict--dom-select dom selector1))
	 (selector2 '((:tag ul)
		      (=> (:tag a :attrs ((class))))))
	 (nodes2 (org-dict--dom-select dom selector2))
	 (selector3 '((:tag div :attrs ((id)))))
	 (nodes3 (org-dict--dom-select dom selector3))
	 )

    (should (= (length nodes1) 1))
    (should (eq (caar nodes1) 'form))
    (should (= (length nodes2) 1))
    (should (eq (caar nodes2) 'a))
    (should (= (length nodes3) 13))

    ))
(ert-deftest org-dict-test--replace-node-in-dom ()
    (ert-skip "Not implemented yet"))

(ert "org-dict-test--")

