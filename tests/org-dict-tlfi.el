(require 'org-dict-tlfi)


(setq org-dict-tlfi--surround-regexp "^[[:space:]\n]*\\(\\)\\(.*?\\)\\([,: ]*\\)?$")


(ert-deftest org-dict-tlfi-test--surround-regex ()
  ""
  (let ((str1 "
                           Œil, branche, grille de l'étrier.")
	(expect1 "/Œil, branche, grille de l'étrier./")
	(str2 "Ac.")
	(expect2 "/Ac./")
	(str3 "Sous tilleuls,")
	(expect3 "/Sous tilleuls/,")
	(str4 "SYNT. ")
	(expect4 "/SYNT./ ")
	(str5 "Prononc. et Orth. :")
	(expect5 "/Prononc. et Orth./ :"))
    
    (should (string= (replace-regexp-in-string org-dict-tlfi--surround-regexp "\\1/\\2/\\3" str1) expect1))
    (should (string= (replace-regexp-in-string org-dict-tlfi--surround-regexp "\\1/\\2/\\3" str2) expect2))
    (should (string= (replace-regexp-in-string org-dict-tlfi--surround-regexp "\\1/\\2/\\3" str3) expect3))
    (should (string= (replace-regexp-in-string org-dict-tlfi--surround-regexp "\\1/\\2/\\3" str4) expect4))
    (should (string= (replace-regexp-in-string org-dict-tlfi--surround-regexp "\\1/\\2/\\3" str5) expect5))))

(ert "org-dict-tlfi-test--")
