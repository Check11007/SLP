
799;6. Определите функцию, переводящую список чисел в список соответствующих
;им названий.
(defun shift (list)
(cond
((null list) nil)
((equal (car list) 0) (cons 'ноль (shift (cdr list))))
((equal (car list) 1) (cons 'один (shift (cdr list))))
((equal (car list) 2) (cons 'два (shift (cdr list))))
((equal (car list) 3) (cons 'три (shift (cdr list))))
((equal (car list) 4) (cons 'четыре (shift (cdr list))))
((equal (car list) 5) (cons 'пять (shift (cdr list))))
((equal (car list) 6) (cons 'шесть (shift (cdr list))))
((equal (car list) 7) (cons 'семь (shift (cdr list))))
((equal (car list) 8) (cons 'восемь (shift (cdr list))))
((equal (car list) 9) (cons 'девять (shift (cdr list))))
(t (cons (car list) (shift (cdr list))))
)
)

;16. Определите функции, добавляющую элементы одного списка во второй список, начиная с заданной позиции.
; объединение
(defun append1 (x y)
    (cond 
        ((NULL x) y)
        (T 
            (cons (car x) (append1 (cdr x) y))
        )
    )
)
; l и m - 2 одинаковых списка 
; n1 - заданный номер
(defun ARRAY (l m n n1)
    (cond
        ((NULL l) Nil)
         ((= (car l) (funcall #'z1 m n1)) (append1 n (ARRAY (cdr l) m n n1)))
         
        (T
            (cons (car l) (ARRAY(cdr l) m n n1))
        )
    )
)
(ARRAY '(2 1 3 4 5 6 8 7 9 21 11 13 22) '(2 1 3 4 5 6 8 7 9 21 11 13 22) '(99 999 999 999 99) 4)
; (2 1 3 99 999 999 999 99 5 6 8 7 9 21 11 13 22)

;23. Определите функции, преобразующие список (a b с) к виду (а (b (с))) и
;наоборот.

(defun create-list-in-list (L)
    ( ( lambda (p rest)
               ( cond
                   ( (null rest) (list p) )
                   ( t (list p ( create-list-in-list rest ) ) )
               )   
        ) (car L) (cdr L)
     )
)

(defun create-list-of-list (L)
    ( ( lambda (p rest)
               ( cond
                   ( (null rest) (list p)  )
                   ( t (cons p ( create-list-of-list rest )  ) )
               )   
        ) (car L) (cadr L)
     )
)
(print "23. Определите функции, преобразующие список (a b с) к виду (а (b (с))) и наоборот.")
( print ( create-list-of-list '(a (b (c))) ) )
( print ( create-list-of-list '(A (B (C (D (E (F)))))) ) )
( print ( create-list-of-list '(a) ) )

( print ( create-list-in-list '(a b c) ) )
( print ( create-list-in-list '(a) ) )
( print ( create-list-in-list '(a b c d e f) ) )


;27. Определите функцию, которая, чередуя элементы списков (a b...) и (1 2...),
;образует новый список (a 1 b 2 ...).
(defun mixF (lst1 lst2 lstout)
    (cond
        (( null lst1) (complete lst2 lstout))
        (t (mixF lst2 (cdr lst1) (cons (car lst1) lstout)))
    )
)


(defun complete (lst lstout)
    (cond
        (( null lst) lstout)
        (t (complete (cdr lst) (cons (car lst1) lstout)))
    )
)   

(print ( rev (mixF '(1 2 3 4 5) '(a b c d e) () ) ()))

;31. Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый
;элемент, входящий в оба списка х и у, в противном случае NIL
(defun is_similar (x lst)
(cond
((eql (car lst) x) x)
((null lst) NIL)
(t (is_similar x (cdr lst)))
)
(defun ПЕРВЫЙ-СОВПАДАЮЩИЙ (x y)
(cond
((null x) NIL)
((eql (is_similar (car x) y) NIL) (ПЕРВЫЙ-СОВПАДАЮЩИЙ (cdr x) y))
(t (car x))
)
)

(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(10 11 12 13) '(7 8 4 3 7)))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(9 5 7 2 3) '(2 8 4 3 7)))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '() '(2 8 4 3 7)))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(9 5 7 2 3) '()))
  
 
