(defun compress (X)
    (if (consp x)
    (compr (car x) 1 (cdr x))
    x))

(defun compr (elt n lst)
    (if (null lst)
        (list ( n-elts elt n))
        (let ((next (car lst)))
            (if (eql next elt)
                (compr elt (+ n 1) (cdr lst))
                (cons ( n-elts elt n)
                    (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
    (if (> n 1)
        (list n elt)
        elt))

(defun list-of (n elt)
    (if (zerop n)
        nil
        (cons elt (list-of (- n 1) elt))))

(defun decompress (lst)
    (if (null lst)
        nil
        (let ((elt (car lst))
            (rest (decompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                rest)
            (cons elt rest)))))



(defconstant lt (list 1 1 1 1 1 0 2 3 3 4 4 4 4 1 0 19 19 3 1 3 3 33))

(compress lt) 
(decompress (compress lt)) 