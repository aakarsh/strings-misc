(defun kmp/prefix-vector(s)
  (let ((prefix-array (make-vector (length s ) 0 ))
        (border 0))
    (loop for i from 1 to (-  (length s) 1) do
          (while (and (> border 0) (not (equal  (aref s i) (aref s border))))
            (setq border (aref prefix-array (- border 1))))          
          (if (equal (aref s i)  (aref s border))
              (incf border)
            (setq border 0))
          (aset prefix-array  i border))    
    prefix-array))

(defun kmp/match (pat text &optional delim)
  (let* ((delim  (if ( not delim) ?\u001a  delim))
         (p (format "%s%s%s" pat delim text))
         (v  (kmp/prefix-vector p)))
    (loop for i from (+ 1 (length pat)) to (-  (length p) 1) 
          when (equal  (aref v i) (length pat))
          collect (- i (* 2 (length pat))))))


(defun kmp/find (pattern)
  (interactive "sFind exact string:")
  (set (make-local-variable 'kmp-index) 0)
  (set (make-local-variable 'kmp-match-positions) '())
  ;; inefficient creating new string 
  (setq kmp-match-positions  (kmp/match pattern (buffer-substring-no-properties (point-min) (point-max))))
  (if (>= kmp-index (length kmp-match-positions))
      (message "No results for %s!" pattern)
    (goto-char (car kmp-match-positions))))


(defun kmp/find-next ()
  (interactive)
  (incf kmp-index)
  (if (>= kmp-index (length kmp-match-positions))
      (message "Finished Search!")
    (goto-char (nth kmp-index kmp-match-positions  ))))



