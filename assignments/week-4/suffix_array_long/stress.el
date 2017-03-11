(require 'cl-lib)

(defvar suffix/dir "/home/aakarsh/src/c++/coursera/strings/assignments/week-4/suffix_array_long")
(defvar suffix/c-file (format "%s/%s" suffix/dir "suffix_array_long.cc"))
(defvar suffix/bin-file (format "%s/bin/%s" suffix/dir "suffix_array_long"))
(defvar suffix/out-buf "suffix.txt")
(defvar suffix/default-input (format "%s/bin/%s" suffix/dir "fail.1" ) )

(defun suffix/rerun()
  (interactive)
  (with-current-buffer (get-buffer-create suffix/out-buf)
    (save-window-excursion
      (delete-region (point-min) (point-max))
      (insert "----------------------------------------------------\n")
      (insert (shell-command-to-string "date"))
      (insert "----------------------------------------------------\n")
      (insert (shell-command-to-string (format "cat %s | %s" suffix/default-input  suffix/bin-file))))
    (save-buffer))  
  (pop-to-buffer  (get-buffer-create suffix/out-buf))
  (goto-char (point-min)))

(defun suffix/run-cmd-input (in)
  (let ((out "")) 
    (with-temp-buffer
      (insert in)
      (shell-command-on-region (point-min) (point-max) suffix/bin-file (current-buffer) "*suffix:out*")
      (setq out (buffer-substring-no-properties (point-min) (- (point-max) 2) )))    
    out))

(defun suffix/generate-suffixes (string)
  "Generate a list of suffixes for string"
  (let* ((retval '())
        (last-char-pos (- (length string) 0))
        (pos last-char-pos)
        (cur ""))    
    (loop for i downfrom last-char-pos to 0  do
          (setq cur (substring string i last-char-pos) )
          (when (not ( string-empty-p cur))
            (decf pos)
            (setq retval  (cons  (list cur pos) retval) )))    
    (sort  retval
           (lambda (sp1 sp2)
             (string< (car sp1) (car sp2))))))

(defun suffix/suffix-array (string)
  (mapcar 'cadr (suffix/generate-suffixes string)))

(defun suffix/list= (l1 l2)
  (let ((retval t))
    (loop for i from 0 to (length l1) do
          (if (not (equal (nth i l1) (nth i l2)))              
              (setq retval nil)))
    retval))

(defun suffix/test (in)
  (let* ((result t)
         (l1  (mapcar 'string-to-number (split-string (suffix/run-cmd-input in))))
         (l2  (suffix/suffix-array in)))
    (when (not (suffix/list= l1 l2))
      (setq result nil)
      (message "Faiing for input :%s\nExpected:[%s] \nGot:[%s]" in l1 l2))
    result))

(defun suffix/test-all (inputs)
  (loop for input in  inputs do 
      (when (not (suffix/test input))
        (message "Failing on input:%s" input))))

(global-set-key (kbd "C-c t") 'suffix/rerun)

(defun suffix/stress (&optional num-iterations str-length)  
  (let ((passing t)
        (num-iterations (if (not num-iterations) 10000 num-iterations  ))
        (str-len (if (not str-length) 500  str-length))
        (n 0))    
    (while (and  passing (< n num-iterations ))
      (setq passing   (suffix/test (format  "%s$"  (random-string-random-string str-len))))
      (incf n))))

(defun suffix/stress-do ()
  (interactive)
  (suffix/test-all '("AACGATAGCGGTAGA$" "AAA$" "GAC$" "GAGAGAGA$" ))
  (suffix/stress 100 4)
  (message "stage 1")
  (suffix/stress 100 10)
  (message "stage 2")
  (suffix/stress 100 100)
  (message "stage 3")
  (suffix/stress 100 1000)
  (message "stage 4")
  (suffix/stress 20 10000)
  (message "Done"))

(suffix/stress-do)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from #emacs - didi
(defgroup random-string nil
  "Generate random passwords."
  :group 'applications
  :version "24.3.1")

(defcustom random-string-random-categories-list
  (list "abcdefghijklmnopqrstuvxwyz"
        "ABCDEFGHIJKLMNOPQRSTUVXWYZ"
        "01234567890"
        "!@#%&*()[]{},./<>;:"
        )
  "List of categories for random strings."
  :type '(repeat string)
  :group 'random-string)

(defcustom random-string-password-length 50
  "Length of the password."
  :type 'integer
  :group 'random-string)

(defcustom random-string-use-undo-p t
    "Non-nill means use the buffer's undo list.

Text inserted to the buffer is added to the buffer's undo list.
This variable can be used to prevent it."
    :type 'boolean
    :group 'random-string)

(defun random-string-random-string (n)
    "Generate a random string of lenght N from `random-categories-list'.

The generated string will have at least one of the characters in
each category and characters can appear more than once."
    (cl-labels ((random-elt (seq)
                            (elt seq (random (length seq))))
                (gen-random-pass (set)
                                 (apply #'string (cl-loop repeat n collect (random-elt set))))
                (validp (pass)
                        (cl-every (lambda (category)
                                    (cl-some (lambda (c)
                                               (let ((case-fold-search nil))
                                                 (cl-find c category
                                                          :test #'char-equal)))
                                             pass))
                                  random-string-random-categories-list)))
      (when (< n (length random-string-random-categories-list))
        (error "n is less than minimum %d: %d"
               (length random-string-random-categories-list) n))
      (cl-loop with random-set = (apply #'concat random-string-random-categories-list)
               for pass = (gen-random-pass random-set)
               when (validp pass) return pass)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
