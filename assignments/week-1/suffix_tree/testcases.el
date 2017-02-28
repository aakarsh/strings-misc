(setq an/testsuites
      (cons 
       (make-an/testsuite
        :name "suffix_tree"
        :dir "/home/aakarsh/src/c++/coursera/strings/assignments/week-1/suffix_tree"
        :testcases
        (list
         (make-an/testcase  
          :name "simple-1"
          :lines '(
                   "A$")
          :cmd "suffix_tree"
          :ans "")

         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "ACA$")    
          :cmd "suffix_tree"
          :ans "")


         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "ATAAATG$")    
          :cmd "suffix_tree"
          :ans "")
))
       an/testsuites))

