(setq an/testsuites
      (cons 
       (make-an/testsuite
        :name "trie"
        :dir "/home/aakarsh/src/c++/coursera/strings/assignments/week-1/trie"
        :testcases
        (list
         (make-an/testcase  
          :name "simple-1"
          :lines '(
                   "A$")
          :cmd "trie"
          :ans "")

         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "ACA$")    
          :cmd "trie"
          :ans "")


         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "ATAAATG$")    
          :cmd "trie"
          :ans "")
))
       an/testsuites))

