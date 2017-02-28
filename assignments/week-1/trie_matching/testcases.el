(setq an/testsuites
      (cons 
       (make-an/testsuite
        :name "trie_matching"
        :dir "/home/aakarsh/src/c++/coursera/strings/assignments/week-1/trie_matching"
        :testcases
        (list
         (make-an/testcase  
          :name "simple-1"
          :lines '(
                   "A$")
          :cmd "trie_matching"
          :ans "")

         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "ACA$")    
          :cmd "trie_matching"
          :ans "")


         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "ATAAATG$")    
          :cmd "trie_matching"
          :ans "")
))
       an/testsuites))

