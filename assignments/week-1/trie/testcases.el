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
                   "1\nATA")
          :cmd "trie"
          :ans "0->1:A\n0->2:T")

         (make-an/testcase  
          :name "simple-2"
          :lines '(
                   "3\nAT\nAG\nAC")    
          :cmd "trie"
          :ans "0->1:A\n0->2:T\n1->4:C\n1->3:G")


         (make-an/testcase  
          :name "simple-3"
          :lines '(
                   "3\nATAGA\nATC\nGAT")    
          :cmd "trie"
          :ans "0->1:A\n0->7:G\n0->2:T\n1->4:A\n1->6:C\n1->3:G\n1->5:T")
))
       an/testsuites))

