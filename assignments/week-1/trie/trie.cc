#include <string>
#include <iostream>
#include <vector>
#include <map>

using std::map;
using std::vector;
using std::string;

#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif


// char : Label for the edge , int: Node number
typedef map<char, int> edges;

// root: Of a trie which contains a bunch of edges.
typedef vector<edges> trie;

trie build_trie(vector<string> & patterns) {

  trie pattern_trie;
  edges root_node;

  pattern_trie.push_back(root_node);

  for(int i = 0; i < patterns.size(); i++ )  {
    int cur_node = 0;

    if(debug){
      std::cerr<<"-------------------------------------------"<<std::endl;
      std::cerr<<"adding-pattern:["<<patterns[i]<<"]"<<std::endl;
    }

    for(int j = 0; j < patterns[i].size() ;j++) {

      char cur_char = patterns[i][j];

      if(debug)
        std::cerr<<"current-char:["<<cur_char<<"]"<<std::endl;

      bool found = false;

      //Go through all the edges.
      edges node_edges = pattern_trie[cur_node];

      std::map<char,int>::iterator entry = node_edges.find(cur_char);

      if(entry != node_edges.end()) {

        if(debug)
          std::cerr<<"found-edge:["<<cur_char<<"]:"<<entry->first<<","<<entry->second<<std::endl;

        cur_node = entry->second;
        found = true;

      } else  { // add a new edge
        int node_number = pattern_trie.size();

        if(debug)
          std::cerr<<"adding-edge:["<<cur_char<<","<<node_number<<"]"<<std::endl;

        edges new_node;

        // leaf node : empty edge set
        pattern_trie.push_back(new_node);
        pattern_trie[cur_node][cur_char] = node_number;
        cur_node = node_number;
      }
    }
    if(debug)
      std::cerr<<"-------------------------------------------"<<std::endl;
  }
  return pattern_trie;
}

int main() {
  size_t n;
  std::cin >> n;
  vector<string> patterns;
  for (size_t i = 0; i < n; i++) {
    string s;
    std::cin >> s;
    patterns.push_back(s);
  }

  trie t = build_trie(patterns);
  for (size_t i = 0; i < t.size(); ++i) {
    for (const auto & j : t[i]) {
      std::cout << i << "->" << j.second << ":" << j.first << "\n";
    }
  }

  return 0;
}
