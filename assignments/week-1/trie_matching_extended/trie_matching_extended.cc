#include <algorithm>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <map>

using namespace std;

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

// edges along with fact that its a terminator
typedef pair<edges,int> node;

// root: Of a trie which contains a bunch of edges.
typedef vector<node> trie;

vector<char> prefix_trie_match(int start_index, const string & text , trie & pat_trie) ;
trie build_trie(const vector<string> & patterns);
vector <int> solve(const string& text, int n, const vector <string>& patterns);

/**
 * Builds a trie a set of patterns.
 *
 * Trie format - [{<edge-label>:<trie_position>,..},{..},..]
 *
 */
trie build_trie(const vector<string> & patterns) {
  trie pattern_trie;
  //  edges root_node;

  node root_node;

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
      node current_node = pattern_trie[cur_node];
      edges node_edges = current_node.first;

      std::map<char,int>::iterator entry = node_edges.find(cur_char);

      if(entry != node_edges.end()) {

        if(debug) {
          std::cerr<<"found-edge:["<< cur_char <<"]:"<< entry->first <<","<< entry->second <<std::endl;
        }
        cur_node = entry->second;
        found = true;

        if(j+1 == patterns[i].size()) {
          if(debug) {
            std::cerr<<"setting-terminal :"<< patterns[i]<<std::endl;
          }
          pattern_trie[cur_node].second = true;
        }        


      } else  { // add a new edge
        int node_number = pattern_trie.size();

        if(debug) {
          std::cerr<<"adding-edge:["<< cur_char << "," << node_number <<"]"<<std::endl;
        }

        //edges new_node;
        node new_node;

        // leaf node : empty edge set
        pattern_trie.push_back(new_node);
        cur_node = node_number;
        (pattern_trie[cur_node].first)[cur_char] = node_number;





        if(debug) {
          std::cerr<<"After adding edges "<<pattern_trie[cur_node].first.size()<<std::endl;
        }

      
    }
    if(debug)
      std::cerr<<"-------------------------------------------"<<std::endl;

  }
  return pattern_trie;
}


/**
 * Search for all occurances of patterns over the text
 *
 * text         -  text to match against
 * n            -  number of patterns to match
 * patterns     -  patterns to match against
 * @return      -  all starting positions which match the text
 */
vector <int> solve(const string& text, int n,
                   const vector <string>& patterns) {

  vector <int> result;
  trie search_tree = build_trie(patterns);

  for(int i = 0; i < text.size(); i++) {

    vector<char> matched_pat = prefix_trie_match(i,text,search_tree);

    if(debug)
      std::cerr<<"search_text_post["<<i<<"] match_size:"<<matched_pat.size()<<std::endl;

    if(debug && matched_pat.size() > 0) {
      std::cerr<<"Matched Pattern [";
      for(char c : matched_pat)
        std::cerr<<c;
      std::cerr<<"]"<<std::endl;
      std::cerr<<"index : "<<i<<std::endl;
      //matched position

    }
    if(matched_pat.size() > 0) {
      result.push_back(i);
    }
  }

  return result;
}


vector<char> prefix_trie_match(int start_index, const string & text , trie & pat_trie) {

  int current_node = 0;
  int text_pos = start_index;
  int cur_symbol = text[text_pos++];

  vector<char> matched;
  int iter_num  = 0;

  while(true) {

    node nd = pat_trie[current_node];

    // current node is empty
    if((nd.first).empty()) {

      if(debug)
        std::cerr<<"["<< iter_num <<"] "<<"" <<std::endl;

      return matched;
    }

    edges edge_set = nd.first;

    if(debug) {
      std::cerr<<"edge-set:"<<current_node<<" size "<< edge_set.size()<<std::endl;
      std::cerr<<"terminal-pattern-node:["<< nd.second<<"]"<<std::endl;
    }
    if(nd.second) { // terminal node;
      if(debug)
        std::cerr<<"Terminal Pattern Node : "<< nd.second<<std::endl;
      return matched;
    }
    
    map<char,int>::iterator entry = edge_set.find(cur_symbol);
    if(entry != edge_set.end()) { // found - traverse

      if(debug)
        std::cerr<<"traversing the edge"<<std::endl;

      char matched_char = entry->first;
      matched.push_back(matched_char);

      current_node = entry->second;
      cur_symbol = text[text_pos++];

    } else { // no match found

      if(debug && text_pos < text.size()) {
        std::cerr<<"["<<iter_num<<"] "<<" no-match  ["<<text[text_pos]<<"]" <<std::endl;
      }

      return vector<char>();
    }

    iter_num++;
  }

  return matched;
}


int main (void) {
  int n;
  string text;

  cin>>text;
  cin>>n;

  vector <string> patterns (n);
  for (int i = 0; i < n; i++) {
    cin >> patterns[i];
  }

  vector <int> ans;
  ans = solve (text, n, patterns);

  for (int i = 0; i < (int) ans.size (); i++) {
    cout << ans[i];
    if (i + 1 < (int) ans.size ()){
      cout << " ";
    }
    else {
      cout << endl;
    }
  }
  return 0;
}
