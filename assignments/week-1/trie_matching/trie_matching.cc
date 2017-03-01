#include <algorithm>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <map>


using std::map;
using std::vector;
using std::string;

using namespace std;

#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif

// char : Label for the edge , int: Node number
typedef map<char, int> edges;

// root: Of a trie which contains a bunch of edges.
typedef vector<edges> trie;

int const Letters =    4;
int const NA      =   -1;

struct Node {

  int next [Letters];

  Node() { fill(next, next + Letters, NA); }

  bool isLeaf () const {
    return (next[0] == NA &&
            next[1] == NA &&
            next[2] == NA &&
            next[3] == NA);
  }
};


int letterToIndex(char letter) {
  switch (letter){
  case 'A': return 0; break;
  case 'C': return 1; break;
  case 'G': return 2; break;
  case 'T': return 3; break;
  default: assert (false); return -1;
  }
}

vector<char> prefix_trie_match(int start_index, const string & text , trie & pat_trie) ;

/**
 * Builds a trie a set of patterns.
 *
 * Trie format - [{<edge-label>:<trie_position>,..},{..},..]
 *
 */
trie build_trie(const vector<string> & patterns) {

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

        if(debug) {
          std::cerr<<"adding-edge:["<< cur_char << "," << node_number <<"]"<<std::endl;
        }

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

  for(int i = 0; i < text.size(); i++ ){

    vector<char> matched_pat = prefix_trie_match(i,text,search_tree);
  
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
  int cur_symbol = text [text_pos++];
  vector<char> matched;

  while(true){

    // current node is empty
    if(pat_trie[current_node].size() == 0) {
      return matched;
    }

    edges edge_set = pat_trie[current_node];

    map<char,int>::iterator entry = edge_set.find(cur_symbol);
    if(entry != edge_set.end()) { // found - traverse
      char matched_char = entry->first;
      matched.push_back(matched_char);      
      current_node = entry->second;
      cur_symbol = text[text_pos++];
    } else { // no match found
      return vector<char>();
    }
  }
  return matched;
}





int main (void)
{
  string text;
  int n;

  cin >>text;
  cin >> n;

  vector <string> patterns(n);
  for (int i = 0; i < n; i++){
    cin>>patterns[i];
  }

  vector <int> ans;
  ans = solve(text,n,patterns);

  for (int i = 0; i < (int) ans.size (); i++) {
    cout << ans[i];
    if (i+1 < (int) ans.size ()) {
      cout << " ";
    }
    else {
      cout << endl;
    }
  }

  return 0;
}
