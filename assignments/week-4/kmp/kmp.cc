#include <cstdio>
#include <iostream>
#include <string>
#include <vector>

using std::cin;
using std::string;
using std::vector;

#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif

using namespace std;

void print_vector(string label, vector<int> v) {
  if(!debug) 
    return;
  cerr<<label<<":";
  for(int i : v)
    cerr<<i<<" ";
  cerr<<endl;
}

/**
 * Computes lenghts of maximally overlapping suffix and prefixes
 * of pattern at each position inside the pattern.
 */
vector<int> compute_prefixes(const string & pattern) {

  if(debug)
    std::cerr<<"compute_prefixes"<<std::endl;

  vector<int> prefix_array(pattern.size(),0);

  prefix_array[0] = 0;
  int border = 0; // length of curent border

  for(int i = 1; i< pattern.size() ; i++){
    if(debug)
      std::cerr<<"i["<<i<<"]"<<std::endl;

    /**
     * while non-matching end, consider predecessor's border length
     */
    while(border > 0 && pattern[i] != pattern[border]) 
      border = prefix_array[border-1];

    border = pattern[i] == pattern[border] ? ( border + 1) : 0;
    
    prefix_array[i] = border;    
  }

  return prefix_array;
}

/**
 * Find all occurrences of the pattern in the text and return a
 * vector with all positions in the text (starting from 0) where 
 * the pattern starts in the text.
 */
vector<int> find_pattern(const string& pattern, const string& text) {
  vector<int> result;
  if(debug)
    std::cout<<pattern.size()<<std::endl;
  string  pat_txt = pattern+"$"+text;
  vector<int> prefix_array  = compute_prefixes(pat_txt);
  print_vector("prefix_array",prefix_array);

  for(int i = 0; i < pat_txt.size() ; i++) {
    if(prefix_array[i] == pattern.size()) {
      if(debug)
        cout<<"found@:"<<i<<endl;

      result.push_back(i -  (2*pattern.size()));
    }
  }

  return result;
}


int main() {
  string pattern, text;
  cin >> pattern;  
  cin >> text;
  vector<int> result = find_pattern(pattern, text);
  for (int i = 0; i < result.size(); ++i) {
    printf("%d ", result[i]);
  }
  printf("\n");
  return 0;
}
