#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <utility>

using std::cin;
using std::cout;
using std::endl;
using std::make_pair;
using std::pair;
using std::string;
using std::vector;

using namespace std;

#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif

vector<char> alphabet {'$','A','C','G','T'};

// Need a constant time operation
inline int key(char c) {
  auto beg = alphabet.begin();
  auto end = alphabet.end();
  return distance(beg ,find(beg,end,c));
}

void print_vector(string label, vector<int> v) {
  if(!debug) 
    return;
  cerr<<label<<":";
  for(int i : v)
    cerr<<i<<" ";
  cerr<<endl;
}

/**
 * Given a set of characters return an ordering for the characters
 * over the text.
 */
vector<int> sort_characters(const string& text, vector<char>& alphabet) {

  int n  = text.size();
  vector<int> order(n,0);
  vector<int> count(alphabet.size(),0);

  // count frequency of each alphabet
  for(int i = 0; i < n; i++) {
    count[key(text[i])]++;
  }

  print_vector("frequencies",count);
  
  // compute partial sum of each alphabet
  for(int j = 1; j < alphabet.size(); j++) {
    count[j] += count[j-1];
  }
  
  print_vector("partials",count);

  // start from the end pull out alphabet and decrement its count give
  // the appropriate value of the order

  for(int i = n-1 ; i >= 0; i--) {
    int k  = key(text[i]);    
    count[k]--;
    order[count[k]] = i;
  }
  print_vector("order",order);
  return order;
}

/**
 * Build suffix array of the string text and return a vector result of
 * the same length as the text such that the value result[i] is the
 * index (0-based) in text where the i-th lexicographically smallest
 * suffix of text starts.
 */
vector<int> BuildSuffixArray(const string& text) {
  vector<int> result;
  vector<int> order = sort_characters(text,alphabet);
  // Implement this function yourself
  
  return result;
}

void print_by_order(string & text ,vector<int> & order) {
  for(int i = 0 ; i < order.size(); i++){
    cout<<text[order[i]];    
  }
  std::cout<<endl;
}

void test_sort_characters() {
  string text("ACTGAACAA$");
  vector<int> order = sort_characters(text,alphabet);
  print_by_order(text,order);

  string text2("ACTTTGGGTTTGAACAA$");
  order = sort_characters(text2,alphabet);
  print_by_order(text2,order);
}

int main() {
  test_sort_characters();
  string text;
  cin >> text;
  vector<int> suffix_array = BuildSuffixArray(text);
  for (int i = 0; i < suffix_array.size(); ++i) {
    cout << suffix_array[i] << ' ';
  }
  cout << endl;
  return 0;
}
