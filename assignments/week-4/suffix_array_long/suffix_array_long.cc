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

vector<char> alphabet = {'$','A','C','G','T'};

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
 * Creates single character long equivance classes, each letter gets
 * same equivalence class.
 */
vector<int> char_classes(const string & text, vector<int> & order)
{
  int n = text.size();
  vector<int> classes(n,0);

  for(int i = 1; i < n; i++) {
    classes[order[i]] = classes[order[i-1]];
    // if we are *not* the same equivalence class increment class
    // label
    if(text[order[i-1]] != text[order[i]])
      classes[order[i]]++;
  }
  return classes;
}

/**
 * order  - input is going to be sorted cyclic lists as represented by order
 *          C_[order[0]],C_[order[1]], C_[order[2]],...,C[order[n-1]] are in
 *          sorted order, sorted as cycic suffixes of length cycle_length
 *
 *
 */
vector<int> sort_doubled(const string& text, vector<int> & order,
                         vector<int> & classes, int cycle_length) {

  int n = text.size();
  vector<int> count(n,0);
  vector<int> new_order(n,0);

  // Counting sort by last half of the array

  // 1. Count frequency of each class
  for(int i = 0; i < n; i++) {
    count[classes[i]]++;
  }

  //print_vector("sort-doubed: counts",count);

  // 2. Compute partial sums of class counts
  for(int i = 1; i < n; i++) {
    count[i] += count[i-1];
  }

  //print_vector("sort-doubed: partial sums ",count);

  // 3.
  for(int i = text.size()-1; i >= 0; i--) {

    int start = ( order[i] - cycle_length + n ) % n;

    //cout<<"start : "<<start<<endl;
    int equivalence_class = classes[start];

    // take a instance of the class
    count[equivalence_class]--;

    // assign order to equivalent class
    //cout<<"count["<<equivalence_class<<"]:"<<count[equivalence_class]<<endl;
    new_order[count[equivalence_class]] = start;

    //print_vector("new_order:",new_order);
  }

  return new_order;
}

/**
 * Given a sorted of orders , re-compute their equivalence classes for
 * the new cycle_length. We only need to compare the ends of the
 * equivalence classes to check if they lie in the same class.
 */
vector<int> update_classes(vector<int> & new_order,
                           vector<int> & classes,
                           int cycle_length,int text_length)
{

  vector<int> new_classes(classes.size(),0);
  int n = text_length;

  int current_class = 0;
  print_vector("new_order",new_order);
  print_vector("classes",classes);
  if(debug)
    std::cerr<<"Updating:[";

  for(int i = 1; i < new_order.size(); i++ ) {
    if(debug)
      std::cerr<<"("<<(i-1-cycle_length+n)%n<<","<<i-1<<") ";

    pair<int,int> prev(classes[(i-1-cycle_length+n)%n],classes[i-1]);
    pair<int,int> cur(classes[(i-cycle_length+n)%n] ,classes[i]);

    int old_class = current_class;
    std::cout<<"\ncompare:("<<prev.first<<","<<prev.second<<") ";
    std::cout<<"("<<cur.first<<","<<cur.second<<") "<<endl;
    new_classes[i] =  ( prev != cur ) ? ++current_class : current_class;

    std::cout<<old_class<<"->" <<current_class<<std::endl;

  }

  if(debug)
    std::cerr<<"]"<<endl;

  return new_classes;
}

void print_cycles(const string &text,vector<int> & order,
                  vector<int> & classes,
                  int cycle_length) {
  if(!debug)
    return;
  int  c = 0;

  for(int cur : order) {
    printf("%-5s (%3d,%3d,%3d):[","suffix",cur,classes[cur],cycle_length);

    for(int i = 0; i < cycle_length; i++) {
      int j = (cur + i) % text.size();
      cerr<<text[j];
    }
    cerr<<"]"<<endl;
  }
}
/**
 * Build suffix array of the string text and return a vector result of
 * the same length as the text such that the value result[i] is the
 * index (0-based) in text where the i-th lexicographically smallest
 * suffix of text starts. Implement this function yourself.
 */
vector<int> build_suffix_array(const string& text,vector<char> alphabet)
{

  vector<int> order = sort_characters(text,alphabet);
  vector<int> classes = char_classes(text, order);

  int cycle_length = 1;
  int j = 0;

  while(cycle_length  < text.size()) {
    printf("--[%2d]=-------------------------------------------\n",j++);

    std::cout<<text<<std::endl;
    print_vector("order",order);
    print_vector("classes",classes);
    print_cycles(text,order,classes,cycle_length); //text.size());

    printf("--------------------------------------------------\n");
    vector<int> new_order = sort_doubled(text,order,classes,cycle_length);
    update_classes(new_order,classes,cycle_length,text.size());
    order = new_order;
    cycle_length *= 2;
  }

  return order;
}

void print_by_order(string & text ,vector<int> & order) {
  for(int i = 0 ; i < order.size(); i++){
    cout<<text[order[i]];
  }
  std::cout<<endl;
}

void test_sort_characters() {
  vector<int> order;
  vector<int> classes;

  {
    string text("ACTGAACAA$");
    std::cerr<<text<<endl;
    vector<int> order = sort_characters(text,alphabet);
    print_by_order(text,order);
    vector<int> classes = char_classes(text,order);
    print_vector("classes" , classes);
  }
  /*
  {
    string text("ACTTTGGGTTTGAACAA$");
    std::cerr<<text<<endl;
    order = sort_characters(text,alphabet);
    vector<int> order = build_suffix_array(text,alphabet);
    print_cycles(text,order,text.size());
  }
  */

  {

    alphabet = {'$','a','b'} ;
    string text("ababaa$");
    std::cerr<<text<<endl;
    order = sort_characters(text,alphabet);
    vector<int> order = build_suffix_array(text,alphabet);
    //    print_cycles(text,order,text.size());
  }


}


int main() {
  if(debug)
    test_sort_characters();
  /*
  string text;
  cin >> text;
  vector<int> suffix_array = build_suffix_array(text);
  for (int i = 0; i < suffix_array.size(); ++i) {
    cout << suffix_array[i] << ' ';
  }
  cout << endl;
  */
  return 0;
}
