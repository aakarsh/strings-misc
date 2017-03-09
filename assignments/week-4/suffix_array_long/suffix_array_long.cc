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

  if(debug)
    print_vector("frequencies",count);

  // compute partial sum of each alphabet
  for(int j = 1; j < alphabet.size(); j++) {
    count[j] += count[j-1];
  }

  if(debug)
    print_vector("partials",count);

  // start from the end pull out alphabet and decrement its count give
  // the appropriate value of the order

  for(int i = n-1 ; i >= 0; i--) {
    int k  = key(text[i]);
    count[k]--;
    order[count[k]] = i;
  }
  
  if(debug)
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
 * Extracts cycle of length cycle_length from string , starting at
 * position start.
 */
string text_part(const string& text, int start, int cycle_length)
{
  string part("");
  int n  = text.size();
  int cur = start;
  for(int i = 0; i < cycle_length; i++) {
    cur = cur % n;
    part.append( &text[cur] , 1);
    cur++;
  }
  return part;
}

void print_ordering(const string& text, vector<int>order, int cycle_length)
{
  if(!debug) return;
  int i = 0;
  for(int start : order) {
    string part("-");
    if(start >= 0)
      part = text_part(text,start,cycle_length);
    fprintf(stderr,"(%2d,%2d) [%8s]\n",start,(start+cycle_length)%text.size(),part.c_str());
    i++;
  }
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
  vector<int> new_order(n,-1);

  if(debug)
    print_vector("old-order:", order);

  // Counting sort by last half of the array

  // 1. Count frequency of each class
  for(int i = 0; i < n; i++) count[classes[i]]++;

  print_vector("sort-doubed: counts",count);

  // 2. Compute partial sums of class counts
  for(int i = 1; i < n; i++) count[i] += count[i-1];

  print_vector("sort-doubed: partial-sums", count);
  print_vector("sort-doubed: classes"     , classes);


  
  // 3.
  for(int i = text.size() - 1; i >= 0; i--) {

    // The previous cycle will start from position cycle_length -1
    // end at order[i]
    int start = (order[i] - cycle_length + 1 + n  ) % n;
    int eq_cls = classes[start];

    // take a instance of the class
    count[eq_cls]--;

    if(debug) {
   
    fprintf(stderr,"%-10s\n",text.c_str());
    fprintf(stderr,"+-----------------------------------------------+\n");
    fprintf(stderr,"|%2s |%2s |%2s |%c | %-10s |%-5s |%-5s |%-5s|\n","i","len","ord",'l',"suffix","start", "class","count");
    fprintf(stderr,"+----------------------------------------------+\n");

      // we are taking a character from previous cycle's equivalence class
      fprintf(stderr,"|%2d |%2d |%2d |%c | %-10s |%-5d |%-5d |%-5d|\n"
              ,i,cycle_length,order[i], text[order[i]],
              text_part(text,start,cycle_length).c_str(),
              start,eq_cls,count[eq_cls]);
      fprintf(stderr,"+----------------------------------------------+\n");
    }

    new_order[count[eq_cls]] = start;

    if(debug) {
      fprintf(stderr,"start : %d\n",start);
      cerr<<"text : "<<text<<endl;
      print_vector("old-order", order);
      print_vector("new_order",new_order);
      print_vector("partial_sums",count);
      print_ordering(text,new_order,cycle_length);
    }
  }

  if(debug)
    fprintf(stderr,"+-------------------------------+\n");

  //  print_vector("new_order:",new_order);
  print_ordering(text,new_order,cycle_length);

  return new_order;
}

/**
 * Given a sorted of orders , re-compute their equivalence classes for
 * the new cycle_length. We only need to compare the ends of the
 * equivalence classes to check if they lie in the same class.
 */
vector<int> update_classes(const string & text,
                           vector<int> & order,
                           vector<int> & classes,
                           int cycle_length,int text_length)
{

  vector<int> new_classes(classes.size(),-1);
  int n = text_length;

  int current_class = 0;

  print_vector("update_classes:order",order);
  print_vector("update_classes:classes",classes);

  if(debug)
    std::cerr<<"Updating:[";

  new_classes[order[0]] = current_class;

  for(int i = 1; i < order.size(); i++ ) {

    // seems to fix class assignment for GA
    int prev_start   = order[i-1]; 
    int prev_mid     = (order[i-1] + cycle_length -1) % text_length;

    int cur_start = order[i]; 
    int cur_mid   = (order[i] + cycle_length -1 ) % text_length; 

    pair<int,int> prev(classes[prev_start], classes[prev_mid]);
    pair<int,int> cur (classes[cur_start] , classes[cur_mid]);

    int old_class = current_class;

    if(debug) {
      std::cerr<<text<<endl;
      std::cerr<<"{ ("<< prev.first <<","<< prev.second <<")->";
      std::cerr<<"("<< cur.first <<","<< cur.second <<") } ";
      std::cerr<<endl;
    }

    if(prev!=cur) {
      new_classes[order[i]] = ++current_class;
    } else {
      new_classes[order[i]] = current_class;
    }
    print_vector("new_classes",new_classes);
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
  for(auto cur : order) {    
    fprintf(stderr,"%-5s (%3d,%3d,%3d):[%s]\n","suffix",cur,classes[cur],cycle_length,
           text_part(text,cur,cycle_length).c_str());

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

  while(true) {
    if(debug) {
      fprintf(stderr,"--[%2d,%3d]=-------------------------------------------\n",j++,cycle_length);
      std::cerr<<text<<std::endl;
      print_vector("order",order);
      print_vector("classes",classes);
      print_cycles(text,order,classes,cycle_length);
      fprintf(stderr,"--------------------------------------------------\n");
    }
    cycle_length *= 2;
    if(cycle_length >= text.size())
      break;

    vector<int> new_order = sort_doubled(text,order,classes,cycle_length);
    classes = update_classes(text,new_order,classes,cycle_length,text.size());
    order = new_order;

  }

  return order;
}

void print_by_order(string & text ,vector<int> & order) {
  if(debug)
    return;
  for(int i = 0 ; i < order.size(); i++){
    if(order[i] == -1)
      cerr<<"*";
    else
      cerr<<text[order[i]];
  }
  std::cerr<<endl;
}

void test_sort_characters() {
  vector<int> order;
  vector<int> classes;

  /*
  {
    string text("ACTGAACAA$");
    std::cerr<<text<<endl;
    vector<int> order = sort_characters(text,alphabet);
    print_by_order(text,order);
    vector<int> classes = char_classes(text,order);
    print_vector("classes-" , classes);
  }

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
    if(debug)
      std::cerr<<text<<endl;
    order = sort_characters(text,alphabet);
    vector<int> order = build_suffix_array(text,alphabet);
    
    //print_cycles(text,order,text.size());
  }


}

int main() {
  if(false)
    test_sort_characters();

  string text;
  cin >> text;
  vector<int> suffix_array = build_suffix_array(text,alphabet);
  for (int i = 0; i < suffix_array.size(); ++i) {
    cout << suffix_array[i] << ' ';
  }
  cout << endl;

  return 0;
}
