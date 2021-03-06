#include <climits>
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

/* Debugging method defintions */
void print_ordering(const string& text, vector<long long>order, long long cycle_length);
void print_cycles(const string &text,vector<long long> & order,vector<long long> & classes,long long cycle_length);
void print_vector(string label, vector<long long> v);

/* Primitives*/
inline long long key(char c);

/**
 * Given a set of characters return an ordering for the characters
 * over the text.
 */
vector<long long> sort_by_chars(const string& text) {

  long long n  = text.size();
  long long max_size = 10000;

  vector<long long> order(n,0);
  vector<long long> count(max_size,0);

  // count frequency of each alphabet
  for(long long i = 0; i < n; i++) {
    count[key(text[i])]++;
  }

  #if DEBUG
  //prlong long_vector("frequencies",count);
  #endif
    
  // compute partial sum of each alphabet
  for(long long j = 1; j < max_size ; j++) {
    count[j] += count[j-1];
  }

  #if DEBUG
  //print_vector("partials",count);
  #endif

  // start from the end pull out alphabet and decrement its count give
  // the appropriate value of the order

  for(long long i = n-1 ; i >= 0; i--) {
    long long k  = key(text[i]);
    count[k]=count[k]-1;
    order[count[k]] = i;
  }

  #if DEBUG
  print_vector("order",order);
  #endif

  return order;
}

/**
 * Creates single character long equivance classes, each letter gets
 * same equivalence class.
 */
vector<long long> char_classes(const string & text, vector<long long> & order)
{
  long long n = text.size();
  vector<long long> classes(n,0);

  for(long long i = 1; i < n; i++) {

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
string text_part(const string& text, long long start, long long cycle_length) {
  string part("");
  long long n  = text.size();
  long long cur = start;
  for(long long i = 0; i < cycle_length; i++) {
    cur = cur % n;
    part.append( &text[cur] , 1);
    cur++;
  }
  return part;
}


/**
 * order  - input is going to be sorted cyclic lists as represented by order
 *          C_[order[0]],C_[order[1]], C_[order[2]],...,C[order[n-1]] are in
 *          sorted order, sorted as cycic suffixes of length cycle_length
 *
 *
 */
vector<long long> sort_doubled(const string& text, vector<long long> & order,
                         vector<long long>& classes, long long cycle_length) {

  long long n = text.size();
  vector<long long> count(n,0);
  vector<long long> new_order(n,-1);

  #if DEBUG
    fprintf(stderr,"--------------------------------------------------\n");
    std::cerr<<"entry:sort-doubled"<<endl;
    print_vector("old-order:", order);
    print_vector("old-classes:", classes);
    fprintf(stderr,"cycle-length: %d\n",cycle_length);
    fprintf(stderr,"--------------------------------------------------\n");
  #endif

  // Counting sort by last half of the array

  // 1. Count frequency of each class
  for(long long i = 0; i < n; i++)
    count[classes[i]]++;

  print_vector("sort-doubed: counts",count);

  // 2. Compute partial sums of class counts
  for(long long i = 1; i < n; i++)
    count[i] += count[i-1];

  print_vector("sort-doubed: partial-sums", count);
  print_vector("sort-doubed: classes"     , classes);

  // 3.
  for(long long i = text.size() - 1; i >= 0; i--) {

    long long suffix_start = order[i];

    // The previous cycle will start from position cycle_length -1
    // end at order[i]
    // No idea why i am just totally stuck
    long long prefix_start = (suffix_start - (cycle_length/2) + n   ) % n;

    /* Represents the equivalence class of prefix of current element  */
    long long prefix_class = classes[prefix_start];

    /* Reduce count of prefix equivalence class*/
    count[prefix_class]= count[prefix_class] - 1;

    #if DEBUG

      fprintf(stderr,"%-10s\n",text.c_str());
      fprintf(stderr,"+-----------------------------------------------+\n");
      fprintf(stderr,"|%2s |%2s |%2s |%c | %-10s |%-5s |%-5s |%-5s|\n","i","len","ord",'l',"suffix","start", "class","count");
      fprintf(stderr,"+----------------------------------------------+\n");

      // we are taking a character from previous cycle's equivalence class
      fprintf(stderr,"|%2d |%2d |%2d |%c | %-10s |%-5d |%-5d |%-5d|\n"
              ,i,cycle_length,order[i], text[order[i]],
              text_part(text,prefix_start,cycle_length).c_str(),
              prefix_start,prefix_class,count[prefix_class]);

      fprintf(stderr,"+----------------------------------------------+\n");
    #endif

    new_order[count[prefix_class]] = prefix_start;

    #if DEBUG

      fprintf(stderr,"prefix [%s] class [%d] prefix-start:[%d] ",
              text_part(text,prefix_start,cycle_length/2).c_str(),classes[prefix_start],prefix_start);

      fprintf(stderr,"suffix [%s] class [%d] suffix-start:[%d] \n",
              text_part(text,suffix_start,cycle_length/2).c_str(),classes[suffix_start],suffix_start);

      fprintf(stderr,"start : %d\n",prefix_start);
      fprintf(stderr,"cls : %d\n",prefix_class);
      fprintf(stderr,"count[cls] : %d\n",count[prefix_class]);
      fprintf(stderr,"new_order[[count[cls]] : %d\n",new_order[count[prefix_class]]);

      cerr<<"text : "<<text<<endl;
      print_vector("old-order", order);
      print_vector("new_order",new_order);
      print_vector("partial_sums",count);
      print_ordering(text,new_order,cycle_length);

    #endif

  }

  #if DEBUG 
    fprintf(stderr,"+-------------------------------+\n");
    print_ordering(text,new_order,cycle_length);
  #endif

  return new_order;
}

/**
 * Given a sorted of orders , re-compute their equivalence classes for
 * the new cycle_length. We only need to compare the ends of the
 * equivalence classes to check if they lie in the same class.
 */
vector<long long> update_classes(const string & text, vector<long long> & order,
                           vector<long long> & classes,long long cycle_length,
                           long long text_length)
{

  vector<long long> new_classes(classes.size(),-1);
  long long n = text_length;

  long long current_class = 0;

  #if DEBUG
  print_vector("update_classes:order",order);
  print_vector("update_classes:classes",classes);
  std::cerr<<"Updating:[";
  #endif
    
  new_classes[order[0]] = current_class;

  for(long long i = 1; i < order.size(); i++ ) {

    // seems to fix class assignment for GA
    long long prev_start   = order[i-1];
    long long prev_mid     = (order[i-1] + cycle_length/2) % text_length;

    long long cur_start = order[i];
    long long cur_mid   = (order[i] + cycle_length/2) % text_length;

    pair<long long,long long> prev(classes[prev_start], classes[prev_mid]);
    pair<long long,long long> cur (classes[cur_start] , classes[cur_mid]);

    long long old_class = current_class;

    #if DEBUG
    if(debug) {
      fprintf(stderr,"\n|%7s|%7s|%4s|\n","Prev","Curr","Same");
      fprintf(stderr,"|%3d,%3d|%3d,%3d|%4d|\n",
              prev.first,prev.second,cur.first,cur.second,prev == cur);

    }
    #endif

    if(prev!=cur) {
      new_classes[order[i]] = ++current_class;
    } else {
      new_classes[order[i]] = current_class;
    }
    #if DEBUG
    print_vector("new_classes",new_classes);
    #endif
  }

  #if DEBUG
  if(debug)
    std::cerr<<"]"<<endl;
  #endif 

  return new_classes;
}


/**
 * Build suffix array of the string text and return a vector result of
 * the same length as the text such that the value result[i] is the
 * index (0-based) in text where the i-th lexicographically smallest
 * suffix of text starts. Implement this function yourself.
 */
vector<long long> build_suffix_array(const string& text)
{
  vector<long long> order = sort_by_chars(text);
  vector<long long> classes = char_classes(text, order);                                
  long long n = text.size();
  long long cycle_length = 1;
  long long j = 0;

  while(cycle_length < n) {

    #if DEBUG
    if(debug) {
      fprintf(stderr,"--[%2d,%3d]=-------------------------------------------\n",j++,cycle_length);
      std::cerr<<text<<std::endl;
      print_vector("order",order);
      print_vector("classes",classes);
      print_cycles(text,order,classes,cycle_length);
      fprintf(stderr,"--------------------------------------------------\n");
    }
    #endif
    
    cycle_length *= 2;    
    order = sort_doubled(text,order,classes,cycle_length);
    classes = update_classes(text,order,classes,cycle_length,n);
  }

  print_cycles(text,order,classes,cycle_length/2);
  return order;
}

void test_sort_by_chars() {
  vector<long long> order;
  vector<long long> classes;
  {
    string text("ACTGAACAA$");
    std::cerr<<text<<endl;
    vector<long long> order = sort_by_chars(text);
    vector<long long> classes = char_classes(text,order);
    print_vector("classes-" , classes);
  }

  {
    string text("ACTTTGGGTTTGAACAA$");
    std::cerr<<text<<endl;
    order = sort_by_chars(text);
    vector<long long> order = build_suffix_array(text);
  }


  {
    string text("ababaa$");
    #if DEBUG
    if(debug)
      std::cerr<<text<<endl;
    #endif
    order = sort_by_chars(text);
    vector<long long> order = build_suffix_array(text);
    //print_cycles(text,order,text.size());
  }
}

int main() {
  if(false)  test_sort_by_chars();
  string text;
  cin >> text;
  vector<long long> suffix_array = build_suffix_array(text);
  for (long long i = 0; i < suffix_array.size(); ++i) {
    cout << suffix_array[i] << ' ';
  }
  cout << endl;
  return 0;
}

/** Primitive methods **/
inline long long key(char ch) {
  return static_cast<long long>(ch);
}

/** Debugging methods **/
void print_vector(string label, vector<long long> v) {
  if(!debug) return;
  cerr<<label<<":";
  for(long long i : v)
    cerr<<i<<" ";
  cerr<<endl;
}

void print_ordering(const string& text, vector<long long>order, long long cycle_length)
{
  if(!debug) return;
  long long i = 0;
  for(long long start : order) {
    string part("-");
    if(start >= 0)
      part = text_part(text,start,cycle_length);
    fprintf(stderr,"(%2d,%2d) [%8s]\n",start,(start+cycle_length)%text.size(),part.c_str());
    i++;
  }
}

void print_cycles(const string &text,vector<long long> & order,vector<long long> & classes,long long cycle_length) {

  if(!debug) return;

  long long  c = 0;

  fprintf(stderr,"--------------------------------------------------\n");
  fprintf(stderr,"%7s |%7s |%7s |%7s\n","order","class","cycle","suffix");
  fprintf(stderr,"--------------------------------------------------\n");

  for(auto cur : order) {
    const char* suffix = text_part(text,cur,cycle_length).c_str();
    fprintf(stderr,"%7d |%7d |%7d | [%s]|\n",cur,classes[cur],cycle_length,suffix);
  }

  fprintf(stderr,"--------------------------------------------------\n");
}
