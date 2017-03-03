#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <map>

using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::vector;

const char alphabet[] = {'$','A','C','G','T'};
const int alphabet_size = 5;

#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif

using namespace std;

inline int char_index(char c) {
  for(int i = 0; i< alphabet_size; i++)
    if(alphabet[i] == c)
      return i;
}

inline char index_char(int i) {
  return alphabet[i];
}

void print_vector(string label, vector<int> v) {
  if(!debug) 
    return;
  cerr<<label<<":";
  for(int i : v)
    cerr<<i<<" ";
  cerr<<endl;
}


string inverse_bwt(const string& last) {

  int n  = last.size();
  string first(last);
  sort(first.begin(),first.end());

  vector<int> alphabet_frequency(alphabet_size,0);
  vector<int> rank(n,0);

  int i = 0;
  for(char c : last)  {
    int cindex = char_index(c);
    rank[i++] = alphabet_frequency[cindex];
    alphabet_frequency[cindex] =  alphabet_frequency[cindex] + 1 ;
  }

  print_vector("alphabet_frequency",alphabet_frequency);
  print_vector("rank",rank);


  vector<int> partial_sums(alphabet_size,0);
  int partial_sum = 0;

  for(int i = 0; i < alphabet_size;i++) {
    partial_sums[i] = partial_sum;
    partial_sum += alphabet_frequency[i];

  }


  print_vector("partial_sums",partial_sums);

  if(debug) {
    printf("%-5s %-5s %-5s\n","first","last","rank");
    for(int i = 0; i < n; i++) {
      printf("%-5c %-5c %-5d\n",first[i],last[i],rank[i]);
    }
    printf("\n");
  }

  
  char prev = '$';
  vector<char> result;
  int lookup = 0;
  int c = 0;

  while (c < n) {
    
    char cur = last[lookup];
    int  c_index = char_index(cur);

    if(debug) {
      std::cerr<<"c_index:"<<c_index<<" cur ";
      std::cerr<<cur<<" partial_sums :"<<partial_sums[c_index]<<endl;
    }

    lookup = partial_sums[c_index] + rank[lookup];

    if(debug){
      std::cerr<<"partial_sums :"<<partial_sums[c_index];
      std::cerr<<",rank :"<<rank[c];
      std::cerr<<",lookup :"<<lookup<<endl;
    }

    char first_char = first[lookup];
    
    if(debug) {
      std::cerr<<"cur : "  << cur<<endl;
      std::cerr<<"first: " << first_char<<endl;
      std::cerr<<"pushing : " << cur<<endl;
    }
    if(cur!='$')
      result.push_back(cur);

    c++;

  }
  
  std::reverse(result.begin(),result.end());
  result.push_back('$');
    
  char res[n+1];
  i = 0;
  for(char c : result){

    res[i++] = c;
  }
  res[i] ='\0';
  return string(res);   
}

int main() {
  string bwt;
  cin >> bwt;
  cout << inverse_bwt(bwt) << endl;
  return 0;
}
