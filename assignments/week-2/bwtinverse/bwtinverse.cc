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


#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif

string inverse_bwt(const string& last) {

  int n  = last.size();
  string text = "";
  if(debug)
    std::cerr<<last<<endl;
  string first(last);

  sort(first.begin(),first.end());

  std::map<char,int> freq;

  vector<int> rank(0,n);

  int i = 0;

  for(char c : last)  {
    int s = freq[c];
    if(debug)
      std::cerr<<c<<":"<<s<<",";
    rank.push_back(s);
    freq[c]=s+1;    
  }
  if(debug)
    std::cerr<<endl;

  int total = 0;
  char prev = '$';

  vector<char> result;
  enum column {FIRST,LAST};

  column current_column = LAST;
  int c = 0;
  int lookup = 0;

  while(c < n) {
    char cur = last[lookup];
    int cur_rank = rank[lookup];

    if(debug)
      std::cerr<<"Looking for "<<cur<<" rank: "<<cur_rank<<std::endl;

    int j = 0;
    int h = 0;

    while(h < n) {
      if(debug)
        std::cerr<<"cur:"<<cur<<" first[h] "<<first[h]<<" cur_rank "<<cur_rank<<" j:"<<j<<std::endl;
      if(cur_rank == j && cur == first[h]) {
        lookup = h;
        if(debug)
          std::cerr<<"found@:"<<h<<endl;
        break;
      } else if(cur == first[h]) {
        j++;
      }
      h++;
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
