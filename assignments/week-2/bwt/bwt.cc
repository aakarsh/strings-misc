#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <list>

using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::vector;

using namespace std;


#ifdef DEBUG
const bool debug = true;
#else
const bool debug = false;
#endif


void rotate(std::list<char> & pos){
  pos.push_front(pos.back());
  pos.pop_back();
}

/**
 * Naive impementation of bwt
 */
string bwt(const string& text) {
  int n  = text.size();

  std::list<char> pos;
  for(int i = 0; i < n; i++)
    pos.push_back(text[i]);

  vector<string> table;

  for(int i = 0; i < n; i++) {
    rotate(pos);
    char s[n];
    int j = 0;
    for(char c : pos) {
      s[j] = c;
      j++;
    }

    string line(s,n);
    table.push_back(line);
  }
  // sort them ?
  sort(table.begin(),table.end());

  if(debug) {
    std::cerr<<"----------"<<endl;
    for(int i = 0; i < n; i++) {
      std::cerr<<table[i]<<endl;
    }
    std::cerr<<"----------"<<endl;
  }

  char r[n+1];
  for(int i = 0; i < n ; i++) {
    r[i] = table[i][n-1];
  }
  r[n] = '\0';

  if(debug) {
    for(int i = 0; i < n; i++) {
      std::cerr<<r[i];
    }
    std::cerr<<endl;
    std::cerr<<"*********"<<endl;
  }

  return string(r);
}



int main() {
  string text;
  cin >> text;
  cout << bwt(text) << endl;
  return 0;
}
