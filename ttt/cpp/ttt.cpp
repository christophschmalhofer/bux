
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <map>
#include <utility>
#include <cstdlib>
#include <ctime>


using namespace std;

namespace TicTacToe {

  class Who {
  private:
    char icon_;

  public:
    Who(char icon):icon_(icon) {
    }

    bool empty() {
      return icon_ == '_';
    }

    char icon() const{
      return icon_;
    }
    

    bool operator==(const Who& otherWho) const{
      return icon_ == otherWho.icon_;
    }

    bool operator!=(const Who& otherWho) const{
      return !(*this == otherWho);
    }
  };


  ostream& operator<<(ostream& strm, const Who& who) {
    return strm << who.icon();
  } 
    
  const Who Empty('_');
  const Who Me('o');
  const Who You('x');
    
  const Who& getOtherPlayer(const Who& who) {
    if (who == Me) {
      return You;
    } else {
      return Me;
    }
  }

  const unsigned int size = 3;
  

  struct Position { 
    Position(): x(0), y(0) {};
    Position(unsigned int x, unsigned int y): x(x), y(y) {};

    unsigned int x; 
    unsigned int y; 
  };


  Position winnerTriples[][3] = {
    {Position(0,0),Position(0,1),Position(0,2)},
    {Position(1,0),Position(1,1),Position(1,2)},
    {Position(2,0),Position(2,1),Position(2,2)},
    {Position(0,0),Position(1,0),Position(2,0)},
    {Position(0,1),Position(1,1),Position(2,1)},
    {Position(0,2),Position(1,2),Position(2,2)},
    {Position(0,0),Position(1,1),Position(2,2)},
    {Position(0,2),Position(1,1),Position(2,0)}
  };


  typedef vector<vector<const Who*> > BoardType;

  class Board {
  private:
    BoardType  board;
    int countMoves;

    static const int maxUtil = 20;
    friend ostream &operator<<(ostream& strm, const Board& board);
  public:

    Board(): board(), countMoves(0){
      for(unsigned int i = 0; i < size; i++) {
	vector<const Who*> row;
	for(unsigned int k = 0; k < size; k++) {
	  row.push_back(&Empty);
	}
	board.push_back(row);
      }
    }

    Board(const Board& copyFromMe):board(copyFromMe.board.size()), countMoves(copyFromMe.countMoves) {
      copy(copyFromMe.board.begin(), copyFromMe.board.end(), board.begin());
    }

    vector<Position> emptyCells() {
      vector<Position> empties;
      for(unsigned int i = 0; i < size; i++) {
	for(unsigned int k = 0; k < size; k++) {
	  if (*board[i][k] == Empty) {
	    empties.push_back(Position(i,k));
	  }
	}
      }
      return empties;
    }

    bool noCellLeft() {
      return countMoves == size * size;
    }

    void setWho(Position pos, const Who& who) {
      countMoves++;
      board[pos.x][pos.y] = &who;
    }

    const Who& getWho(Position pos)  {
      return *board[pos.x][pos.y];
    }

    const Who& findWinner() {
      for(int nr = 0; nr < 8; nr++) {
	Position triple[3] = winnerTriples[nr];
	if( getWho(triple[0]) != Empty && getWho(triple[0]) == getWho(triple[1]) && getWho(triple[1]) == getWho(triple[2])) {
	  return getWho(triple[0]);
	}
      }
      return Empty;
    }

    int utility(const Who& player, const Who& mover, int depth) {
      const Who& winner = findWinner();
      if (winner == player) {
	return maxUtil - depth;
      } else if (winner == getOtherPlayer(player)) {
	return depth - maxUtil;
      }
      if(noCellLeft()) {
	return 0;
      }

      map<int, Position> utilities;
      vector<Position> empties = emptyCells();
      for(vector<Position>::const_iterator it = empties.begin(); it != empties.end(); ++it) {
	const Position pos = *it;
	Board clone(*this);
	clone.setWho(pos, mover);
	int utility = clone.utility(player, getOtherPlayer(mover), depth + 1);
	utilities[utility] = pos;
      }
      pair<int,Position> minMaxPair = (player == mover) ? *utilities.rbegin() : *utilities.begin();
      return minMaxPair.first;
    }
  };


  Position parsePosition( string line) {
    stringstream ss(line);
    string token;
    getline(ss, token, ',');
    Position pos;
    stringstream(token) >> pos.x;
    getline(ss, token, ',');
    stringstream(token) >> pos.y;
    return pos;
  }

  ostream &operator<<(ostream& strm, const Position& pos) {
    return strm << "(" << pos.x << "," << pos.y << ")";
  }

  ostream &operator<<(ostream& strm, const Board& board) {
    strm << endl;
    for(vector<vector<const Who*> >::const_iterator it = board.board.begin(); 
	it != board.board.end(); 
	++it) {
      for(vector<const Who*>::const_iterator cell = (*it).begin(); 
	  cell != (*it).end(); ++cell) 
	{
	  strm << **cell << " ";
	}
      strm << endl;
    }
    
    return strm;
  }

  void computeMove(Board& board, const Who& who) {
    vector<Position> empties = board.emptyCells();
    multimap<int,Position> mmap;

    for(vector<Position>::const_iterator it = empties.begin(); it != empties.end(); ++it) {
      const Position pos = *it;
      Board copy(board);
      copy.setWho(pos, who); 
      int utility = copy.utility(who, getOtherPlayer(who),1);
      mmap.insert( pair<int,Position>(utility, pos));
    }
    
    int maxUtilFound = mmap.rbegin()->first;
    srand((unsigned int) time(0));
    multimap<int,Position>::iterator bucketIterator = mmap.lower_bound(maxUtilFound);
    advance(bucketIterator, (unsigned int) rand() % mmap.count(maxUtilFound));
    board.setWho(bucketIterator->second, who);
  }

}

using namespace TicTacToe;

int main() {
  string input;
  Board board;
  cout << board << endl;
  while (!getline(cin, input).eof()) {
    Position pos = parsePosition(input);
    if (board.getWho(pos) == Empty) {
      board.setWho(pos, You);
    } else {
      cout << "Position: " << pos << " nicht frei. " << endl;
      continue;
    }
    if ( board.findWinner() != Empty || board.noCellLeft()) {
      break;
    }
    computeMove(board, Me);
    if (board.findWinner() != Empty || board.noCellLeft()) {
      break;
    }
    cout << board << endl;
  }
  const Who& winner = board.findWinner();  
  if ( winner != Empty) {
    cout << "Winner: " << winner.icon() << endl << board;
  } else {
    cout << "Remis" << endl << board;
  }
  return 0;
}


