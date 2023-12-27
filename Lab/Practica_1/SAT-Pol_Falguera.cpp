#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

// First  - Positive literals
// Second - Negative literals
vector<pair<vector<int>,vector<int>> > OcurrList; // Clauses where a literal appears in positive and negative
vector<pair<int,int> > NumOcurrs; // Number of ocurrences in positive and negative of a literal
vector<int> ConflictOcurrs;

int conflicts = 0;

void balance_ConflictOcurrs() {
  for (int n: ConflictOcurrs) {
    n /= 2;
  }
}

void update_ConflictOcurrs(int index) {
  vector<int> literals = clauses[index];

  for (int lit: literals) {
    if (lit < 0) lit = -lit;
    ++ConflictOcurrs[lit];
  }
}

void time_to_restart(int index) {
  
  if (conflicts%1000 == 0) { // Every 1000 conflicts, we divide by 2
    balance_ConflictOcurrs();
  }

  update_ConflictOcurrs(index);
}

void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);  
  // Read clauses AND initialize OcurrList

  OcurrList.resize(numVars+1);
  NumOcurrs.resize(numVars+1);
  ConflictOcurrs.resize(numVars+1,0);

  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      clauses[i].push_back(lit);
      if (lit > 0) {
        OcurrList[lit].first.push_back(i);
        NumOcurrs[lit].first++;
      }
      else {
        OcurrList[-lit].second.push_back(i);
        NumOcurrs[-lit].second++;
      }
    } 
  }    
}

int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}


bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    // Limitate the numClauses to all the clauses where the propagated literal appears
    // and, among them, pick the ones affected by his propagated logical value

    int lit = modelStack[indexOfNextLitToPropagate]; // Propagated literal
    vector<int> limitedClauses; // Clauses affected by the logical value of the propagated literal
    
    if (lit > 0) limitedClauses = OcurrList[lit].second;
    else limitedClauses = OcurrList[-lit].first;
    
    ++indexOfNextLitToPropagate;

    for (int i: limitedClauses) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;

      for (uint k = 0; not someLitTrue and k < clauses[i].size(); ++k){
        int val = currentValueInModel(clauses[i][k]);
        if (val == TRUE) someLitTrue = true;
        else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[i][k]; }
      }

      if (not someLitTrue and numUndefs == 0) {
        ++conflicts;
        time_to_restart(i);
        return true; // conflict! all lits false
      }
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
    }    
  }
  return false;
}

void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  int max = 0;
  int max_act = 0;
  for (uint i = 1; i <= numVars; ++i) // stupid heuristic:
    if (model[i] == UNDEF) {
      int min_ocurrs = min(NumOcurrs[i].first,NumOcurrs[i].second);
      if (min_ocurrs > max_act) {
        max_act = min_ocurrs;
        max = i;
      }
    }
  return max; // reurns 0 when all literals are defined
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
