#include <stdio.h>
#include <sys/time.h>

/* Sparse array realization */
typedef struct Node{
  struct Node *left, *right, *up, *down;
  struct Row *row;
  struct Col *col;
} Node;

typedef struct Row{
  struct Row *up, *down;
  struct Node *head;
  int length, name;
} Row;

typedef struct Col{
  struct Col *left, *right;
  struct Node *head;
  int length, name;
} Col;


Row *rptr;
Col *cptr;
int row_num = 0, col_num = 0;


/* insert element to the left */
void insert_left(Node *ref, Node *newbie){
  newbie->right = ref;
  newbie->left  = ref->left;
  newbie->left->right = newbie;
  newbie->right->left = newbie;
};


void insert_up(Node *ref, Node *newbie){
  newbie->down = ref;
  newbie->up   = ref->up;
  newbie->up->down = newbie;
  newbie->down->up = newbie;
};

void remove_from_col(Node *node){
  node->up->down = node->down;
  node->down->up = node->up;
  if(node->col->head == node)
    node->col->head = node->down;
  node->col->length--; 
}

void restore_in_col(Node *node){
  node->up->down = node;
  node->down->up = node;
  if(node->col->head == node->down && node->row->name < node->down->row->name)
    node->col->head = node;
  node->col->length++; 
}

void remove_from_row(Node *node){
  node->left->right = node->right;
  node->right->left = node->left;
  if(node->row->head == node)
    node->row->head = node->right;
  node->row->length--; 
}

void restore_in_row(Node *node){
  node->left->right = node;
  node->right->left = node;
  if(node->row->head == node->right && node->col->name < node->right->col->name)
    node->row->head = node;
  node->row->length++; 
}

Row *remove_row(Row *row){
  if(row->length > 0){
    Node *node = row->head;
    do{
      remove_from_col(node);
      node = node->right;
    }while( node != row->head && node != 0);  
  }
  row->up->down = row->down;
  row->down->up = row->up;
  if(rptr == row)
    rptr = row->down;
  row_num--;
  return row;
}

Row *restore_row(Row *row){
  if(row->length > 0){
    Node *node = row->head;
    do{
      restore_in_col(node);
      node = node->right;
    }while( node != row->head && node != 0);  
  }
  row->up->down = row;
  row->down->up = row;
  if(row->down == rptr && row->name < row->down->name)
    rptr = row;
  row_num++;
  return row;
}


Col *remove_col(Col *col){
  if(col->length > 0){
    Node *node = col->head;
    do{
      remove_from_row(node);
      node = node->down;
    }while( node != col->head && node != 0);  
  }
  col->left->right = col->right;
  col->right->left = col->left;
  if(cptr == col)
    cptr = col->right;
  col_num--;
  return col;
}

Col *restore_col(Col *col){
  if(col->length > 0){
    Node *node = col->head;
    do{
      restore_in_row(node);
      node = node->down;
    }while( node != col->head && node != 0);  
  }
  col->left->right = col;
  col->right->left = col;
  if(col->right == cptr && col->name < col->right->name)
    cptr = col;
  col_num++;
  return col;
}

Col** create_cols(int num){
  if(num <= 0)
    return 0;
  Col **cols = new Col*[num];
  for (int i = 0; i < num; ++i){
    cols[i] = new Col();
    cols[i]->name = i+1;
    cols[i]->length = 0;
    if( i> 0 ) {
      cols[i]->left = cols[i-1];
      cols[i-1]->right = cols[i];
    }
  }
  cols[0]->left = cols[num-1];
  cols[num-1]->right = cols[0];
  cptr = cols[0];
  col_num = num;
  return cols;
}

Row *add_row(Row *tail, int name){
  Row *row = new Row();
  row->length = 0;
  row->name = name;
  if(tail == 0){
    row->up = row->down = row;
  }else{
    row->up = tail;
    row->down = tail->down;
    row->up->down = row;
    row->down->up = row;
  }
  row_num++;
  return row; 
}

void add_node(Row *row, Col *col){
  Node *node = new Node();
  node->row = row;
  node->col = col;

  if(row->length == 0){
    row->head = node;
    node->left = node->right = node;
  }else
    insert_left(row->head, node);
  row->length++;

  if(col->length == 0){
    col->head = node;
    node->up = node->down = node;
  }else
    insert_up(col->head, node);
  col->length++;
}

/* select column with mininimal number of elements */
Col *select_col(){
  int min = cptr->length;
  Col *col = cptr->right, *mc = cptr;
  while(col != cptr){
    if(col->length < min){
      mc = col;
      min = col->length;
    }
    col = col->right;
  }
  return mc;
}

Row *sol[1000];          // solution stack
int depth=0;              // current depth of the stack

// Stacks of deleted rows and cols
Row *rowStack[1000]; int rsId=0;
Col *colStack[1000]; int csId=0;

// checkpoint ids of row and col stacks
// when we moving up we restore cols and rows
// up to the checkpoints given
int rsIdCheckpoint[1000];
int csIdCheckpoint[1000];


int orientations[24][5];  // possible orientations of the piece
int subsets[960][5];      // possible positions of the piece

// voxel number zyx to array index i
// i.e. 321 -> 3*25 + 2*5 + 1 = 86 
int dec2pent(int dec)
{
  int z = dec/100,
      y = (dec/10)%10,
      x = dec % 10;
  return 25*z+5*y+x;
}

char sols[100][126];      // string representing solution
int sol_num = 0;          // number of found solutions

/* compare to solution string
   whether they represent the same solution
   for example, ABBAA and ACCAA
*/
bool compare_solutions(char s1[], char s2[])
{
  int arr[125];
  for (int i = 0; i < 125; ++i){
    arr[i] = 100*(s1[i]-'A') + (s2[i]-'A');
  }

  // non-recursive quicksort
  #define  MAX_LEVELS  125
  int piv, beg[MAX_LEVELS], end[MAX_LEVELS], i=0, L, R ;
  beg[0]=0; end[0]=125;
  while (i>=0) {
    L=beg[i]; R=end[i]-1;
    if (L<R) {
      piv=arr[L];
      while (L<R) {
        while (arr[R]>=piv && L<R) R--;
        if (L<R) arr[L++]=arr[R];
        while (arr[L]<=piv && L<R) L++;
        if (L<R) arr[R--]=arr[L];
      }
      arr[L]=piv; beg[i+1]=L+1; end[i+1]=end[i]; end[i++]=L;
    } else i--;
  }

  int num = 1;
  for (int i = 1; i < 125; ++i)
    if(arr[i] != arr[i-1])
      num++;

  return num==25;
}

/* rotate and reflect the solution
   and compare to already found ones
*/
bool is_unique(char s[]){
  char tr[48][126];
  for (int x = 0; x < 5; ++x)
    for (int y = 0; y < 5; ++y)
      for (int z = 0; z < 5; ++z){
        char c = s[25*z+5*y+x];
        for (int px = 0; px < 2; ++px){
          int X = px ? 4-x : x;
          for (int py = 0; py < 2; ++py){
            int Y = py ? 4-y : y;
            for (int pz = 0; pz < 2; ++pz){
              int Z = pz ? 4-z : z;
              tr[6*(4*pz+2*py+px)+0][25*Z+5*Y+X] = c;
              tr[6*(4*pz+2*py+px)+1][25*Y+5*X+Z] = c;
              tr[6*(4*pz+2*py+px)+2][25*X+5*Z+Y] = c;
              tr[6*(4*pz+2*py+px)+3][25*X+5*Y+Z] = c;
              tr[6*(4*pz+2*py+px)+4][25*Y+5*Z+X] = c;
              tr[6*(4*pz+2*py+px)+5][25*Z+5*X+Y] = c;
            }
          }
        }
      }
  bool unq = false;
  for (int i = 0; i < 48 && !unq; ++i)
    for (int j = 0; j < sol_num; ++j)
      unq = unq || compare_solutions(sols[j], tr[i]);
  return !unq;
}

struct timeval start_time;
float passed_time(){
  struct timeval now;
  gettimeofday(&now, NULL);
  return (now.tv_sec - start_time.tv_sec)
    + 0.000001 * (now.tv_usec - start_time.tv_usec);
}

/* save and print out solution found if it's unique */
void save_solution(){
  char cube[125];
  for (int i = 0; i < 125; ++i)
    cube[i] = '*';

  for (int i = 0; i < depth; ++i){
    int *subset = subsets[ sol[i]->name - 1];
    for (int j = 0; j < 5; ++j)
      cube[dec2pent(subset[j])] = 65+i;
  }
  
  if(! is_unique(cube) )
    return;

  for (int i = 0; i < 125; ++i)
    sols[sol_num][i] = cube[i];
  sols[sol_num][125] = 0;

  printf("\nSolution %d found in %3.1f s: ",
    1+sol_num, passed_time());
  printf("\n\n");

  for (int y = 0; y < 5; ++y) {
    printf("\t");
    for (int z = 0; z < 5; ++z) {
      for (int x = 0; x < 5; ++x)
        printf("%c", cube[25*z+5*y+x]);
      printf(" ");
    }
    printf("\n");
  }

  sol_num++;
}

int routineX(){
  if(col_num == 0){
    save_solution();
    return true;
  }
  if(row_num == 0){
    return false;
  }
  Col *mc = select_col();
  if(mc->length == 0){
    return false;
  }
  Node *fall = mc->head;
  do{
    sol[depth] = fall->row;
    rsIdCheckpoint[depth] = rsId;
    csIdCheckpoint[depth] = csId;
    // printf("\n%c\n", (char) (64+fall->row->name) );
    Node *slide = fall;
    do{
        Node *deletor = slide->down;
        while(deletor != slide){
          rowStack[rsId] = remove_row(deletor->row); rsId++;
          deletor = deletor->down;
        }
      colStack[csId] = slide->col; csId++;
      slide = slide->right;
    }while(slide != fall);
    rowStack[rsId] = remove_row(fall->row); rsId++;
    for (int i = csIdCheckpoint[depth]; i < csId; ++i)
      remove_col(colStack[i]);
    
    depth++;

    routineX();

    depth--;
    while(csId > csIdCheckpoint[depth]){
      csId--; restore_col(colStack[csId]);
    }
    while(rsId > rsIdCheckpoint[depth]){
      rsId--; restore_row(rowStack[rsId]);
    }
    fall = fall->down;
  }while(fall != mc->head);
}

/* simple bubble sort */
void sort(int arr[]){
  for (int i = 0; i < 4; ++i)
    for (int j = i+1; j < 5; ++j)
      if(arr[j] < arr[i]){
        arr[j] +=arr[i];
        arr[i] = arr[j] - arr[i];
        arr[j] = arr[j] - arr[i];
      }
}

/* creating subset array for 25N problem */
void create_subsets(){
  // notation for voxel coordinate
  // int c = 100*z + 10*y + x
  int sample[] = {0, 1, 2, 12, 13};
  for (int i = 0; i < 5; ++i){
    int x = sample[i] % 10,
        y = sample[i] / 10;
    orientations[0][i] = sample[i];      // copy 
    orientations[1][i] = 10*(1-y)+(3-x); // inversion
    orientations[2][i] = 10*y+(3-x);     // x-flipped
    orientations[3][i] = 10*(1-y)+x;     // y-fliped
  }

  // possible rotations around (0,0,0)
  for (int j = 0; j < 4; ++j){
    for (int i = 0; i < 5; ++i){
      int z = 0, y = orientations[j][i] / 10,
                 x = orientations[j][i] % 10;
      orientations[1*4+j][i] = 100*z + 10*x + y;
      orientations[2*4+j][i] = 100*y + 10*x + z;
      orientations[3*4+j][i] = 100*y + 10*z + x;
      orientations[4*4+j][i] = 100*x + 10*z + y;
      orientations[5*4+j][i] = 100*x + 10*y + z;
    }
  }

  // possible translations in the cube
  int index=0;
  for (int j = 0; j < 24; ++j){
    int r = j/4;
    int cond = (r > 0) && (j%4 < 2);
    sort(orientations[j]);
    for (int x = 0; x < 6-(1<<(2-(r%3))); ++x)
      for (int y = 0; y < (r%5==0 ? 4 : ((r < 3) ? 2 : 5)); ++y)
        for (int z = 0; z < 6-(1<<(r/2)); ++z){
          int point = 100*z+10*y+x;
          // Since solution can be rotated and flipped
          // we use only 2 orientations at 0,0,0 point
          // This reduces number of non-uique solutions
          // and speeds up the calculation
          if(point == 0 && cond) 
            subsets[index][0] = -1;
          else
            for (int i = 0; i < 5; ++i)
              subsets[index][i] = point + orientations[j][i];
          index++;
        }
  }
}

int main() 
{
  gettimeofday(&start_time, NULL);
  create_subsets();
  Col **cols = create_cols(125);
  rptr = 0;
  for (int j = 0; j < 960; ++j)
    if(subsets[j][0]>=0){       // dont use unuseful piece positions 
      rptr = add_row(rptr, j+1);
      for (int i = 0; i < 5; ++i)
        add_node(rptr, cols[dec2pent(subsets[j][i])] );
    }
  rptr = rptr->down;
  printf("\nSparse array filled in %3.1f ms.\n",
    1000*passed_time() );
  routineX();
  printf("\nThat's all folks. Ended in %3.1f s.\n",
    passed_time() );
} 