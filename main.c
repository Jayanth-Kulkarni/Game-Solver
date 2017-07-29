#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#define SIZE  10


int *action;
int *currentstrategyset;
int **strategy;
int **payoffofplayers;
int *strongdominanceequilibria;
int *weakdominanceequilibria;
int *veryweakdominanceequilibria;
int numberofstrategyprofiles;
int numberofplayers;
int *powerset;
char **powerset_char;
char **powerset_char2;
float ***payoff_mixed;
float **pay;
float ***equation_values;
float ***mixedstrategynashequilibrium;
int action_min_size;

typedef struct maxmin_data
{
    int* value;
    int* index;
}maxmin_data;

maxmin_data* maxmin_array;

typedef struct minimax_data
{
    int** value;
    int** index;
}minimax_data;

minimax_data* minimax_array;

int cmpfunc (const void * a, const void * b)
{
   return (*(int*)a - *(int*)b);
}

void gauss_elimination_pivoting(int i,int n,int r,int z)
{
    int j,k,p,q,m;
    float temp,x[SIZE],sum,max;
    for(j=0;j<n-1;j++)
    {
        max = fabs(pay[j][j]);
        p=j;
        for(m=j+1;m<n;m++)
        {
            if(fabs(pay[m][j]) > max)
            {
                max = pay[m][j];
               p = m;
            }
        }
        if(p != j)
        {
            for(q=j;q<n+1;q++)
            {
                temp = pay[j][q];
                pay[j][q] = pay[p][q];
                pay[p][q] = temp;
            }
        }
        for(m=j+1;m<n;m++)
        {
            temp = pay[m][j] / pay[j][j];
            for(k=j;k<n+1;k++)
            {
                pay[m][k] = pay[m][k] - (temp * pay[j][k]);
            }
        }
    }

    x[n-1] = (float)(pay[n-1][n] / pay[n-1][n-1]);


    for(m=n-2;m>=0;m--)
    {
        sum = 0;
        for(j=m+1;j<n;j++)
        {
            sum = sum + (pay[m][j] * x[j]);
        }
        x[m]=(pay[m][n] - sum)/pay[m][m];
    }
    for(m=0;m<n;m++)
    {
            if(x[m]==-0)
    {
        x[m] = 0;
    }

        equation_values[i][z][m] = x[m];
    }
}

int  printPowerSet(int *set, int set_size,int t)
{
    int mm;
    int count=0;
    unsigned int pow_set_size = pow(2, set_size);
    powerset = (int*)malloc(sizeof(int*)*pow_set_size);
    int counter,i,j;
    for(counter = 0; counter < pow_set_size; counter++)
    {
    int temp = 0;
    for(j = 0; j < set_size; j++)
    {
       if(counter & (1<<j))
       {

           temp*=10;
           temp+=set[j];
       }
    }
    mm = pow(10,(action_min_size));
    if((temp<mm)&&(temp>9))
    {

        count++;
    }
    powerset[counter]  = temp;
    }
    qsort(powerset,pow(2, set_size),sizeof(int),cmpfunc);
    if(t==1)
    powerset_char = (char**)malloc(sizeof(char*)*count);
    powerset_char2 = (char**)malloc(sizeof(char*)*count);
    for(i=0;i<count;i++)
    {
        if(t==1)
        powerset_char[i] = (char*)malloc(sizeof(char*)*action_min_size);
        powerset_char2[i] = (char*)malloc(sizeof(char*)*action_min_size);
        for(j=0;j<action_min_size;j++)
            {
                if(t==1)
                powerset_char[i][j] = '0';
                powerset_char2[i][j] = '0';
            }
    }
    j=0;
    int index = 0;
    for(i=0;i<pow(2, set_size);i++)
    {
    if(powerset[i]>9 && powerset[i]<mm)
    {
      int temp = powerset[i];
      int temp2;
      int c = 0;
      while(temp!=0)
      {
          temp/=10;
          ++c;
      }
      temp = pow(10,(c-1));
      temp2 = powerset[i];
      int temp3 = temp2;
      int subindex=0;
      while(temp3>0)
      {
          temp2 = temp3;
          temp2 /= temp;
          if(t==1)
          powerset_char[index][subindex] = temp2 + '0';
          powerset_char2[index][subindex] = temp2 + '0';
          temp3 %=temp;
          temp /= 10;
          subindex++;
      }
      index++;
    }
    }
return index;
}

int findindex(int * currentstrategyset)
{
    int i;
    int c = 0,j;
    for (i = 0; i < numberofstrategyprofiles  ; ++i)
    {
        c=0;
        for (j = 0; j < numberofplayers; ++j)
        {
           if(strategy[i][j]==currentstrategyset[j])
           {
                c++;
           }
           if(c==numberofplayers)
           {
                return i;
           }
        }
    }
    return -1;
}

int findpayoff(int * currentstrategyset,int i)
{
    int index = findindex(currentstrategyset);
    int val = payoffofplayers[index][i];
    return val;
}

void set_strategy(int i,int max)
{
	while(i<max)
	{
		if(currentstrategyset[i]+1>action[i])
		{
			currentstrategyset[i] = 1;
			i++;
			continue;
		}
		else
		{
			currentstrategyset[i]++;
			return;
		}
	}
}
void set_strategy_complex(int i, int current_player)
{
    while(i<numberofplayers)
    {
        if(currentstrategyset[i]+1>action[i])
        {
            currentstrategyset[i] = 1;
            if((i+1)<current_player)
            {
                i++;
                continue;
            }
            if((i+1)==current_player)
            {
                i=i+2;
                continue;
            }
        }
        else
        {
            currentstrategyset[i] = currentstrategyset[i] + 1;
            return;
        }
    }
}

void change_strategyset(int current_player)
{
    if(current_player==numberofplayers-1)
    {
        set_strategy(0,current_player);
    }
    else if(current_player==0)
    {
        set_strategy(1,numberofplayers);
    }
    else
    {
        set_strategy_complex(0,current_player);
    }
}
void finddominance(int i)
{
    int current_strategy_loop,canprint,j=0,main_for=0;
    int canbreak;
    int current_payoff,comparing_payoff;
    int strong;int k=0;
    int c1,c2,c3;
    for(current_strategy_loop=1;current_strategy_loop<=action[i];current_strategy_loop++)
    {
        strong = 1;
        for(j=0;j<numberofplayers;j++)
        {
            currentstrategyset[j] = 1;
        }
        while(1)
        {
            currentstrategyset[i] = current_strategy_loop;
            canbreak = 0;
            c1=0;
            c2=0;
            current_payoff = findpayoff(currentstrategyset,i);
            for(j=current_strategy_loop-1;j>=1;j--)
            {
                currentstrategyset[i] = j;
                comparing_payoff = findpayoff(currentstrategyset,i);
                if(current_payoff<comparing_payoff)
                {
                    canbreak = 1;
                    break;
                }
                if(current_payoff == comparing_payoff)
                {
                    strong = 0;
                }
                if(current_payoff>comparing_payoff)
                {
                    c1++;
                }

            }
            if(canbreak==1)
            {
                main_for = 1;
                break;
            }

            currentstrategyset[i] = current_strategy_loop;
            for(j=current_strategy_loop+1;j<=action[i];j++)
            {
                currentstrategyset[i] = j;
                comparing_payoff = findpayoff(currentstrategyset,i);
                if(current_payoff<comparing_payoff)
                {
                    canbreak = 1;
                    break;
                }
                if(current_payoff == comparing_payoff)
                {
                     strong = 0;
                }
                if(current_payoff>comparing_payoff)
                {
                    c2++;
                }
            }
            if(canbreak==1)
            {
                main_for = 1;
                break;
            }

            if((c1==current_strategy_loop-1)&&(c2==action[i]-current_strategy_loop))
            {
                c3 = 1;
            }
            canprint = 1;
            for(k=0;k<numberofplayers;k++)
            {
                if(k==i)
                {
                    continue;
                }
                if(currentstrategyset[k]!=action[k])
                {
                    canprint = -1;
                    break;
                }
            }
            if(canprint == 1)
            {
              if(strong == 1)
                {
                    printf("Player %d has a strong dominance strategy in %d\n",i+1,current_strategy_loop);
                    strongdominanceequilibria[i] = current_strategy_loop;
                }

              if(c3==1)
                {
                  printf("Player %d has a weakdominance strategy in %d\n",i+1,current_strategy_loop);
                  weakdominanceequilibria[i] = current_strategy_loop;
                }

                  printf("Player %d has a very weakdominance strategy in %d\n",i+1,current_strategy_loop);
                  veryweakdominanceequilibria[i] = current_strategy_loop;
                main_for = 1;
                break;
            }
            else
            {
                change_strategyset(i);
                continue;
            }
        }
        if(main_for==1)
            continue;

    }
}

void maxmin(int i)
{
    int current_strategy_loop,canprint,j=0;
    int current_payoff,comparing_payoff;
    int k=0;
    for(current_strategy_loop=1;current_strategy_loop<=action[i];current_strategy_loop++)
    {
        for(j=0;j<numberofplayers;j++)
        {
            currentstrategyset[j] = 1;
        }
        currentstrategyset[i] = current_strategy_loop;
        current_payoff = findpayoff(currentstrategyset,i);
        maxmin_array[i].index[current_strategy_loop] = current_strategy_loop;
        maxmin_array[i].value[current_strategy_loop] = current_payoff;
        while(1)
        {
            currentstrategyset[i] = current_strategy_loop;
            comparing_payoff = findpayoff(currentstrategyset,i);
            if(current_payoff>comparing_payoff)
            {
                current_payoff = comparing_payoff;
                maxmin_array[i].index[current_strategy_loop] = current_strategy_loop;
                maxmin_array[i].value[current_strategy_loop] = current_payoff;
            }
            canprint = 1;
            for(k=0;k<numberofplayers;k++)
            {
                if(k==i)
                {
                    continue;
                }
                if(currentstrategyset[k]!=action[k])
                {
                    canprint = -1;
                    break;
                }
            }
            if(canprint == 1)
            {
                break;
            }
            else
            {
                change_strategyset(i);
                continue;
            }
        }

    }
}

void minimax(int i)
{
    int player;
    int current_strategy_loop,canprint,j=0;
    int current_payoff,comparing_payoff;
    int k=0;

    for(current_strategy_loop=1;current_strategy_loop<=action[i];current_strategy_loop++)
    {
        for(j=0;j<numberofplayers;j++)
        {
            currentstrategyset[j] = 1;
        }
        currentstrategyset[i] = current_strategy_loop;
        for(player=0;player<numberofplayers;player++)
        {
            if(player==i)
                continue;

            current_payoff = findpayoff(currentstrategyset,player);
            minimax_array[i].index[player][current_strategy_loop] = current_strategy_loop;
            minimax_array[i].value[player][current_strategy_loop] = current_payoff;
        }
        while(1)
        {
            currentstrategyset[i] = current_strategy_loop;
            for(player=0;player<numberofplayers;player++)
            {
                if(player==i)
                    continue;
                comparing_payoff = findpayoff(currentstrategyset,player);
                if(current_payoff<comparing_payoff)
                {
                    current_payoff = comparing_payoff;
                    minimax_array[i].index[player][current_strategy_loop] = current_strategy_loop;
                    minimax_array[i].value[player][current_strategy_loop] = current_payoff;
                }
            }
            canprint = 1;
            for(k=0;k<numberofplayers;k++)
            {
                if(k==i)
                {
                    continue;
                }
                if(currentstrategyset[k]!=action[k])
                {
                    canprint = -1;
                    break;
                }
            }
            if(canprint == 1)
            {
                break;
            }
            else
            {
                change_strategyset(i);
                continue;
            }
        }

    }
}

void mixedstrategynash()
{
    int i,j;
    int set1[action[0]];
    int set2[action[1]];
    if(action[0]!=action[1])
    {
        if(action[0]<action[1])
        {
            action_min_size = action[0];
        }
        else
        {
            action_min_size = action[1];
        }
    }
    else
    {
        action_min_size = action[0];
    }
    payoff_mixed = (float***)malloc(numberofplayers*sizeof(float));
    mixedstrategynashequilibrium = (float***)malloc(sizeof(float*)*numberofplayers);
    for(i=0;i<numberofplayers;i++)
    {
        payoff_mixed[i] = (float**)malloc((action[i])*sizeof(float));
        mixedstrategynashequilibrium[i] = (float**)malloc((action[i])*sizeof(float));
        if(i==0)
        {
            for(j=0;j<action[i];j++)
            {
            payoff_mixed[i][j] = (float*)malloc((action[1])*sizeof(float));
            mixedstrategynashequilibrium[i][j] = (float*)malloc((action[1])*sizeof(float));
            }
        }
        else
        {
            for(j=0;j<action[i];j++)
            {
            payoff_mixed[i][j] = (float*)malloc((action[0])*sizeof(float));
            }
        }
    }

    for(i=0;i<numberofplayers;i++)
    {
    int current_strategy_loop,canprint,j=0;
    int comparing_payoff;
    int k=0;
    for(current_strategy_loop=1;current_strategy_loop<=action[i];current_strategy_loop++)
    {
        for(j=0;j<numberofplayers;j++)
        {
            currentstrategyset[j] = 1;
        }
        currentstrategyset[i] = current_strategy_loop;
        while(1)
        {
            currentstrategyset[i] = current_strategy_loop;
            comparing_payoff = findpayoff(currentstrategyset,i);
            if(i==0)
            {
            payoff_mixed[i][currentstrategyset[0]-1][currentstrategyset[1]-1] = comparing_payoff;
            }
            else
            {

             payoff_mixed[i][currentstrategyset[1]-1][currentstrategyset[0]-1] = comparing_payoff;
            }
            canprint = 1;
            for(k=0;k<numberofplayers;k++)
            {
                if(k==i)
                {
                    continue;
                }
                if(currentstrategyset[k]!=action[k])
                {
                    canprint = -1;
                    break;
                }
            }
            if(canprint == 1)
            {
                break;
            }
            else
            {
                change_strategyset(i);
                continue;
            }
        }
    }
    }
    int k=0;

for(i=0;i<action[0];i++)
{
    set1[i] = i+1;
}
int val = printPowerSet(set1, action[0],1);
pay = (float**)malloc(sizeof(float*)*(action_min_size+1));
for (i = 0; i < action_min_size+1; ++i)
{
    pay[i] = (float*)malloc(sizeof(float*)*(action_min_size+2));
}

for(i=0;i<action[1];i++)
{
    set2[i] = i+1;
}
int val2 = printPowerSet(set2, action[1],2);
int val3 = val*val2;

equation_values = (float***)malloc(sizeof(float*)*numberofplayers);
for(i=0;i<numberofplayers;i++)
{
    equation_values[i] = (float**)malloc(sizeof(float*)*val3);
    for(j=0;j<val3;j++)
    {
        equation_values[i][j] = (float*)malloc(sizeof(float*)*(action_min_size+2));
    }
}

int l,va = val3;
k=0;
for(k=0;k<val2;k++)
{


    for(i=0;i<val;i++)
    {
       for(j=0;j<action_min_size;j++)
        {
            for(l=0;l<action_min_size;l++)
            {
                pay[j][l] = payoff_mixed[0][powerset_char[i][j]-1-'0'][powerset_char2[k][l]-1-'0'];
            }
            pay[j][l] = -1;
            l++;
            pay[j][l] = 0;
        }

        for(l=0;l<action_min_size;l++)
        {
                pay[j][l] = 1;
        }
        pay[j][l] = 0;
        l++;
        pay[j][l] = 1;
        gauss_elimination_pivoting(0,action_min_size+1,action_min_size+2,(k+i));


    }
}

float** values = (float**)malloc(sizeof(float*)*val2);
for(i=0;i<val2;i++)
{
    values[i] = (float*)malloc(sizeof(float*)*action_min_size);
}
for(k=0;k<val;k++)
{
    for(i=0;i<val2;i++)
    {
        va--;
       for(j=0;j<action_min_size;j++)
        {
            for(l=0;l<action_min_size;l++)
            {
                pay[j][l] = payoff_mixed[1][powerset_char2[i][j]-1-'0'][powerset_char[k][l]-1-'0'];
                if(j==0)
                {
                    values[i][l] = payoff_mixed[1][va][powerset_char[k][l]-1-'0'];
                }

            }
            pay[j][l] = -1;
            l++;
            pay[j][l] = 0;
        }
        for(l=0;l<action_min_size;l++)
        {
                pay[j][l] = 1;
        }
        pay[j][l] = 0;
        l++;
        pay[j][l] = 1;
        gauss_elimination_pivoting(1,action_min_size+1,action_min_size+2,(k+i));

    }
}
int aa[val3];
for(i=0;i<val3;i++)
{
    aa[i] = 99;
}
int count = 0;

for(i=0;i<numberofplayers;i++)
{
    for(j=0;j<val3;j++)
    {
        for(k=0;(k<action_min_size+1);k++)
        {
        printf("equation_values[i][j][k] = %f\n",equation_values[i][j][k]);
        }
    }
}
for(i=0;i<numberofplayers;i++)
{
    for(j=0;j<val3;j++)
    {
        float solution = 0;
        for(k=0;(k<action_min_size+1);k++)
        {
            if(equation_values[i][j][k]<0 && k<action_min_size)
            {
                aa[count] = j;
                count++;
            }
            if(i==1)
            {
                if(k<action_min_size)
                {
                    solution += values[j][k]*equation_values[i][j][k];
                }
                else if(solution>equation_values[i][j][k])
                {
                    aa[count] = j;
                    count++;
                }
            }
        }
    }
}
int solv;
qsort(aa,val3,sizeof(int),cmpfunc);
for(i=0;i<val3;i++)
{
    if(aa[i]!=i)
    {

        solv = i;
        printf("solv = %d\n",aa[i]);
        break;
    }
}

for(i=0;i<numberofplayers;i++)
{
    for(k=0;(k<action_min_size+1);k++)
    {
         printf("equation_values[%d][%d][%d] = %f\n",i,solv,k,equation_values[i][solv][k]);
    }
}
}
void nashequilibria()
{

    int i,j,k;
    int alpha;
    int current_payoff,comparing_payoff,cont=0;
    int v =0;
    for(j=0;j<numberofstrategyprofiles;j++)
    {
     for(k=0;k<numberofplayers;k++)
    {
        currentstrategyset[k] = strategy[j][k];
    }
    for(i=0;i<numberofplayers;i++)
    {
        cont = 0;
        current_payoff = findpayoff(currentstrategyset,i);
        alpha = currentstrategyset[i];
        for(currentstrategyset[i]=currentstrategyset[i]-1;currentstrategyset[i]>=1;currentstrategyset[i]--)
        {
            comparing_payoff = findpayoff(currentstrategyset,i);
            if(current_payoff<comparing_payoff)
            {
                cont = 1;
                break;
            }
        }
        if(cont == 1)
        {
            break;
        }
        currentstrategyset[i] = alpha;
        for(currentstrategyset[i]=currentstrategyset[i]+1;currentstrategyset[i]<=action[i];currentstrategyset[i]++)
        {
            comparing_payoff = findpayoff(currentstrategyset,i);
            if(current_payoff<comparing_payoff)
            {
                cont = 1;
                break;
            }
        }
        if(cont==1)
        {
            break;
        }
        currentstrategyset[i] = alpha;
    }

    if(cont==1)
    {
        continue;
    }
    int m;
    printf("Nash equilibrium for strategy\n");
    printf("[");
    for(m=0;m<numberofplayers;m++)
    {
        v = 1;
        printf(" %d ",currentstrategyset[m]);
    }
    printf("]");
    printf("\n\n");
    }
    if(v==0)
       {
           mixedstrategynash();
       }

}


int main()
{
int payoff[30];
char payoffs[30][100];
int payoff_length;
int i=0;
char line[5][128];
FILE *filepointer;
int n=0;
int j=0;
int k=0;
int l=0;
int val[3] = {0};
int value;
char act[100];
char player_names[150];
char players[100][10];
numberofplayers =0;
numberofstrategyprofiles = 1;

    filepointer = fopen("random.nfg","r");
    if(filepointer == NULL)
    {
        perror("Error while opening file!\n");
        exit(EXIT_FAILURE);
    }
    while(fgets(line[i],sizeof line[0],filepointer)!=NULL)
    {

        if(i==1)
        {
            line[i][strlen(line[i])-1] = '\0';
        }
        if(i==3)
        {
            line[i][strlen(line[i])] = '\0';
            payoff_length = strlen(line[i]);
        }
        i++;
    }
    fclose(filepointer);
    i=0;

while(n<payoff_length)
{
        while( (line[3][n]!= ' ') && (n<payoff_length) && (line[3][n]!= '\0'))
        {
            payoffs[j][i] = line[3][n];
            i++;
            n++;
        }
        payoffs[j][i] = '\0';
        i=0;
        n++;
        j++;
}
n = 0;
char s[3] = "{}";
char *token;
token = strtok(line[1],s);
i=0;
while(token != NULL )
{
    if(i==0)
    {
    strcpy(player_names,token);
    }
    else
    {
     strcpy(act,token);
    }
    token = strtok(NULL,s);
    i++;
}
k=0;

for(i=0;act[i]!='\0';i++)
{
   if(act[i]!= ' ')
    {
        k++;
    }
}

action = (int*)malloc(sizeof(int)*k);
k=0;
for(i=0;act[i]!='\0';i++)
{
   if(act[i]!= ' ')
    {
        action[k] = act[i]-'0';
        numberofstrategyprofiles*=action[k];
        k++;
    }
}
const char s1[2] = "\"";
token = strtok(player_names,s1);
i=0;
while(token !=NULL)
{
    token = strtok(NULL,s1);
    if(token !=NULL && strcmp(token," "))
    {
    strcpy(players[i],token);
    i++;
    }
}
numberofplayers = i;       /** the player names are stored in palyers[i]**/

for(k=0;k<j;k++)
{
    for(i=0;i<3;i++)
    {
        val[i] = 0;
    }
    for(i=0;i<strlen(payoffs[k]);i++)
    {
        val[i] = payoffs[k][i] - '0';
    }
    value = 0;
    l = 0;
    if(val[l] == -3) // negative numbers ..
    {
        l++;
      while(l<strlen(payoffs[k]))
    {
            value += val[l];
            if(l<strlen(payoffs[k])-1)
                value*= 10;
            l++;
    }
    value = -value;
    }
    else
    {
    while(l<strlen(payoffs[k]))
    {
        if(val[l]<0)
            break;
        value += val[l];
        if(l<strlen(payoffs[k])-1 )
        {
            if(val[l+1]>=0)
            value = value*10;
        }
        l++;
    }
    }

    payoff[k] = value;      /** the payoffs of the players are stored in payff[i] **/

}
currentstrategyset = (int *)malloc(sizeof(int)*numberofplayers);
maxmin_array = (maxmin_data*)malloc(sizeof(maxmin_data*)*numberofplayers);
minimax_array = (minimax_data*)malloc(sizeof(minimax_data*)*numberofplayers);

for(i=0;i<numberofplayers;i++)
{
    currentstrategyset[i] = 1;
    maxmin_array[i].index = (int *)malloc(sizeof(int)*action[i]);
    maxmin_array[i].value = (int *)malloc(sizeof(int)*action[i]);
    minimax_array[i].index = (int **)malloc(sizeof(int)*numberofplayers);
    minimax_array[i].value = (int **)malloc(sizeof(int)*numberofplayers);

    for(k=0;k<numberofplayers;k++)
    {
        minimax_array[i].value[k] = (int *)malloc(sizeof(int)*(action[i]+1));
        minimax_array[i].index[k] = (int *)malloc(sizeof(int)*(action[i]+1));
        for(j=1;j<=action[i];j++)
        {
            minimax_array[i].index[k][j] = 0;
            minimax_array[i].value[k][j] = 0;
        }
    }
    for(j=1;j<=action[i];j++)
    {
        maxmin_array[i].index[j] = 0;
        maxmin_array[i].value[j] = 0;

    }
}

strategy = (int **)malloc(numberofstrategyprofiles*sizeof(int));
payoffofplayers = (int **)malloc(numberofstrategyprofiles*sizeof(int));
strongdominanceequilibria = (int *)malloc(numberofplayers*sizeof(int));
weakdominanceequilibria = (int *)malloc(numberofplayers*sizeof(int));
veryweakdominanceequilibria = (int *)malloc(numberofplayers*sizeof(int));

for(i=0;i<numberofplayers;i++)
{
strongdominanceequilibria[i] = 0;
weakdominanceequilibria[i] = 0;
veryweakdominanceequilibria[i] = 0;
}

for(i=0;i<numberofstrategyprofiles;i++)
{
    strategy[i] = (int *)malloc(numberofplayers*sizeof(int));
    payoffofplayers[i] = (int *)malloc(numberofplayers*sizeof(int));
}
k=0;
int check=0;
int check_s[2];
for(i=0;i<numberofstrategyprofiles;i++)
{
    for(j=0;j<numberofplayers;j++)
    {
        strategy[i][j] = currentstrategyset[j];
        payoffofplayers[i][j] = payoff[k];
        k++;
    }
    set_strategy(0,numberofplayers);
}

int iszerosum = 0;
int isnotzerosum = 0;
for(i=0;i<numberofstrategyprofiles*numberofplayers;i++)
{

check_s[check] = payoff[i];
check = (check+1)%2;
if (check==0)
{
    if (check_s[1] != -(check_s[0]))
    {
        isnotzerosum++;
    }
}
}

if (isnotzerosum == 0)
{
    printf("It is a zerosum game\n");
}
else
{
    printf("it is not a zerosum game\n");
}
for (i = 0; i < numberofplayers; ++i)
{
finddominance(i);
}
printf("\n");
int m,no,check_2=0,c;
int *in;

for(i=0;i<numberofplayers;i++)
{
    no = 1;
    maxmin(i);
    in = (int*)malloc(sizeof(int*)*action[i]);
    m = maxmin_array[i].value[1];
    in[no] = maxmin_array[i].index[1];
    for(j=2;j<=action[i];j++)
    {
    if(maxmin_array[i].value[j]>=m)
    {
       if(maxmin_array[i].value[j]==m)
       {
           no++;
           in[no] = j;
           check_2 = 1;
       }
       else
       {
        no=1;
        if(check_2==1)
        {
            for(c=0;c<=action[i];c++)
            {
                in[c] = 0;
            }
        }
        m = maxmin_array[i].value[j];
        in[no] = maxmin_array[i].index[j];
       }
    }
    }
    for(c=1;c<=no;c++)
    printf("player %d maxminvalue = %d maxminstrategy = %d\n",i+1,m,in[c]);
}
int **inn;

    int f;
inn = (int**)malloc(sizeof(int*)*numberofplayers);
for(f=0;f<numberofplayers;f++)
{
    inn[f] = (int*)malloc(sizeof(int*)*(action[f]+1));
}
for(i=0;i<numberofplayers;i++)
{
    check_2 = 0;
    no=1;
    int co;

    minimax(i);
    for(c=0;c<numberofplayers;c++)
    {
        if(c==i)
        continue;
    }
    for(c=0;c<numberofplayers;c++)
    {
        if(c==i)
            continue;
        no=1;
        m=minimax_array[i].value[c][1];
        inn[c][no]=minimax_array[i].index[c][1];
        for(j=2;j<=action[c];j++)
        {
            if(m>=minimax_array[i].value[c][j])
            {
                if(m==minimax_array[i].value[c][j])
                {
                    check_2 = 1;
                    no++;
                    inn[c][no] = minimax_array[i].index[c][j];
                }
                else
                {
                    no = 1;
                    if(check_2==1)
                    {
                        for(co=1;co<=action[i];co++)
                        {
                            inn[c][co] = 0;
                        }
                    }
                    inn[c][no] = minimax_array[i].index[c][j];
                    m = minimax_array[i].value[c][j];
                }
            }
        }
        for(co=1;co<=no;co++)
        printf("player%d has a minmax strategy %d against player%d giving player%d the payoff of %d\n",i+1,inn[c][co],c+1,c+1,m);
    }
}

int mark1=1;
int mark2=1;
int mark3=1;

for(i=0;i<action[i];i++)
{
    if(strongdominanceequilibria[i]==0)
    {
        mark1 = 0;
    }

    if(weakdominanceequilibria[i]==0)
    {
        mark2 = 0;
    }
    if(veryweakdominanceequilibria[i]==0)
    {
        mark3 = 0;
    }
}

if(mark1 == 1)
{
    printf("There exists a strongly dominant strategy equilibrium\n\[");
    for(i=0;i<action[i];i++)
    {
        printf(" %d ",strongdominanceequilibria[i]);
    }
    printf("]\n");
}
    printf("\n");
if(mark2 == 1)
{
    printf("There exists a weakly dominant strategy equilibrium\n\[");
    for(i=0;i<action[i];i++)
    {
        printf(" %d ",weakdominanceequilibria[i]);
    }
    printf("]\n");
}
    printf("\n");
if(mark3 == 1)
{
    printf("There exists a very weakly dominant strategy equilibrium\n[");
    for(i=0;i<action[i];i++)
    {
        printf(" %d ",veryweakdominanceequilibria[i]);
    }
    printf("]\n");
}
    printf("\n");
nashequilibria();



return 0;
}
