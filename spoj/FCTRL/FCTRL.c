#include <stdio.h>
 
int main(void) {
	int n,m,j;
	scanf("%d", &n);
	while (j=0,n--) {
		scanf("%d", &m);
		while (m>4)
			j+=(m/=5);
		printf("%d\n",j);
	}	
	return 0;
}
