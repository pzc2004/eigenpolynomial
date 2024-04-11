#include <bits/stdc++.h>
using namespace std;
#define N 11
bool vis[N];
int gcd(int a, int b) {
	return b == 0 ? a : gcd(b, a % b);
}
struct Frac {
	int p, q;
	Frac(int p= 0, int q= 1) : p(p), q(q) {}
	void Simplify() {
		int t= abs(gcd(p, q));
		if(t == 0)
			p= 1, q= 0;
		else
			p/= t, q/= t;
		if(q < 0) p= -p, q= -q;
	}
	Frac operator-() { return Frac(-p, q); }
	bool operator==(const Frac &b) {
		return p * b.q == b.p * q;
	}
	bool operator!=(const Frac &b) {
		return p * b.q != b.p * q;
	}
	Frac operator+(const Frac &b) {
		Frac s(p * b.q + b.p * q, q * b.q);
		s.Simplify();
		return s;
	}
	Frac operator-(const Frac &b) {
		Frac s(p * b.q - b.p * q, q * b.q);
		s.Simplify();
		return s;
	}
	Frac operator*(const Frac &b) {
		Frac s(p * b.p, q * b.q);
		s.Simplify();
		return s;
	}
	Frac operator/(const Frac &b) {
		if(b.p == 0) {
			printf("Div 0.\n");
			exit(1);
		}
		Frac s(p * b.q, q * b.p);
		s.Simplify();
		return s;
	}
	void print() {
		if(p == 0 || q == 1)
			printf("%d", p);
		else
			printf("%d/%d", p, q);
	}
};
struct Poly {
	int deg, maxlen;
	Frac p[N];
	Poly(Frac p0= 0, int maxlen= 0) : maxlen(maxlen) {
		for(int i= 1; i <= maxlen; i++) p[i]= 0;
		p[0]= p0, deg= 0;
	}
	void getDeg(int n= -1) {
		if(n == -1) n= maxlen;
		for(int i= n; i >= 0; i--) {
			if(p[i] != 0 || i == 0) {
				deg= i;
				return;
			}
		}
	}
	Poly operator-() {
		Poly b= *this;
		for(int i= 0; i <= deg; i++) b.p[i]= -b.p[i];
		return b;
	}
	Poly operator+(const Poly &b) {
		if(b.maxlen != maxlen) {
			printf("Different maxlen.");
			exit(1);
		}
		Poly c(0, maxlen);
		int len= max(deg, b.deg);
		for(int i= len; i >= 0; i--) c.p[i]= p[i] + b.p[i];
		c.getDeg(len);
		return c;
	}
	Poly operator-(const Poly &b) {
		if(b.maxlen != maxlen) {
			printf("Different maxlen.");
			exit(1);
		}
		Poly c(0, maxlen);
		int len= max(deg, b.deg);
		for(int i= len; i >= 0; i--) c.p[i]= p[i] - b.p[i];
		c.getDeg(len);
		return c;
	}
	Poly operator*(const Poly &b) {
		if(b.maxlen != maxlen) {
			printf("Different maxlen.");
			exit(1);
		}
		Poly c(0, maxlen);
		int len= deg + b.deg;
		if(len > maxlen) {
			printf("Out of range.\n");
			exit(1);
		}
		for(int i= 0; i <= deg; i++) {
			for(int j= 0; j <= b.deg; j++) {
				c.p[i + j]= c.p[i + j] + p[i] * b.p[j];
			}
		}
		c.getDeg(len);
		return c;
	}
	void print(char c= ' ') {
		for(int i= deg; i >= 0; i--) {
			if(i != deg && p[i].p == 0) continue;
			if(i != deg && p[i].p > 0) putchar('+');
			if(i) {
				if(p[i].p == p[i].q) {
					if(p[i].p < 0) putchar('-');
				} else
					p[i].print();
				printf("x");
				if(i != 1) printf("^%d", i);
			} else {
				p[i].print();
			}
		}
		putchar(c);
	}
};
struct Matrix {
	int n;
	Poly a[N][N];
	void gen() {
		int k, b;
		printf("Please input n (n must not bigger than 10)\n");
		scanf("%d", &n);
		printf("Please input k. abs(aij)<=k.\n");
		scanf("%d", &k);
		printf("Please input b. If b==0, aij will be nonnegative. Otherwise aij may be negative.\n");
		scanf("%d", &b);
		srand(time(0));
		for(int i= 1; i <= n; i++) {
			for(int j= 1; j <= n; j++) {
				a[i][j]= Poly(rand() % (k + 1), n);
				if(b) {
					int sgn= rand() % 2;
					if(sgn) a[i][j]= -a[i][j];
				}
			}
		}
	}
	void dfs(int i, bool j, Poly now, Poly &ans) {
		if(i == n + 1) {
			if(j)
				ans= ans - now;
			else
				ans= ans + now;
			return;
		}
		for(int x= 1; x <= n; x++) {
			if(!vis[x]) {
				vis[x]= 1;
				bool k= j;
				for(int y= x + 1; y <= n; y++)
					if(vis[y]) k= !k;
				dfs(i + 1, k, now * a[i][x], ans);
				vis[x]= 0;
			}
		}
	}
	Poly det() {
		Poly ans(0, n);
		dfs(1, 0, Poly(1, n), ans);
		return ans;
	}
	Poly eigenpolynomial() {
		Matrix b= *this;
		for(int i= 1; i <= n; i++) {
			for(int j= 1; j <= n; j++) {
				b.a[i][j]= -b.a[i][j];
				if(i == j) {
					b.a[i][j].p[1]= 1, b.a[i][j].deg= 1;
				}
			}
		}
		return b.det();
	}
	void print() {
		for(int i= 1; i <= n; i++) {
			for(int j= 1; j <= n; j++) {
				a[i][j].print();
			}
			putchar('\n');
		}
	}
};
int main() {
	Matrix a;
	a.gen();
	// a.n= 3;
	// a.a[1][1]= Poly(1, 3);
	// a.a[1][2]= Poly(0, 3);
	// a.a[1][3]= Poly(0, 3);
	// a.a[2][1]= Poly(0, 3);
	// a.a[2][2]= Poly(1, 3);
	// a.a[2][3]= Poly(0, 3);
	// a.a[3][1]= Poly(0, 3);
	// a.a[3][2]= Poly(0, 3);
	// a.a[3][3]= Poly(1, 3);
	a.print();
	a.det().print('\n');
	a.eigenpolynomial().print('\n');
	return 0;
}