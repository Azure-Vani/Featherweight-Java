class A extends Object {
    int a;

    A(int x) {
        super();
        this.a = x;
    }

    int f() {
        return this.a;
    }

    int g(int x) {
        return x;
    }
}

class B extends A {
    int a;

    B(int a) {
        super(a + 1);
        this.a = a - 1;
    }
}

(new B(0)).f()
