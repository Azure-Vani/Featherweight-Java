class A extends Object {
    int a;

    A(int b) {
        super();
        this.a = b;
    }

    int f() {
        return this.a;
    }
}

(new A(1)).f()
